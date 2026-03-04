## clear plots
if(!is.null(dev.list()))dev.off()

## clean workspace
rm(list=ls())

## set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## load package, if the package hasn't been loaded, install the package
# install.packages("WDI")
library(WDI)
library(dplyr)
library(tidyr)

## search for indicators using keywords
gdp <- WDIsearch("gdp")
inflation <- WDIsearch("inflation")
fdi <- WDIsearch("foreign direct investment")
population <- WDIsearch("labor force")

# Load WDI country metadata (comes built-in with the WDI package)
country_meta <- WDI_data$country

# Keep only rows where region is NOT "Aggregates"
countries_only <- country_meta$iso2c[country_meta$region != "Aggregates"]

## select data for concerned variables
## 1. inflation

inflation_dat <- WDI(
  country   = "all",          
  indicator = "FP.CPI.TOTL.ZG", # GDP (current LCU)
  start     = 1970,
  end       = 2021
)

## 2. foreign direct investment
fdi_dat <- WDI(
  country   = "all",          
  indicator = "BX.KLT.DINV.CD.WD", # Foreign direct investment, net inflows (BoP, current US$)
  start     = 1970,
  end       = 2021
)


## 3. gdp
gdp_dat <- WDI(
  country   = "all",          
  indicator = "NY.GDP.MKTP.CN", # GDP (current LCU)
  start     = 1970,
  end       = 2021
)

## 4. population
population_dat <- WDI(
  country   = "all",          
  indicator = "SL.TLF.TOTL.IN", # Labor force, total
  start     = 1970,
  end       = 2021
)

# Store all datasets in a named list
dataset_list <- list(
  inflation   = inflation_dat,
  fdi         = fdi_dat,
  gdp         = gdp_dat,
  population  = population_dat
)

## removing unnecessary rows and keeping only necessary rows. Particularly, we 
## will keep only countries data and not grouped data like Asia, Europe, etc.
filtered_list <- lapply(dataset_list, function(dat) {
  dat[dat$iso2c %in% countries_only, ]
})

# Access individual filtered datasets by name
inflation_dat_filtered  <- filtered_list$inflation
fdi_dat_filtered        <- filtered_list$fdi
gdp_dat_filtered        <- filtered_list$gdp
population_dat_filtered <- filtered_list$population

# Making a list of the dataset
filtered_dataset_list <- list(
  inflation   = inflation_dat_filtered,
  fdi         = fdi_dat_filtered,
  gdp         = gdp_dat_filtered,
  population  = population_dat_filtered
)

## keeping only the necessary columns in all datasets
use_only_var_list <- lapply(filtered_dataset_list, function(dat) {
  dat |> select(-c(iso2c, iso3c))
})

# Unpack the dataset
inflation_dat_use_only_var  <- use_only_var_list$inflation
fdi_dat_use_only_var        <- use_only_var_list$fdi
gdp_dat_use_only_var        <- use_only_var_list$gdp
population_dat_use_only_var <- use_only_var_list$population


##renaming the variable
inflation_renamed <- inflation_dat_use_only_var |> 
  rename(inf = FP.CPI.TOTL.ZG)

fdi_renamed <- fdi_dat_use_only_var |> 
  rename(fdi = BX.KLT.DINV.CD.WD)

gdp_renamed <- gdp_dat_use_only_var |> 
  rename(gdp = NY.GDP.MKTP.CN)

popn_renamed <- population_dat_use_only_var |> 
  rename(popn = SL.TLF.TOTL.IN)


## We see NA's in the datasets which are missing values, so we need to remove the
## NA's from all datasets to be able to have a regression-ready dataset
## First of all, let's join the dataset

compiled_dat <- inflation_renamed |> 
  left_join(fdi_renamed,  by = c("country", "year")) |> 
  left_join(gdp_renamed,  by = c("country", "year")) |> 
  left_join(popn_renamed, by = c("country", "year"))

## Now, let's remove the NA's.
na_removed_dat <- compiled_dat |> 
  na.omit()

## We can't have negative FDI for inflows because that would mean that it's an
## outflow and not inflow, so we filter out the negative FDI inflows. Also, the 
## log-transformation, which we'll do later, doesn't accept the negative values,
## so we remove the negative values

non_negative_fdi <- na_removed_dat |> 
  filter(fdi > 0)

## Our log-transformation in the regression requires positive values,
## so negative CPI observations are excluded.

non_negative_cpi <- non_negative_fdi |> 
  filter(inf > 0)

## We do not have Afghanistan and Albania in our former analysis, 

