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
library(readxl)
library(plm)
library(stargazer)
library(flextable)
library(modelsummary)
library(pandoc)
library(officer)

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
  indicator = "NY.GDP.MKTP.CD", # GDP (current LCU)
  start     = 1970,
  end       = 2021
)

## 4. population
population_dat <- WDI(
  country   = "all",          
  indicator = "SP.POP.TOTL", # Population, total
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
  rename(gdp = NY.GDP.MKTP.CD)

popn_renamed <- population_dat_use_only_var |> 
  rename(popn = SP.POP.TOTL)


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

## We only want to keep those countries which were selected in our former analysis,
## so we have to filter out those countries. For that we need the list of the 
## countries that were in the former analysis. 

file_path <- "D:/Research/Does FDI have negative relationship with Inflation/replication_package/replication/"
former_analysis_dat <- read_excel(paste0(file_path, "Final_data.xlsx"), sheet = "Final_data")

selected_country_year <- former_analysis_dat |> 
  select(Country, year)

## Now that we have the year and country that was used in the former data analysis,
## we can now use the data for those countries and period by joining the two dataset.

data_analysis_new <- selected_country_year |> 
  left_join(non_negative_cpi, by = c("Country" = "country", "year"))

## Now that we have exactly the same countries and year as our former analysis,
## we now proceed to log_transform our variables

final_dat <- data_analysis_new |> 
  mutate(ln_cpi  = log(inf),
         ln_fdi  = log(fdi),
         ln_gdp  = log(gdp),
         ln_popn = log(popn)) |> 
  select(-c("inf", "fdi", "gdp", "popn")) |> 
  select("Country", "year", "ln_gdp", "ln_fdi", "ln_popn", "ln_cpi")


## Now that we have analysis ready data, we now prepare for the components of the
## data analysis

## First, we set our data as panel data

# Encode country as factor 
final_dat$c_Country <- as.numeric(factor(final_dat$Country))

# Set data as panel data 
panel_dat <- pdata.frame(final_dat, index = c("year", "c_Country"))

# Describe the panel dataset (equivalent to xtdescribe)
pdim(panel_dat)

## 1. Summary Statistics

# Function to compute overall, between, within stats
xtsum_table <- function(var_name, var, id) {
  overall_mean <- mean(var, na.rm = TRUE)
  between_vals <- tapply(var, id, mean, na.rm = TRUE)
  within_vals  <- var - tapply(var, id, mean, na.rm = TRUE)[id] + overall_mean
  
  data.frame(
    Variable     = c(paste(var_name, "overall"), "between", "within"),
    Mean         = c(round(overall_mean, 3),          NA,    NA),
    Std.dev      = c(round(sd(var, na.rm = TRUE), 3),
                     round(sd(between_vals, na.rm = TRUE), 3),
                     round(sd(within_vals,  na.rm = TRUE), 3)),
    Min          = c(round(min(var, na.rm = TRUE), 3),
                     round(min(between_vals, na.rm = TRUE), 3),
                     round(min(within_vals,  na.rm = TRUE), 3)),
    Max          = c(round(max(var, na.rm = TRUE), 3),
                     round(max(between_vals, na.rm = TRUE), 3),
                     round(max(within_vals,  na.rm = TRUE), 3)),
    Observations = c(paste("N =", sum(!is.na(var))),
                     paste("n =", length(unique(id))),
                     paste("T-bar =", round(sum(!is.na(var)) / length(unique(id)), 2)))
  )
}


# Apply to all variables
summary_table <- bind_rows(
  xtsum_table("INF",  panel_dat$ln_cpi, index(panel_dat)$c_Country),
  xtsum_table("FDI",  panel_dat$ln_fdi, index(panel_dat)$c_Country),
  xtsum_table("GDP",  panel_dat$ln_gdp, index(panel_dat)$c_Country),
  xtsum_table("POP",  panel_dat$ln_popn, index(panel_dat)$c_Country)
)


# Create flextable and export to Word
ft <- flextable(summary_table) |>
  set_header_labels(
    Variable = "Variable", Std.dev = "Std. dev.",
    Min = "Min", Max = "Max", Observations = "Observations"
  ) |>
  bold(part = "header") |>
  align(j = 2:5, align = "center", part = "all") |>
  set_caption("Table 1: Summary Statistics of the variables") |>
  autofit()

# Save to Word
# save_as_docx(ft, path = "summary_stats.docx")

## 2. Correlation Matrix

# Compute correlation matrix with significance stars
cor_with_stars <- function(data) {
  vars <- colnames(data)
  n    <- length(vars)
  mat  <- matrix("", nrow = n, ncol = n, dimnames = list(vars, vars))
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        mat[i, j] <- "1.000"
      } else if (i > j) {
        test <- cor.test(data[[i]], data[[j]], use = "complete.obs")
        r    <- round(test$estimate, 3)
        p    <- test$p.value
        stars <- ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
        mat[i, j] <- paste0(formatC(r, format = "f", digits = 3), stars)
      }
      # upper triangle stays empty
    }
  }
  as.data.frame(mat)
}

# Select variables in same order as paper
cor_data <- as.data.frame(panel_dat[, c("ln_gdp", "ln_fdi", "ln_popn", "ln_cpi")])

# Rename for display
colnames(cor_data) <- c("GDP", "FDI", "POP", "INF")

# Compute
cor_table <- cor_with_stars(cor_data)

# Add row names as first column
cor_table <- cbind(Variable = rownames(cor_table), cor_table)

# Build flextable
ft <- flextable(cor_table) |>
  set_header_labels(Variable = "") |>
  bold(j = 1, part = "body") |>
  bold(part = "header") |>
  align(j = 2:5, align = "center", part = "all") |>
  align(j = 1, align = "left",   part = "all") |>
  hline_top(part = "header",   border = officer::fp_border(width = 2)) |>
  hline_bottom(part = "header", border = officer::fp_border(width = 1)) |>
  hline_bottom(part = "body",   border = officer::fp_border(width = 2)) |>
  set_caption("Table 2: Correlation matrix of the variables") |>
  autofit()

# Export to Word
# save_as_docx(ft, path = "correlation_matrix.docx")

## Now let's move on to regression

# Pooled OLS
ols1 <- plm(ln_cpi ~ ln_fdi,                        data = panel_dat, model = "pooling")
ols2 <- plm(ln_cpi ~ ln_fdi + ln_gdp + ln_popn,    data = panel_dat, model = "pooling")

# Fixed Effect
fe1  <- plm(ln_cpi ~ ln_fdi,                        data = panel_dat, model = "within")
fe2  <- plm(ln_cpi ~ ln_fdi + ln_gdp + ln_popn,    data = panel_dat, model = "within")

# Random Effect
re1  <- plm(ln_cpi ~ ln_fdi,                        data = panel_dat, model = "random")
re2  <- plm(ln_cpi ~ ln_fdi + ln_gdp + ln_popn,    data = panel_dat, model = "random")

# ── Build regression table ────────────────────────────────────────────────────

models <- list(
  "(1)" = ols1, "(2)" = ols2,
  "(3)" = fe1,  "(4)" = fe2,
  "(5)" = re1,  "(6)" = re2
)

# Custom row/coefficient names to match paper
coef_map <- c(
  "ln_fdi" = "FDI",
  "ln_gdp" = "GDP",
  "ln_popn" = "Popn",
  "(Intercept)" = "Constant"
)

# Goodness-of-fit rows to display
gof_map <- data.frame(
  raw      = c("nobs", "r.squared", "ngroups"),
  clean    = c("Observations", "R-squared", "Number of year"),
  fmt      = c(0, 3, 0)
)

# Get the table as flextable for manual formatting
reg_table <- modelsummary(
  models,
  coef_map = coef_map,
  gof_map  = gof_map,
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  output   = "flextable"
) |>
  add_header_row(
    values  = c("Models &\nVARIABLES", "Pooled OLS Model", "Fixed Effect Model", "Random Effect Model"),
    colwidths = c(1, 2, 2, 2)
  ) |>
  bold(part = "header") |>
  hline_top(part = "header",   border = officer::fp_border(width = 2)) |>
  hline_bottom(part = "header", border = officer::fp_border(width = 1)) |>
  hline_bottom(part = "body",   border = officer::fp_border(width = 2)) |>
  autofit()

# save_as_docx(reg_table, path = "regression_table.docx")


## save in word

# save_as_docx(reg_table, path = "regression_table.docx")

## Diagnostic test

# ── Appendix B: Breusch-Pagan LM Test for Random Effects ─────────────────────
# Uses the pooling model to test whether random effects are needed
bp_test <- plmtest(ols2, type = "bp")  # ols2 is your pooled OLS with all vars
print(bp_test)

# Build the variance table (Var and SD columns)
bp_var_table <- data.frame(
  Var = c("Inf", "e", "u"),
  `SD = sqrt(Var)` = c(
    sqrt(bp_test$statistic),   # placeholder — extract manually if needed
    "",
    ""
  )
)

# ── Appendix C: Hausman Test ──────────────────────────────────────────────────
# Compares Fixed Effect vs Random Effect
hausman_test <- phtest(fe2, re2)
print(hausman_test)

# Build Hausman result table
hausman_table <- data.frame(
  ` `      = c("Chi-square test value", "P-value"),
  `Coef.`  = c(round(hausman_test$statistic, 3),
               round(hausman_test$p.value,   3))
)

# ── Export both to Word ───────────────────────────────────────────────────────

# Hausman flextable
ft_hausman <- flextable(hausman_table) |>
  set_header_labels(X.. = "", Coef.. = "Coef.") |>
  bold(part = "header") |>
  align(j = 2, align = "right", part = "all") |>
  hline_top(part = "header", border = fp_border(width = 1.5)) |>
  hline_bottom(part = "header", border = fp_border(width = 1)) |>
  hline_bottom(part = "body",   border = fp_border(width = 1.5)) |>
  set_caption("Hausman (1978) specification test") |>
  autofit()

# BP LM test results as text + table in Word
doc <- read_docx() |>
  # Appendix B
  body_add_par("Appendix B", style = "heading 2") |>
  body_add_par("Breusch and Pagan Lagrangian multiplier test for random effects",
               style = "Normal") |>
  body_add_par(paste0("chibar2(01) = ", round(bp_test$statistic, 2)), style = "Normal") |>
  body_add_par(paste0("Prob > chibar2 = ", round(bp_test$p.value, 4)), style = "Normal") |>
  
  # Appendix C
  body_add_par("Appendix C", style = "heading 2") |>
  body_add_flextable(ft_hausman)

print(doc, target = "appendix_BC.docx")

