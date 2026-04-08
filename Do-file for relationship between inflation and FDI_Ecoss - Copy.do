Relationship between FDI and Inflation

////changing the directory
cd "D:\Research\Does FDI have negative relationship with Inflation"
 
////import from excel file
import excel Final_data, sheet("Final_data") firstrow 

gen Log_Pop_gdp_interaction = Log_Pop * Log_gdp

///encode the country
encode Country, generate(c_Country)

///set the data as panel data
xtset year c_Country

///describe the panel dataset
xtdescribe


///installing outreg2 
ssc install outreg2

///summarize the variables
xtsum Log_gdp Log_FDI Log_Pop Log_inf

///exporting the results into latex
outreg2 using summary_stats_2.tex, replace ctitle("Summary Statistics") sum(log)

///correlation matrix
corr Log_gdp Log_FDI Log_Pop Log_inf

//correlation matrix with significance
//correlate Log_inf Log_FDI Log_gdp Log_Pop, sig

///adding lagged inflation in the model 
//xtsum Log_inf L.Log_inf Log_FDI Log_Pop Log_gdp 

///adding interaction varibale of Pop ad GDP
//xtsum Log_inf L.Log_inf Log_FDI Log_Pop Log_gdp Log_Pop_gdp_interaction

****Pooled OLS Models******
///regress OLS without control variables
regress Log_inf Log_FDI

//storing as model1
estimates store model1

///regress OLS with control variables
regress Log_inf Log_FDI Log_Pop Log_gdp 

///store as model2
estimates store model2

///adding lagged inflation in the model 
///regress Log_inf L.Log_inf Log_FDI Log_Pop Log_gdp

///adding interaction varibale of Pop ad GDP
///regress Log_inf L.Log_inf Log_FDI Log_Pop Log_gdp c.Log_Pop#c.Log_gdp
 
///Breusch-Pagan/Cook-Weisbergtest for heteroskedasticity without control variables
///hettest Log_FDI

///Breusch-Pagan/Cook-Weisbergtest for heteroskedasticity with control variables
///hettest Log_FDI Log_Pop Log_gdp

///Fixed Effect regression without control variables
xtreg Log_inf Log_FDI, fe

 ///store as model3
estimates store model3
 
///Fixed Effect regression with control variables
xtreg Log_inf Log_FDI Log_gdp Log_Pop, fe
 
 ////store as model4
estimates store model4

///storing the fixed effect regression
est store fe

///adding lagged inflation in the model 
//xtreg Log_inf L.Log_inf Log_FDI Log_Pop Log_gdp, fe

///adding interaction varibale of Pop ad GDP
//xtreg Log_inf L.Log_inf Log_FDI Log_Pop Log_gdp c.Log_Pop#c.Log_gdp, fe

///random effects regression without control variables
xtreg Log_inf Log_FDI, re
 
 ///storing as model5
estimares store model5

///random effects regression
xtreg Log_inf Log_FDI Log_gdp Log_Pop, re

///store as model6
estimates store model6

///storing the random effect regression
est store re

///adding lagged inflation in the model 
//xtreg Log_inf L.Log_inf Log_FDI Log_Pop Log_gdp, re

///adding interaction varibale of Pop ad GDP
//xtreg Log_inf L.Log_inf Log_FDI Log_Pop Log_gdp c.Log_Pop#c.Log_gdp, re

///Breusch and Pagan Lagrangian multiplier test for Poolability (Since Prob > chibar2 = 0.0000 and we conclude that the Pooled OLS model is not appropriate.)
xttest0

///choosing the fixed or random using hausman test
hausman fe re

///GEE Considerations Generalized Estimating Equations (GEE) are useful when you have non-normal data, such as binary or count outcomes, or when you want to specify a particular correlation structure for the data (e.g., exchangeable, autoregressive).
///When to Use GEE If you are interested in population-averaged (marginal) effects rather than individual-specific effects. If the outcome variable is non-normally distributed (e.g., binary or count data).
///Correlation Structure: You need to specify a working correlation structure for GEE models, which estimates the within-group correlation (e.g., exchangeable, independent, or ar1 for autoregressive).
///No Hausman Test for GEE: Since GEE is based on estimating equations rather than random or fixed effects, there is no equivalent Hausman test for GEE. Instead, you choose GEE based on the nature of your data (e.g., if you need marginal effects or specific correlation structures).

///When to Use GEE: If you are interested in population-averaged (marginal) effects rather than individual-specific effects. If the outcome variable is non-normally distributed (e.g., binary or count data).

///Correlation Structure: You need to specify a working correlation structure for GEE models, which estimates the within-group correlation (e.g., exchangeable, independent, or ar1 for autoregressive). No Hausman Test for GEE: Since GEE is based on estimating equations rather than random or fixed effects, there is no equivalent Hausman test for GEE. Instead, you choose GEE based on the nature of your data (e.g., if you need marginal effects or specific correlation structures).

///generalized estimating equation (GEE) regression without control variables
//xtgee Log_inf Log_FDI, family(gaussian) link(identity) corr(exchangeable)
 

///generalized estimating equation (GEE) regression with control variables
//xtgee Log_inf Log_FDI Log_gdp Log_Pop, family(gaussian) link(identity) corr(exchangeable)


///adding lagged inflation in the model 
//xtgee Log_inf L.Log_inf Log_FDI Log_Pop Log_gdp, family(gaussian) link(identity) corr(exchangeable)

///adding interaction varibale of Pop ad GDP
//xtgee Log_inf L.Log_inf Log_FDI Log_Pop Log_gdp c.Log_Pop#c.Log_gdp, family(gaussian) link(identity) corr(exchangeable)


///exporting the stored regression results
outreg2 [model1 model2 model3 model4 model5 model6] using MYFILE, word keep(Log_FDI Log_gdp Log_Pop)
