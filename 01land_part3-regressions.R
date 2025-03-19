#Secured ownership enables people to have various adaptation strategies developed on the particular land. Farmers who own land are more likely to invest in various adaptation options, including crop and livestock management practices and water conservation. Studies show that land ownership encourages the adoption of various technologies linked to land including irrigation, drainage, tree planting and crop diversification (Lutz et al. 1994; Shultz et al. 1997; Nhemachena and Hassan 2008).
#https://springerplus.springeropen.com/articles/10.1186/s40064-016-2484-y
#The relationship between illegal hunting and lack of employment opportunities, access to land
#and domestic sources of animal protein is well documented in Sub-Saharan Africa (Ndibalema &  Songorwa 2008; Lindsey & Bento 2012; Lindsey et al. 2013). Improved access to, and
#ownership of these assets as a measure of relative wealth is expected to discourage illicit
#utilisation of natural resources (Loibooki et al. 2002). Contrary, this study revealed that the
#majority of illegal hunters were employed and the ownership of land and livestock was widely
#distributed amongst them. https://researchspace.ukzn.ac.za/server/api/core/bitstreams/9a43baa8-0879-49b1-b3ba-1444d87b436d/content
library(ggplot2)
library(lfe)
library(stargazer)
library(dplyr)
library(tidyr)
library(texreg)
library(ggplot2)
library(extrafont)
library(extrafontdb)
#library(dotwhisker)
library(broom)
library(ggthemes)
library(haven)
library(gridExtra)
library(stargazer)
library(grid)
# font_import()
fonts()
par(family="Times New Roman")
ft="Times New Roman"
fs=22
# functions
std=function(x) (x-mean(x,na.rm=T))/sd(x,na.rm=T) #formula for standardization
ihs=function(x) log(x+(x^2+1)^0.5) #formula for IHS transformation 
####################################Load data sets###################################################################################################################################
#####################################################################################################################################################################################
globalwd <- "~/OneDrive - University of Georgia/Research/Bushmeat Project/"
setwd(globalwd)
z = read.csv("04.02.2024_cleaned_datasets/data-regression-part3.csv") # import data from regressions- part 3 education file
colnames(z)
z$tot_land[z$tot_land < 0] = 0
# Define specific cutoffs for tot_land
z <- z %>%
  mutate(land_category = case_when(
    tot_land <= 7 ~ "Landholding (Small)",
    tot_land > 7 ~ "Large Landholding"
  ))

table(z$land_category)


# Fit the regression model with fixed effects for land_category
model_land_felm <- felm(ihs(bm.inc) ~ dry + wet + dry:factor(land_category) + wet:factor(land_category) | factor(ghousecode) + factor(intyear) | 0 | gvillcode, data = z)
# Summary of the model
summary(model_land_felm)
#Dry Shock: The coefficient for dry shock is positive and statistically significant, indicating that, on average, dry shocks increase bushmeat income.
#Wet Shock: The coefficient for wet shock is negative but not statistically significant, suggesting that wet shocks do not have a significant effect on bushmeat income on average.
#the effect of dry shocks on bushmeat income does not significantly vary for large landholdings compared to small landholdings.
#large landholdings experience an increased effect of wet shocks on bushmeat income compared to small landholdings.
# Save the summary table in LaTeX format
stargazer(model_land_felm, type = "latex", title = "Landholding Size moderates the effects of shocks?",
          covariate.labels = c("Dry Shock", "Wet Shock", "Dry * Large Landholding", "Wet * Large Landholding"),
          dep.var.labels = "Bushmeat Income (IHS)",
          out = "06.15.2024_Results/Land/Land_regression_results.tex")

# Save the summary table in HTML format
stargazer(model_land_felm, type = "html", title = "Landholding Size moderates the effects of shocks?",
          covariate.labels = c("Dry Shock", "Wet Shock", "Dry * Large Landholding", "Wet * Large Landholding"),
          dep.var.labels = "Bushmeat Income (IHS)",
          out = "06.15.2024_Results/Land/Land_regression_results.html")

