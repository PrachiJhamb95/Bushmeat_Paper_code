#Residence period-https://springerplus.springeropen.com/articles/10.1186/s40064-016-2484-y
#The residence period the head of household lived in the study area had significant positive beta coefficient (β) (P ≥ 0.05), suggesting that it influences adoption of developed adaptation strategies. This implied that, a unit change in resident period increases likelihood of increasing the number of adaptation strategies (Table 10). However, there were no significant differences (P ≥ 0.05) in residence period when more than three adaptation strategies were involved. It is speculated that more adaptation strategies were probably developed by people who lived in the study area for a long time because of increased experience, knowledge and gained information on adverse climate change effects. According to Ellis (2000) and Nhemachena and Hassan (2008) people living for a long period in a certain area are able to develop large number of adaptation strategies.
#how long the household has lived in the village can affect their adaptation strategies.
#Residence period (X4)—this is a continuous explanatory variable that was measured from number of years the respondent has lived in the study area. Duration a household head spent in the area for living was related to increased experience about the area. This included gained knowledge and information about agronomic practices and climate change.
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
#install.packages("gridExtra")
library(gridExtra)
#install.packages("stargazer")
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
#######################################################################################################
globalwd <- "~/OneDrive - University of Georgia/Research/Bushmeat Project/"
setwd(globalwd)
z = read.csv("04.02.2024_cleaned_datasets/data-regression-part3.csv") # import data from regressions- part 1 after creating dummy variables.
colnames(z)
#"hhc_formed"= How long ago was this household formed (see definition of household)
#"hhc_born"= Was the household head born in this village?
#"hhc_hdlong"= If ‘no’: how long has the household head lived in the village?
#"hhc_hdethnic" = Does the household head belong to the largest ethnic group/caste in the village?
z$hhc_hdlong[z$hhc_hdlong < 0] <- NA
z$hhc_born[z$hhc_born < 0] <- NA
summary(z$hhc_born)
summary(z$hhc_hdlong)
# Create residence period categories based on quartiles
z <- z %>%
  mutate(residence_period = case_when(
    hhc_hdlong <= quantile(hhc_hdlong, 0.25, na.rm = TRUE) ~ "aShort-Term Resident",
    hhc_hdlong > quantile(hhc_hdlong, 0.25, na.rm = TRUE) & hhc_hdlong <= quantile(hhc_hdlong, 0.75, na.rm = TRUE) ~ "bMedium-Term Resident",
    hhc_hdlong > quantile(hhc_hdlong, 0.75, na.rm = TRUE) ~ "cLong-Term Resident"
  ))

# Filter out rows with "Unknown" residence period
z <- z %>%
  filter(!is.na(residence_period))

table(z$residence_period)

# Fit the regression model with fixed effects for residence_period
model_residence_felm <- felm(ihs(bm.inc) ~ dry + wet + dry:factor(residence_period) + wet:factor(residence_period) | factor(ghousecode) + factor(intyear) | 0 | gvillcode, data = z)
# Summary of the model
summary(model_residence_felm)
# Save the summary table in LaTeX format
stargazer(model_residence_felm, type = "latex", title = "Residence Period moderates the effects of shocks?",
          covariate.labels = c("Dry Shock", "Wet Shock", "Dry * Short-Term Resident", "Dry * Medium-Term Resident", "Dry * Long-Term Resident", "Wet * Short-Term Resident", "Wet * Medium-Term Resident", "Wet * Long-Term Resident"),
          dep.var.labels = "Bushmeat Income (IHS)",
          out = "06.15.2024_Results/Residence/Residence_regression_results.tex")
# Save the summary table in HTML format
stargazer(model_residence_felm, type = "html", title = "Residence Period moderates the effects of shocks?",
          covariate.labels = c("Dry Shock", "Wet Shock", "Dry * Short-Term Resident", "Dry * Medium-Term Resident", "Dry * Long-Term Resident", "Wet * Short-Term Resident", "Wet * Medium-Term Resident", "Wet * Long-Term Resident"),
          dep.var.labels = "Bushmeat Income (IHS)",
          out = "06.15.2024_Results/Residence/Residence_regression_results.html")

# Extract the coefficients using broom
tidy_model_residence <- tidy(model_residence_felm)
# Extract the covariance matrix
cov_matrix <- vcov(model_residence_felm)
