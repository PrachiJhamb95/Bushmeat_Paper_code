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
####################################Load data sets###################################################################################################################################
#####################################################################################################################################################################################

globalwd <- "~/OneDrive - University of Georgia/Research/Bushmeat Project/"
setwd(globalwd)
x = read.csv("04.02.2024_cleaned_datasets/data-regression-part1.csv") # import data from regressions- part 1 after creating dummy variables.
colnames(x)
##Removed these in the updated version of old files already. x <- x[ ,-c(74:89)] #deleting all climate columns and merging them again. Coz the units of precipitation and temperature didn't seem right. Perhaps something was wrong in the merging earlier. 
CLQ = read.csv("Replication-Noack2019JAERE-Biodiversity/pen-climate_quarterly.csv",sep=",")
x <- merge(x,CLQ, all.x = TRUE) #merge climate quarterly columns with data
colnames(x)
geo = read.csv("Replication-Noack2019JAERE-Biodiversity/geodata.csv")
colnames(geo)
# this is because NA's are coded as negative values
#geo[geo<0]=NA
x <- merge(x,geo, all.x = TRUE) #merge geo columns with data
colnames(x)
###For income as a continuous independent variable - Create Income excluding bushmeat income.
x <- x %>% mutate(tot_inc_aeu_ppp_bm = tot.inc - bm.inc) 

INQ_annual = read_dta("04.02.2024_raw data/nielsen data/Global PEN downloaded 13.06.2013_/Income/annual_income_local.dta") ## annual PEN data from Nielsen
colnames(INQ_annual)
selected_socioecon <- INQ_annual %>% dplyr::select(4,47:61) #hhsize,female,age,educ,savings,land,fugmem #16variables#annual#don't vary for household. 
#"fug_h_mem"  HH membership to forest user group
z <- merge(x,selected_socioecon, by = c("ghousecode"),all.x = TRUE)

GEO = read_dta("04.02.2024_raw data/nielsen data/Global PEN downloaded 13.06.2013_/Data/supp_data/SAB_Spatial_data.dta")
colnames(GEO)
z <- merge(z,GEO, by = c("gvillcode"),all.x = TRUE)
colnames(z)

infra_village = read_dta("04.02.2024_raw data/nielsen data/Global PEN downloaded 13.06.2013_/Data/original/global_stata/v1_c_inf.dta") ## village infrastructure from original PEN data
colnames(infra_village)
#"inf_fcred" #How many households (approx.) have access to formal credit (government or private bank operating in the village)?
#"inf_icred" #Are informal credit institutions such as savings clubs and money lenders present in the village?
# "inf_kmdistr" = distance in kilometer to the nearest district market
#"inf_kmriv"= distance in kms to the nearest accessible river
# "inf_rdcar" = Does the village have at least one road usable by cars during all seasons?
# Convert character column to numeric
infra_village$gvillcode <- as.numeric(infra_village$gvillcode)
selected_infra <- infra_village %>% dplyr::select(2,5,6,8,10,11) #distance 
selected_infra[selected_infra < 0] = NA # this is because NA's are coded as negative values
# Perform left join
z <- left_join(z, selected_infra, by = "gvillcode")

#Welfare perceptions and social capital
welfare_hh = read_dta("04.02.2024_raw data/nielsen data/Global PEN downloaded 13.06.2013_/Data/original/global_stata/hhda2_e_wpsc.dta") ## household social capital from original annual A2 PEN data
colnames(welfare_hh)
welfare_hh[welfare_hh < 0] = NA # this is because NA's are coded as negative values
# "wpsc_satis" = All things considered, how satisfied are you with your life over the past 12 months?
# "wpsc_well" = Compared with other households in the village (or community), how well-off is your household? (Perception of Household Well-being Compared to Others in the Village)
# "wpsc_trust" = Do you in general trust people in the village (community)? (Trust and Help Within the Village Community)
# "wpsc_help" = Can you get help from other people in the village (community) if you are in need, for example, if you need extra money because someone in your family is sick?

# Convert character column to numeric
welfare_hh$ghousecode <- as.numeric(welfare_hh$ghousecode)
selected_welfare <- welfare_hh %>% dplyr::select(2,4,6,12,13) #welfare perceptions
# Perform left join
z <- left_join(z, selected_welfare, by = "ghousecode")
 
#aes_pen- number of adult equivalent units in household//
#Remittances is one part of the other total income sources that were received...I need to first subset that..It is coded =1 in the raw data file. 
other_income_source = read_dta("04.02.2024_raw data/nielsen data/Global PEN downloaded 13.06.2013_/Data/original/global_stata/qtr_j_oth.dta") ## household social capital from original annual A2 PEN data
colnames(other_income_source)
other_income_source <- other_income_source[, c(2,4,5,6)]
other_income_source$ghousecode <- as.integer(other_income_source$ghousecode)
other_income_source <- other_income_source %>% filter(oth_type == 1)
# Rename the income column
other_income_source <- other_income_source %>% rename(remittances = oth_totinc)

# Perform left join
z <- left_join(z, other_income_source , by = c("ghousecode","qtr"))
z$remit_aeu <- z$remittances / z$aes_pen
z$remittance_inc <- z$remit_aeu / z$ppp #This is the final remittance income.
z$remit_aeu <- NULL
z$oth_type <- NULL
z$remittances <- NULL
z$remittance_inc[is.na(z$remittance_inc)] <- 0 #replace NAs with zeros in remittance income
     
colnames(z)
#Now this is the final data set

adaptation_hh = read_dta("04.02.2024_raw data/nielsen data/Global PEN downloaded 13.06.2013_/Data/original/global_stata/hhda1_b.dta") ## from original PEN data
colnames(adaptation_hh)
adaptation_hh <- adaptation_hh[, c(2,5:8)]

adaptation_hh$ghousecode <- as.integer(adaptation_hh$ghousecode)

z <- left_join(z, adaptation_hh , by = "ghousecode")
###Directly Load this dataset now into all versions of part-3 regressions
write.csv(z, "04.02.2024_cleaned_datasets/data-regression-part3.csv")

#####################################################################################################################################################################################
#####################################################################################################################################################################################
#Explore variations in the effect of shock on Bush-meat income across different levels of household head education 
#Hypothesis: Education level moderates the effect of the shock on bushmeat income. 
#The regression model explores if the impact of drought and wet shocks on bushmeat income is moderated by the education level of the household head. 
# Handle missing values by replacing NA in head_educ with 0
z <- z %>%
  mutate(head_educ = ifelse(is.na(head_educ), 0, head_educ))


# Define simplified education categories from level of education for household heads to divide the hh head educ into low education and high education 
z <- z %>%
  mutate(education_category = case_when(
    head_educ < 5 ~ "Below Primary",
    head_educ >= 5 ~ "More than Primary Educ"
  ))
table(z$education_category)


# Fit the regression model with fixed effects
model_educ_felm <- felm(ihs(bm.inc) ~ dry + wet + dry:factor(education_category) + wet:factor(education_category) | factor(ghousecode) + factor(intyear) | 0 | gvillcode, data = z)
# Summary of the model
summary(model_educ_felm)
# Save the summary table in LaTeX format
stargazer(model_educ_felm, type = "latex", title = "Education moderates the effects of shocks?",
          covariate.labels = c("Dry Shock", "Wet Shock", "Dry * More than Primary Education", "Wet * More than Primary Education"),
          dep.var.labels = "Bushmeat Income (IHS)",
          out = "06.15.2024_Results/Education/Educ_regression_results.tex")
# Save the summary table in HTML format
stargazer(model_educ_felm, type = "html", title = "Education moderates the effects of shocks?",
          covariate.labels = c("Dry Shock", "Wet Shock", "Dry * More than Primary Education", "Wet * More than Primary Education"),
          dep.var.labels = "Bushmeat Income (IHS)",
          out = "06.15.2024_Results/Education/Educ_regression_results.html")


# Extract the coefficients using broom
tidy_model_educ <- tidy(model_educ_felm)
# Extract the covariance matrix
cov_matrix <- vcov(model_educ_felm)
# Compute marginal effects and their standard errors for dry shocks manually
dry_coeff <- tidy_model_educ %>% filter(term == "dry") %>% select(estimate) %>% as.numeric()
dry_interaction_coeff <- tidy_model_educ %>% filter(term == "dry:factor(education_category)More than Primary Educ") %>% select(estimate) %>% as.numeric()

# Calculate marginal effects for each education category
marginal_effects_dry_below_primary <- dry_coeff
marginal_effects_dry_more_than_primary <- dry_coeff + dry_interaction_coeff

# Compute standard errors for marginal effects
se_dry_below_primary <- sqrt(cov_matrix["dry", "dry"])
se_dry_more_than_primary <- sqrt(cov_matrix["dry", "dry"] + cov_matrix["dry:factor(education_category)More than Primary Educ", "dry:factor(education_category)More than Primary Educ"] + 
                                   2 * cov_matrix["dry", "dry:factor(education_category)More than Primary Educ"])

# Compute marginal effects for wet shocks manually
wet_coeff <- tidy_model_educ %>% filter(term == "wet") %>% select(estimate) %>% as.numeric()
wet_interaction_coeff <- tidy_model_educ %>% filter(term == "wet:factor(education_category)More than Primary Educ") %>% select(estimate) %>% as.numeric()

# Calculate marginal effects for each education category
marginal_effects_wet_below_primary <- wet_coeff
marginal_effects_wet_more_than_primary <- wet_coeff + wet_interaction_coeff

# Compute standard errors for marginal effects
se_wet_below_primary <- sqrt(cov_matrix["wet", "wet"])
se_wet_more_than_primary <- sqrt(cov_matrix["wet", "wet"] + cov_matrix["wet:factor(education_category)More than Primary Educ", "wet:factor(education_category)More than Primary Educ"] + 
                                   2 * cov_matrix["wet", "wet:factor(education_category)More than Primary Educ"])

# Create data frames for plotting
marginal_effects_dry_df <- data.frame(
  Education = c("Below Primary", "More than Primary"),
  MarginalEffect = c(marginal_effects_dry_below_primary, marginal_effects_dry_more_than_primary),
  SE = c(se_dry_below_primary, se_dry_more_than_primary)
)

marginal_effects_wet_df <- data.frame(
  Education = c("Below Primary", "More than Primary"),
  MarginalEffect = c(marginal_effects_wet_below_primary, marginal_effects_wet_more_than_primary),
  SE = c(se_wet_below_primary, se_wet_more_than_primary)
)

# Add significance column based on confidence intervals
marginal_effects_dry_df <- marginal_effects_dry_df %>%
  mutate(Significant = ifelse(MarginalEffect - 1.96 * SE > 0 | MarginalEffect + 1.96 * SE < 0, "Significant", "Not Significant"))

marginal_effects_wet_df <- marginal_effects_wet_df %>%
  mutate(Significant = ifelse(MarginalEffect - 1.96 * SE > 0 | MarginalEffect + 1.96 * SE < 0, "Significant", "Not Significant"))


# Plot the marginal effects of dry shocks across education levels
plot_dry <- ggplot(marginal_effects_dry_df, aes(x = Education, y = MarginalEffect)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_errorbar(aes(ymin = MarginalEffect - 1.96 * SE, ymax = MarginalEffect + 1.96 * SE), width = 0.2) +
  geom_text(aes(label = round(MarginalEffect, 3)), vjust = -0.5, color = "black") +
  labs(title = "Dry Shock",
       x = "Education Level",
       y = "Marginal Effect (Percentage Points)") +
  theme_minimal()

# Plot the marginal effects of wet shocks across education levels
plot_wet <- ggplot(marginal_effects_wet_df, aes(x = Education, y = MarginalEffect)) +
  geom_bar(stat = "identity",fill = "blue") +
  geom_errorbar(aes(ymin = MarginalEffect - 1.96 * SE, ymax = MarginalEffect + 1.96 * SE), width = 0.2) +
  geom_text(aes(label = round(MarginalEffect, 3)), vjust = -0.5, color = "black") +
  labs(title = "Wet Shock",
       x = "Education Level",
       y = "Marginal Effect (Percentage Points)") +
  theme_minimal()

# Combine the plots side by side with a combined title
library(grid)
combined_plot <- grid.arrange(
  plot_dry, plot_wet, ncol = 2,
  top = textGrob("Marginal Effects of Shocks on Bushmeat Income by Education Level", gp = gpar(fontsize = 14, fontface = "bold"))
)

# Save the combined plot to a file
ggsave("06.15.2024_Results/Education/educ_marginal_effects.png", combined_plot, width = 12, height = 6)
#Significance of Marginal Effects: The significance of the marginal effects can be inferred from the confidence intervals. If the confidence interval does not include zero, the marginal effect is statistically significant.
#####################################################################################################################################################################################

