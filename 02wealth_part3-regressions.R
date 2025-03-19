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

# Define the thresholds based on the distribution of tot_inc_aeu_ppp_bm
low_threshold <- quantile(z$tot.inc, 0.25, na.rm = TRUE)
high_threshold <- quantile(z$tot.inc, 0.75, na.rm = TRUE)
median_income <- median(z$tot.inc, na.rm = TRUE)

# Create income categories based on these thresholds
z <- z %>%
  mutate(income_category = case_when(
    tot.inc <= low_threshold ~ "Low Income",
    tot.inc > low_threshold & tot.inc <= median_income ~ "Lower Middle Income",
    tot.inc > median_income & tot.inc <= high_threshold ~ "Middle Income",
    tot.inc > high_threshold ~ "very High Income"
  ))

# Fit the regression model with fixed effects for income_category
model_income_felm <- felm(ihs(bm.inc) ~ dry + wet + dry:factor(income_category) + wet:factor(income_category) | factor(ghousecode) + factor(intyear) | 0 | gvillcode, data = z)

# Summary of the model
summary(model_income_felm)

# Save the summary table in LaTeX and HTML format
stargazer(model_income_felm, type = "latex", title = "Income moderates the effects of shocks?",
          covariate.labels = c("Dry Shock", "Wet Shock", "Dry * Lower-Middle Income", "Dry * Upper-Middle Income", "Dry * High Income", 
                               "Wet * Lower-Middle Income", "Wet * Upper-Middle Income", "Wet * High Income"),
          dep.var.labels = "Bushmeat Income (IHS)",
          out = "06.15.2024_Results/Income/Income_regression_results.tex")

stargazer(model_income_felm, type = "html", title = "Income moderates the effects of shocks?",
          covariate.labels = c("Dry Shock", "Wet Shock", "Dry * Lower-Middle Income", "Dry * Upper-Middle Income", "Dry * High Income", 
                               "Wet * Lower-Middle Income", "Wet * Upper-Middle Income", "Wet * High Income"),
          dep.var.labels = "Bushmeat Income (IHS)",
          out = "06.15.2024_Results/Income/Income_regression_results.html")

# Extract the coefficients using broom
tidy_model_income <- tidy(model_income_felm)
# Extract the covariance matrix
cov_matrix <- vcov(model_income_felm)

# Compute marginal effects and their standard errors for dry shocks manually
dry_coeff <- tidy_model_income %>% filter(term == "dry") %>% select(estimate) %>% as.numeric()
dry_interaction_coeff_lower <- tidy_model_income %>% filter(term == "dry:factor(income_category)Lower Middle Income") %>% select(estimate) %>% as.numeric()
dry_interaction_coeff_upper <- tidy_model_income %>% filter(term == "dry:factor(income_category)Middle Income") %>% select(estimate) %>% as.numeric()
dry_interaction_coeff_high <- tidy_model_income %>% filter(term == "dry:factor(income_category)very High Income") %>% select(estimate) %>% as.numeric()

# Calculate marginal effects for each income category
marginal_effects_dry_low <- dry_coeff
marginal_effects_dry_lower <- dry_coeff + dry_interaction_coeff_lower
marginal_effects_dry_upper <- dry_coeff + dry_interaction_coeff_upper
marginal_effects_dry_high <- dry_coeff + dry_interaction_coeff_high

# Compute standard errors for marginal effects
se_dry_low <- sqrt(cov_matrix["dry", "dry"])
se_dry_lower <- sqrt(cov_matrix["dry", "dry"] + cov_matrix["dry:factor(income_category)Lower Middle Income", "dry:factor(income_category)Lower Middle Income"] + 
                       2 * cov_matrix["dry", "dry:factor(income_category)Lower Middle Income"])
se_dry_upper <- sqrt(cov_matrix["dry", "dry"] + cov_matrix["dry:factor(income_category)Middle Income", "dry:factor(income_category)Middle Income"] + 
                       2 * cov_matrix["dry", "dry:factor(income_category)Middle Income"])
se_dry_high <- sqrt(cov_matrix["dry", "dry"] + cov_matrix["dry:factor(income_category)very High Income", "dry:factor(income_category)very High Income"] + 
                      2 * cov_matrix["dry", "dry:factor(income_category)very High Income"])

# Compute marginal effects for wet shocks manually
wet_coeff <- tidy_model_income %>% filter(term == "wet") %>% select(estimate) %>% as.numeric()
wet_interaction_coeff_lower <- tidy_model_income %>% filter(term == "wet:factor(income_category)Lower Middle Income") %>% select(estimate) %>% as.numeric()
wet_interaction_coeff_upper <- tidy_model_income %>% filter(term == "wet:factor(income_category)Middle Income")  %>% select(estimate) %>% as.numeric()
wet_interaction_coeff_high <- tidy_model_income %>% filter(term == "wet:factor(income_category)very High Income") %>% select(estimate) %>% as.numeric()

# Calculate marginal effects for each income category
marginal_effects_wet_low <- wet_coeff
marginal_effects_wet_lower <- wet_coeff + wet_interaction_coeff_lower
marginal_effects_wet_upper <- wet_coeff + wet_interaction_coeff_upper
marginal_effects_wet_high <- wet_coeff + wet_interaction_coeff_high

# Compute standard errors for marginal effects
se_wet_low <- sqrt(cov_matrix["wet", "wet"])
se_wet_lower <- sqrt(cov_matrix["wet", "wet"] + cov_matrix["wet:factor(income_category)Lower Middle Income", "wet:factor(income_category)Lower Middle Income"] + 
                       2 * cov_matrix["wet", "wet:factor(income_category)Lower Middle Income"])
se_wet_upper <- sqrt(cov_matrix["wet", "wet"] + cov_matrix["wet:factor(income_category)Middle Income", "wet:factor(income_category)Middle Income"] + 
                       2 * cov_matrix["wet", "wet:factor(income_category)Middle Income"])
se_wet_high <- sqrt(cov_matrix["wet", "wet"] + cov_matrix["wet:factor(income_category)very High Income", "wet:factor(income_category)very High Income"] + 
                      2 * cov_matrix["wet", "wet:factor(income_category)very High Income"])

# Create data frames for plotting
marginal_effects_dry_df <- data.frame(
  IncomeCategory = c("Very Low Income", "Low Income", "Middle Income", "High Income"),
  MarginalEffect = c(marginal_effects_dry_low, marginal_effects_dry_lower, marginal_effects_dry_upper, marginal_effects_dry_high),
  SE = c(se_dry_low, se_dry_lower, se_dry_upper, se_dry_high)
)

marginal_effects_wet_df <- data.frame(
  IncomeCategory = c("Very Low Income", "Low Income", "Middle Income", "High Income"),
  MarginalEffect = c(marginal_effects_wet_low, marginal_effects_wet_lower, marginal_effects_wet_upper, marginal_effects_wet_high),
  SE = c(se_wet_low, se_wet_lower, se_wet_upper, se_wet_high)
)

# Add significance column based on confidence intervals
marginal_effects_dry_df <- marginal_effects_dry_df %>%
  mutate(Significant = ifelse(MarginalEffect - 1.96 * SE > 0 | MarginalEffect + 1.96 * SE < 0, "Significant", "Not Significant"))

marginal_effects_wet_df <- marginal_effects_wet_df %>%
  mutate(Significant = ifelse(MarginalEffect - 1.96 * SE > 0 | MarginalEffect + 1.96 * SE < 0, "Significant", "Not Significant"))

# Plot the marginal effects of dry shocks across income levels
plot_dry <- ggplot(marginal_effects_dry_df, aes(x = IncomeCategory, y = MarginalEffect)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_errorbar(aes(ymin = MarginalEffect - 1.96 * SE, ymax = MarginalEffect + 1.96 * SE), width = 0.2) +
  geom_text(aes(label = round(MarginalEffect, 3)), vjust = -0.5, color = "black") +
  labs(title = "Dry Shock",
       x = "Income Category",
       y = "Marginal Effect (Percentage Points)") +
  theme_minimal()

# Plot the marginal effects of wet shocks across income levels
plot_wet <- ggplot(marginal_effects_wet_df, aes(x = IncomeCategory, y = MarginalEffect)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_errorbar(aes(ymin = MarginalEffect - 1.96 * SE, ymax = MarginalEffect + 1.96 * SE), width = 0.2) +
  geom_text(aes(label = round(MarginalEffect, 3)), vjust = -0.5, color = "black") +
  labs(title = "Wet Shock",
       x = "Income Category",
       y = "Marginal Effect (Percentage Points)") +
  theme_minimal()

# Combine the plots side by side with a combined title
combined_plot <- grid.arrange(
  plot_dry, plot_wet, ncol = 2,
  top = textGrob("Marginal Effects of Shocks on Bushmeat Income by Income Level", gp = gpar(fontsize = 14, fontface = "bold"))
)

# Save the combined plot to a file
ggsave("06.15.2024_Results/Income/income_marginal_effects.png", combined_plot, width = 12, height = 6)