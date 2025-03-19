########################################## loading packages ###################################################
library(ggplot2)
library(lfe)
library(stargazer)
library(dplyr)
library(tidyr)
library(texreg)
library(ggplot2)
library(extrafont)
library(extrafontdb)
library(dotwhisker)
library(broom)
library(ggthemes)
library(fixest)

# font_import()
fonts()
par(family="Times New Roman")

# functions
std=function(x) (x-mean(x,na.rm=T))/sd(x,na.rm=T) #formula for standardization
ihs=function(x) log(x+(x^2+1)^0.5) #formula for IHS transformation 

################################# data ######################

globalwd <- "/Users/prachijhamb/Library/CloudStorage/OneDrive-UniversityofGeorgia/Research/Bushmeat Project/"
setwd(globalwd)
x=read.csv("04.02.2024_cleaned_datasets/data-regression.csv")
##### Create Hunting Dependent Variables
x$trade <- ifelse(x$bm.inc !=0, x$tot_bm_cash_net_aeu_ppp/x$bm.inc,0) #cash share of bushmeat income
x$prevalence <- x$percentage_hunting  #prevalence of hunting =  percentage of households engaged in hunting activities # share of households in a village that are hunting

x$reliance <- ifelse(x$bm.inc == 0 & (x$tot.inc <= 0 | x$tot.inc == 0), 0,
                     ifelse(x$bm.inc > 0 & (x$tot.inc <= 0 | x$tot.inc < x$bm.inc), 1,
                            ifelse(x$tot.inc != 0, x$bm.inc / x$tot.inc * 1, 1)))
#x$reliance <- ifelse(x$bm.inc == 0 & (x$tot.inc <= 0 | x$tot.inc == 0), 0,
                     #ifelse(x$bm.inc > 0 & (x$tot.inc <= 0 | x$tot.inc < x$bm.inc), 100,
                            #(x$bm.inc / x$tot.inc) * 100))


#When total income is negative, zero or is less than bushmeat income, assume reliance to be 1.
#But if bushmeat income is zero and total income is negative, then reliance is 0.
#If both total income and bushmeat income are zero, then reliance is zero. If 
sd(x$CoKrig)/mean(x$CoKrig)*100

prevalence_by_country <- aggregate(x$prevalence, by=list(Country=x$country), FUN=function(x) mean(x, na.rm=TRUE))
reliance_by_country <- aggregate(x$reliance, by=list(Country=x$country), FUN=function(x) mean(x, na.rm=TRUE))
# Convert reliance to percentage
reliance_by_country$perc <- round(reliance_by_country$x * 100 ,2)
#subset_india <- x %>% filter(x$country == "India" & x$reliance ==1)
############################### define variables #########################################
n=10
z = x %>%
  mutate(dry = ifelse(spei <= -1, 1, 0)) %>% # drought dummy
  mutate(dry_lag3 = ifelse(spei_lag3 <= -1, 1, 0))%>% 
  mutate(dry_lag6 = ifelse(spei_lag6 <= -1, 1, 0))%>%
  mutate(wet = ifelse(spei >= 1, 1, 0)) %>% # flood dummy
  mutate(wet_lag3 = ifelse(spei_lag3 >= 1, 1, 0)) %>%
  mutate(wet_lag6 = ifelse(spei_lag6 >= 1, 1, 0)) %>%
  mutate(drought = ifelse(spei < 0, abs(spei),0)) %>% #continuous drought variable
  mutate(flood = ifelse(spei >0, spei, 0)) %>% #continuous flood variable
  mutate(moderate_wet = ifelse(spei >= 1 & spei < 1.5, 1, 0)) %>%
  mutate(moderate_wet_lag3 = ifelse(spei_lag3 >= 1 & spei_lag3 < 1.5, 1, 0)) %>%
  mutate(severe_wet = ifelse(spei >= 1.5 & spei < 2, 1, 0)) %>%
  mutate(severe_wet_lag3 = ifelse(spei_lag3 >= 1.5 & spei_lag3 < 2, 1, 0)) %>%
  mutate(extreme_wet = ifelse(spei >= 2, 1, 0)) %>%
  mutate(extreme_wet_lag3 = ifelse(spei_lag3 >= 2, 1, 0)) %>%
  mutate(moderate_dry = ifelse(spei > -1.5 & spei <= -1, 1, 0)) %>%
  mutate(moderate_dry_lag3 = ifelse(spei_lag3 > -1.5 & spei_lag3 <= -1, 1, 0)) %>%
  mutate(severe_dry = ifelse(spei > -2 & spei <= -1.5, 1, 0)) %>%
  mutate(severe_dry_lag3 = ifelse(spei_lag3 > -2 & spei_lag3 <= -1.5, 1, 0)) %>%
  mutate(extreme_dry = ifelse(spei <= -2, 1, 0)) %>%
  mutate(extreme_dry_lag3 = ifelse(spei_lag3 <= -2, 1, 0)) %>% 
  mutate(crop_number_total=ifelse(is.na(crop_number_total),0,crop_number_total))%>%
  group_by(region)%>%
  mutate(biodiversity1=std(CoKrig))%>%
  mutate(biodiversity2=std(Krig))%>% 
  mutate(mammal_bio = std(mammal_richness)) %>%
  mutate(bushmeat_bio = std(BushDiv_rasValue)) %>%
  mutate(elevation=std(elevation))%>% 
  mutate(city=std(city))%>% 
  mutate(soil_terrain=std(soil_terrain))%>% 
  mutate(growing_days=std(growing_days))%>% 
  mutate(crop_diversity=std(crop_number_total))%>%
  mutate(forest=std(forest))%>%
  mutate(city=std(city))%>%
  ungroup()%>%
  filter(!is.na(tot.inc) & !is.na(planting) & !is.na(spei)) %>%
  as.data.frame()

##write.csv(z, "04.02.2024_cleaned_datasets/data-regression-part1.csv")

################################################################################################################
#################################################################################################################
####Specification 1: drought and flood dummy without seasons - Baseline
################################################################################################################
#################################################################################################################

b1 = felm(ihs(bm.inc) ~ dry + wet | factor(ghousecode) + factor(intyear)|0|gvillcode,
          z)

summary(b1)
names=c("bushmeat income")
cn=c("dry","wet")

htmlreg(list(b1),custom.model.names=names,custom.coef.names=cn,
        caption = "Baseline Plot",
        caption.above = TRUE,digits=2,stars = c(0.01,0.05, 0.1),
        file = "04.02.2024_Results/baselineBMinc.doc")


plot_title <- "Baseline Plot without Seasons"
# Define the title
ft="Times New Roman"
fs=22
plot <- dwplot(b1,dodge_size=0.8, dot_args = list(size = 5), style = "dotwhisker",whisker_args = list(size = 2))+
  scale_colour_manual(values=c("red")) +
  scale_x_continuous(limits = c(-0.2, 0.3)) + 
  geom_vline(xintercept = 0, colour = "black", linetype = 2) +
  theme_few() +
  theme(legend.position="none",
        strip.text.x = element_text(size = fs, family=ft),
        axis.text.y= element_text(size = fs, family=ft),
        axis.title.y= element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size=fs, family=ft),
        axis.title.x = element_text(size=fs, family=ft),
        plot.title = element_text(size = 20, hjust = 0.5, family = ft, face = "bold"),
        plot.subtitle = element_text(size = 15, hjust = 0.5, family = ft, face = "bold")) +
  labs(title = plot_title,
       subtitle = "Bushmeat Income")
ggsave("04.02.2024_Results/baselineBMinc_without_seasons.png", plot, width = 15, height = 10, units = "in")
################################################################################################################
#################################################################################################################
######Specification 2: drought and flood dummy with Seasons
################################################################################################################
#################################################################################################################

b1 = felm(ihs(bm.inc) ~ planting + growing + harvesting + dry + wet
        + planting:dry + planting:wet + growing:dry + growing:wet 
        + harvesting:dry + harvesting:wet
        |factor(ghousecode)+factor(intyear)|0|gvillcode,
        z)
summary(b1)

t1 = felm(ihs(tot.inc) ~ planting + growing + harvesting + dry + wet
          + planting:dry + planting:wet + growing:dry + growing:wet 
          + harvesting:dry + harvesting:wet
          |factor(ghousecode)+factor(intyear)|0|gvillcode,
          z)

summary(t1)
names=c("bushmeat","total")
cn=c("planting","growing","harvesting","dry","wet","planting$*$dry","planting$*$wet",
     "growing$*$dry","growing$*$wet","harvesting$*$dry","harvesting$*$wet")

htmlreg(list(b1,t1),custom.model.names=names,custom.coef.names=cn,
        caption = "Baseline specification with Dry and Wet Dummy",
        caption.above = TRUE,digits=2,stars = c(0.01,0.05, 0.1),
        include.rsquared = FALSE, include.adjrs = FALSE, include.rmse = FALSE,
        include.fstatistic = FALSE,include.nclusts = FALSE,include.groups = FALSE,
        file = "04.02.2024_Results/Bushmeat_Results-Dummy-baseline.doc")

texreg(list(b1,t1),custom.model.names=names,custom.coef.names=cn,
       caption = "Baseline specification with Dry and Wet Dummy",
       caption.above = TRUE,stars = c(0.01,0.05, 0.1),
       include.rsquared = FALSE, include.adjrs = FALSE, include.rmse = FALSE,
       include.fstatistic = FALSE,include.nclusts = FALSE,include.groups = FALSE,
       file = "04.02.2024_Results/Bushmeat_Results-Dummy-baseline.tex")
################################################################################################################
#################################################################################################################
##############################Dot and Whisker only for Bushmeat and Total Income
################################################################################################################
#################################################################################################################

f=function(m,v,c) {
  int <- (1-c/2) # defines confidence band based on confidence level
  term=paste(rownames(m$coefficients)[v[1]],rownames(m$coefficients)[v[2]],sep="+") # This function is used to concatenate (combine) the names of the coefficients specified by v[1] and v[2]..v = c(4, 6), it means combine the names at indices 4 and 6.
  estimate<-sum(m$coefficients[v]) # sums coefficients
  std.error<-sqrt(sum(m$clustervcv[v,v])) # calculates the standard errors from variance-covariance matrix. To get the std error of marginal terms this is the formula : basically sum the variance co-variance matrix and take the square root of it. 
  conf.low<-estimate-std.error*qnorm(int) # calculate lower bound of the confidence interval
  conf.high<-estimate+std.error*qnorm(int) # calculate upper bound of the confidence interval
  return(c(term, estimate, std.error,conf.low, conf.high))
}
 # returns a vector containing the variable name (term),
  #estimate, standard error, and bounds of the confidence interval.


# confidence level
cl=0.1 #sets the confidence level (cl) to 0.1 (which corresponds to a 90% confidence interval).
#Bushmeat Income
m = b1
bm = cbind(rbind(tidy(m,conf.int = T,conf.level = (1-cl))%>%
                   filter(!term%in%c("planting:dry","planting:wet","growing:dry","growing:wet","harvesting:dry","harvesting:wet")) %>%
                   select(term,estimate,std.error,conf.low,conf.high),
                 f(m,c(4,6),cl),f(m,c(5,7),cl),f(m,c(4,8),cl),f(m,c(5,9),cl),f(m,c(4,10),cl),f(m,c(5,11),cl)
),model="Bushmeat income"
)
bm$estimate <- as.numeric(bm$estimate)
bm$std.error <- as.numeric(bm$std.error)

# Calculate p-values
bm$p_values <- 2 * (1 - pnorm(abs(bm$estimate / bm$std.error)))

##Total Income
m = t1
tm=cbind(rbind(tidy(m,conf.int = T,conf.level = (1-cl))%>%
                 filter(!term%in%c("planting:dry","planting:wet","growing:dry","growing:wet","harvesting:dry","harvesting:wet")) %>%
                 select(term,estimate,std.error,conf.low,conf.high),
               f(m,c(4,6),cl),f(m,c(5,7),cl),f(m,c(4,8),cl),f(m,c(5,9),cl),f(m,c(4,10),cl),f(m,c(5,11),cl)
),model="Total income"
)

tm$estimate <- as.numeric(tm$estimate)
tm$std.error <- as.numeric(tm$std.error)

# Calculate p-values
tm$p_values <- 2 * (1 - pnorm(abs(tm$estimate / tm$std.error)))

d = rbind(bm,tm)%>% mutate(term=recode(term,
                                                "planting"="Planting season",
                                                "growing"="Growing season",
                                                "harvesting"="Harvesting season",
                                                "dry"="Dry",
                                                "wet"="Wet",
                                                "dry+planting:dry"="Dry planting season",
                                                "wet+planting:wet" = "Wet planting season",
                                                "dry+growing:dry"="Dry growing season",
                                                "wet+growing:wet"= "Wet growing season",
                                                "dry+harvesting:dry"="Dry harvesting season",
                                                "wet+harvesting:wet" = "Wet harvesting season"
))%>%
  mutate(order=rep(c(1:5,6:11),2))%>%
  arrange(order)


# Function to determine significance level
get_significance <- function(p_value) {
  if (p_value <= 0.01) {
    return("***")
  } else if (p_value <= 0.05) {
    return("**")
  } else if (p_value <= 0.1) {
    return("*")
  } 
  else {
    return("")
  }
}

# Add significance level to data frame
d$significance <- sapply(d$p_values, get_significance)
# Filter out the first 3 coefficients
d_filtered <- d %>% filter(!term %in% c("Planting season", "Growing season", "Harvesting season"))
# Convert 'model' column to a factor with desired order
d_filtered$model <- factor(d_filtered$model, 
                           levels = c("Bushmeat income" ,"Total income"), 
                           labels = c("Bushmeat income","Total income"))
d_filtered <- d_filtered %>% arrange(desc(model))
ft="Times New Roman"
fs=22
#***dwplot provides an option to present the average marginal effect directly based on margins.###
#shows the regression coefficients' point estimates as dots with confidence interval whiskers. 
# Define the title
plot_title <- "Marginal Effects of Dry and Wet Episodes on Bushmeat Income and Total Income"
plot1 <- dwplot(d_filtered,dodge_size=0.8, dot_args = list(size = 5), style = "dotwhisker",whisker_args = list(size = 2))+
  scale_colour_manual(values=c("orange","seagreen3"))+
  geom_vline(xintercept = 0, colour = "black", linetype = 2) +
  theme_few()+
  facet_wrap(~model, ncol = 2) +
  theme(legend.position="none",
        strip.text.x = element_text(size = fs, family=ft),
        axis.text.y= element_text(size = fs, family=ft),
        axis.title.y= element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size=fs, family=ft),
        axis.title.x = element_text(size=fs, family=ft),
        plot.title = element_text(size = 20, hjust = 0.5, family = ft, face = "bold")) + 
  geom_text(aes(label = significance, x = estimate, y = term), size = 7, hjust = -0.2, vjust = -0.5) +  # Add significance labels
  labs(title = plot_title)  # Add title
plot1
ggsave("04.02.2024_Results/BMincome_baseline.png", plot1, width = 15, height = 10, units = "in")

######################################################################################################################
################################################################################################################
#################################################################################################################
######Specification 3: Bushmeat Income and Bushmeat Reliance
################################################################################################################
#################################################################################################################

b1a = felm(ihs(bm.inc) ~ planting + growing + harvesting + dry + wet
           + planting:dry + planting:wet + growing:dry + growing:wet 
           + harvesting:dry + harvesting:wet
           |factor(ghousecode)+factor(intyear)|0|gvillcode,
           z)

summary(b1a)

r1a = felm(reliance ~ planting + growing + harvesting + dry + wet
          + planting:dry + planting:wet + growing:dry + growing:wet 
          + harvesting:dry + harvesting:wet 
          |factor(ghousecode)+factor(intyear)|0|gvillcode,
          z)

summary(r1a)

names=c("bushmeat","reliance")
cn=c("planting","growing","harvesting","dry","wet","planting$*$dry","planting$*$wet",
     "growing$*$dry","growing$*$wet","harvesting$*$dry","harvesting$*$wet")

htmlreg(list(b1a,r1a),custom.model.names=names,custom.coef.names=cn,
        caption = "Baseline specification with Dry and Wet Dummy",
        caption.above = TRUE,digits=2,stars = c(0.01,0.05, 0.1),
        include.rsquared = FALSE, include.adjrs = FALSE, include.rmse = FALSE,
        include.fstatistic = FALSE,include.nclusts = FALSE,include.groups = FALSE,
        file = "04.02.2024_Results/BushmeatReliance_Results-baseline.doc")

texreg(list(b1a,r1a),custom.model.names=names,custom.coef.names=cn,
       caption = "Baseline specification with Dry and Wet Dummy",
       caption.above = TRUE,stars = c(0.01,0.05, 0.1),
       include.rsquared = FALSE, include.adjrs = FALSE, include.rmse = FALSE,
       include.fstatistic = FALSE,include.nclusts = FALSE,include.groups = FALSE,
       file = "04.02.2024_Results/BushmeatReliance_Results-baseline.tex")

################################################################################################################
#################################################################################################################
##############################Dot and Whisker only for Bushmeat Income and Reliance
################################################################################################################
#################################################################################################################

f=function(m,v,c) {
  int <- (1-c/2) # defines confidence band based on confidence level
  term=paste(rownames(m$coefficients)[v[1]],rownames(m$coefficients)[v[2]],sep="+") # This function is used to concatenate (combine) the names of the coefficients specified by v[1] and v[2]..v = c(4, 6), it means combine the names at indices 4 and 6.
  estimate<-sum(m$coefficients[v]) # sums coefficients
  std.error<-sqrt(sum(m$clustervcv[v,v])) # calculates the standard errors from variance-covariance matrix. To get the std error of marginal terms this is the formula : basically sum the variance co-variance matrix and take the square root of it. 
  conf.low<-estimate-std.error*qnorm(int) # calculate lower bound of the confidence interval
  conf.high<-estimate+std.error*qnorm(int) # calculate upper bound of the confidence interval
  return(c(term, estimate, std.error,conf.low, conf.high))
}

# confidence level
cl=0.1 #sets the confidence level (cl) to 0.1 (which corresponds to a 90% confidence interval).

m = b1a
bm = cbind(rbind(tidy(m,conf.int = T,conf.level = (1-cl))%>%
                   filter(!term%in%c("planting:dry","planting:wet","growing:dry","growing:wet","harvesting:dry","harvesting:wet")) %>%
                   select(term,estimate,std.error,conf.low,conf.high),
                 f(m,c(4,6),cl),f(m,c(5,7),cl),f(m,c(4,8),cl),f(m,c(5,9),cl),f(m,c(4,10),cl),f(m,c(5,11),cl)
),model="Bushmeat income"
)
bm$estimate <- as.numeric(bm$estimate)
bm$std.error <- as.numeric(bm$std.error)

# Calculate p-values
bm$p_values <- 2 * (1 - pnorm(abs(bm$estimate / bm$std.error)))

m = r1a
tm=cbind(rbind(tidy(m,conf.int = T,conf.level = (1-cl))%>%
                 filter(!term%in%c("planting:dry","planting:wet","growing:dry","growing:wet","harvesting:dry","harvesting:wet")) %>%
                 select(term,estimate,std.error,conf.low,conf.high),
               f(m,c(4,6),cl),f(m,c(5,7),cl),f(m,c(4,8),cl),f(m,c(5,9),cl),f(m,c(4,10),cl),f(m,c(5,11),cl)
),model="Reliance(as % of Total Income)"
)

tm$estimate <- as.numeric(tm$estimate)
tm$std.error <- as.numeric(tm$std.error)

# Calculate p-values
tm$p_values <- 2 * (1 - pnorm(abs(tm$estimate / tm$std.error)))

d = rbind(bm,tm)%>% mutate(term=recode(term,
                                       "planting"="Planting season",
                                       "growing"="Growing season",
                                       "harvesting"="Harvesting season",
                                       "dry"="Dry",
                                       "wet"="Wet",
                                       "dry+planting:dry"="Dry planting season",
                                       "wet+planting:wet" = "Wet planting season",
                                       "dry+growing:dry"="Dry growing season",
                                       "wet+growing:wet"= "Wet growing season",
                                       "dry+harvesting:dry"="Dry harvesting season",
                                       "wet+harvesting:wet" = "Wet harvesting season"
))%>%
  mutate(order=rep(c(1:5,6:11),2))%>%
  arrange(order)


# Function to determine significance level
get_significance <- function(p_value) {
  if (p_value <= 0.01) {
    return("***")
  } else if (p_value <= 0.05) {
    return("**")
  } else if (p_value <= 0.1) {
    return("*")
  } 
  else {
    return("")
  }
}

# Add significance level to data frame
d$significance <- sapply(d$p_values, get_significance)
# Filter out the first 3 coefficients
d_filtered <- d %>% filter(!term %in% c("Planting season", "Growing season", "Harvesting season"))
# Convert 'model' column to a factor with desired order
d_filtered$model <- factor(d_filtered$model, 
                           levels = c("Bushmeat income", "Reliance(as % of Total Income)"), 
                           labels = c("Bushmeat income", "Reliance(as % of Total Income)"))
d_filtered <- d_filtered %>% arrange(desc(model))
ft="Times New Roman"
fs=22
#***dwplot provides an option to present the average marginal effect directly based on margins.###
#shows the regression coefficients' point estimates as dots with confidence interval whiskers. 
# Define the title
plot_title <- "Marginal Effects of Dry and Wet Episodes on Bushmeat Income and Reliance"
plot2 <- dwplot(d_filtered,dodge_size=0.8, dot_args = list(size = 5), style = "dotwhisker",whisker_args = list(size = 2))+
  scale_colour_manual(values=c("orange","seagreen3"))+
  geom_vline(xintercept = 0, colour = "black", linetype = 2) +
  theme_few()+
  facet_wrap(~model, ncol = 2) +
  theme(legend.position="none",
        strip.text.x = element_text(size = fs, family=ft),
        axis.text.y= element_text(size = fs, family=ft),
        axis.title.y= element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size=fs, family=ft),
        axis.title.x = element_text(size=fs, family=ft),
        plot.title = element_text(size = 20, hjust = 0.5, family = ft, face = "bold")) + 
  geom_text(aes(label = significance, x = estimate, y = term), size = 7, hjust = -0.2, vjust = -0.5) +  # Add significance labels
  labs(title = plot_title)  # Add title
plot2
ggsave("04.02.2024_Results/BM_Reliance_baseline.png", plot2, width = 15, height = 10, units = "in")
################################################################################################################
#################################################################################################################
################################By Country################################################################
################################################################################################################
#################################################################################################################

#the year dummy coefficient estimates the common change (across all countries/regions) in the bushmeat income in year t relative to the
#base year, controlling for seasons, shocks and country-specific time-invariant characteristics (the country fixed effects). We call it a year fixed effect because the change is common to all countries in year t;
#in other words, the ‘effect’ of year t is ‘fixed’ across all countries.The year fixed effects (i.e. year dummy variables) control for factors changing each year that are common
# to all countries for a given year.

#Similarly, country dummy coefficient estimates the common change (to all years) in the bushmeat income in country i relative
#to base country, controlling for seasons, shocks and year-specific characteristics/shocks common to all countries
#(the year fixed effects). We call it country fixed effect precisely because the difference is common to all years in country i; in other words, the ‘effect’ of country i is ‘fixed’ across all years.
#the country fixed effects (i.e. country dummy variables) control for baseline differences between countries.

# β1(dry) is the estimated effect of drought on bushmeat income, controlling for seasons and country-specific time-invariant
#characteristics and year-specific shocks (the country and year fixed effects).
################################################################################################################
#################################################################################################################
##########################################Regional Dummies#################################################
################################################################################################################
#################################################################################################################

fe_model <- felm(ihs(bm.inc) ~ dry + wet + factor(region):dry + factor(region):wet |factor(intyear)|0|gvillcode,
                 z)
# Summarize the model
summary(fe_model)

fe_model <- felm(ihs(bm.inc) ~ planting + growing + harvesting + dry + wet
                 + planting:dry + planting:wet + growing:dry + growing:wet 
                 + harvesting:dry + harvesting:wet + factor(region):dry + factor(region):wet |factor(intyear)|0|gvillcode,
                 z)
# Summarize the model
summary(fe_model)

################################################################################################################
#################################################################################################################
##################################country and year dummies####################################################
################################################################################################################
#################################################################################################################

fe_model <- felm(ihs(bm.inc) ~ planting + growing + harvesting + dry + wet
                 + planting:dry + planting:wet + growing:dry + growing:wet 
                 + harvesting:dry + harvesting:wet + factor(country):dry + factor(country):wet |factor(intyear)|0|gvillcode,
                 z)
# Summarize the model
summary(fe_model)
















