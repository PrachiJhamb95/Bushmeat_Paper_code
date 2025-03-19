# Getting the path of your current file
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(haven)
library(dplyr)

globalwd <- "/Users/prachijhamb/Library/CloudStorage/OneDrive-UniversityofGeorgia/Research/Bushmeat Project/"
setwd(globalwd)

INQ=read.csv("Replication-Noack2019JAERE-Biodiversity/all_inc_allqtrs.csv",sep=",")
PLT_BIO=read.csv("Replication-Noack2019JAERE-Biodiversity/biodiversity-gaez.csv",sep=",")
BM_BIO = read.csv("04.02.2024_cleaned_datasets/Bushmeat-diversity.csv")
mammal_rich = read.csv("04.02.2024_cleaned_datasets/mammal_richness.csv")
GEO=read.csv("Replication-Noack2019JAERE-Biodiversity/geodata.csv",sep=",")
#SPEI3=read.csv("Replication-Noack2019JAERE-Biodiversity/spei3.csv",sep=",") #Noack's SPEI03
#SPEI6=read.csv("Replication-Noack2019JAERE-Biodiversity/spei6.csv",sep=",") #Noack's SPEI06

SPEI3=read.csv("04.02.2024_cleaned_datasets/spei3.csv",sep=",") #this is the latest version of SPEI03
SPEI6=read.csv("Try_all_spei_versions/version29_spei06_and_spei12.csv",sep=",") #latest version of SPEI06 and SPEI12
SEAS=read.csv("Replication-Noack2019JAERE-Biodiversity/seasons.csv",sep=",")
CRO=read.csv("Replication-Noack2019JAERE-Biodiversity/crop-diversity.csv",sep=",")
Bush_INQ=read_dta("04.02.2024_raw data/nielsen data/Global PEN downloaded 13.06.2013_/quarterly_income_five.dta") %>% 
  select("gvillcode","ghousecode","qtr","siteid","percentage_hunting", "hunting_dummy", "number_hunters" , "village_sample_size","tot_bm_net_inc_aeu_ppp" ,"tot_bm_cash_net_aeu_ppp" , 
        "tot_bm_sub_net_aeu_ppp", "tot_inc_aeu_ppp", "tot_crop_net_inc_aeu_ppp","tot_liv_net_inc_aeu_ppp","wage_cash_inc_aeu_ppp","biz_net_inc_aeu_ppp","tot_oth_inc_aeu_ppp",
        "tot_sub_inc_aeu_ppp","tot_cash_inc_aeu_ppp")

####################################################### Geo #######################################

geo=GEO[,c("gvillcode","Mean.Prctree.00.10","Distance.nearest.Road","Distance.to.nearest.city","Elevation","inf_elec","dem_hhd")]
names(geo)=c("gvillcode","forest","road","city","elevation","electric_hh","households")
# this is because NA's are coded as negative values
geo[geo<0]=NA


###############################################Merge INQ with Bushmeat Income##########################
# Convert "gvillcode" column in Bush_INQ to integer
Bush_INQ$gvillcode <- as.integer(Bush_INQ$gvillcode)
Bush_INQ$siteid <- as.integer(Bush_INQ$siteid)

# Use the merge function
INQ <- merge(INQ,Bush_INQ, by = c("gvillcode", "ghousecode", "qtr", "siteid"),all.x=T)
#INQ_merge2 <- merge(INQ, Bush_INQ, by = c("gvillcode", "ghousecode", "qtr", "siteid"),all=T)

####################################################### Income #######################################

# forest income
for.inc=rowSums(cbind(INQ$net_dfi_inc,INQ$net_fw_inc,INQ$net_fdi_inc),na.rm=T)/INQ$ppp
# environment and fish income
env.inc=rowSums(cbind(INQ$net_fishfor_inc,INQ$net_fishenv_inc,INQ$net_env_inc),na.rm=T)/INQ$ppp
# resource income
res.inc=env.inc+for.inc
# crop income
cro.inc=INQ$net_crop_inc/INQ$ppp 
#crop subsistence income 
subs.cro.inc = INQ$crop_netsub_inc / INQ$ppp

#crop cash income 
cash.cro.inc = INQ$crop_netcash_inc / INQ$ppp

#bush meat income
bm.inc = INQ$tot_bm_net_inc_aeu_ppp

#bush meat subsistence income
subs.bm.inc = INQ$tot_bm_sub_net_aeu_ppp

#bush meat cash income
cash.bm.inc = INQ$tot_bm_cash_net_aeu_ppp


# livestock income
liv.inc=INQ$net_liv_inc/INQ$ppp 

#livestock subsistence income
subs.liv.inc = INQ$liv_netsub_inc / INQ$ppp

#livestock cash income
cash.liv.inc = INQ$liv_netcash_inc / INQ$ppp

# agriculture income
agr.inc=cro.inc+liv.inc
# labor income
wag.inc=INQ$wage_cash_inc/INQ$ppp 
# business income
bus.inc=INQ$net_biz_inc/INQ$ppp 
# total income
tot.inc=INQ$tot_inc_aeu_ppp 

#total subsistence income
subs.tot.inc = INQ$tot_sub_inc_aeu_ppp
#total cash income
cash.tot.inc = INQ$tot_cash_inc_aeu_ppp

# other total income that is cash transfers and remittances income and other transfers
oth_totinc = INQ$oth_totinc/INQ$ppp 
# other income
oth.inc=tot.inc- cro.inc - bm.inc
oth.inc[oth.inc<0]=0

# other subsistence income
subs.oth.inc = subs.tot.inc - subs.cro.inc - subs.bm.inc 
subs.oth.inc[subs.oth.inc <0] =0

# other cash income
cash.oth.inc = cash.tot.inc - cash.cro.inc - cash.bm.inc
cash.oth.inc[cash.oth.inc <0] =0

# forest and crop income combined
fcr.inc=for.inc+cro.inc

#
inc=cbind(bm.inc,cro.inc,liv.inc,wag.inc,bus.inc,tot.inc,oth.inc,for.inc,env.inc,res.inc,fcr.inc,agr.inc,oth_totinc,
          subs.bm.inc,subs.cro.inc,subs.liv.inc,subs.oth.inc,subs.tot.inc, cash.bm.inc, cash.cro.inc, cash.liv.inc,
          cash.oth.inc, cash.tot.inc)

### regional dummies
country=as.vector(INQ$country)
country[country=="Congo, Dem. Rep."]="DRC"

region=rep(NA,dim(INQ)[1])
region[which(country%in%c("Belize","Bolivia","Ecuador","Guatemala","Brazil","Peru"))]="LA"
region[which(country%in%c("Bangladesh","India","Nepal"))]="SA"
region[which(country%in%c("China","Indonesia","Vietnam","Cambodia"))]="EA"
region[which(country%in%c("Cameroon","Ethiopia","Malawi","Mozambique","Senegal","Ghana","Uganda","Zambia","Burkina Faso","DRC","Nigeria"))]="AF"


############################### merge data ######################################
A0=cbind(INQ[,c("siteid","gvillcode","ghousecode","qtr","start_year", "percentage_hunting", "hunting_dummy", "number_hunters" , "village_sample_size", "tot_bm_cash_net_aeu_ppp" , "tot_bm_sub_net_aeu_ppp")],inc,country,region)
A1=merge(A0,PLT_BIO%>%select(-Village, -Lat.dec.deg, -Long.dec.deg),all=T)
A2=merge(A1,geo,all=T)
A3=merge(A2,SPEI3,all=T)
A4=merge(A3,SEAS,all=T)
A5=merge(A4,CRO,all=T)
A6=merge(A5,SPEI6,all=T)
A7 = merge(A6,BM_BIO, all=T)
A8 = merge(A7,mammal_rich,all=T)
A=A8
A[A==-8|A==-9]=NA
A=A[which(!is.na(A$ghousecode) & !is.na(A$tot.inc)),]


#write.csv(A,"01.02.2024_cleaned_datasets/noackreplication-data-regression.csv",row.names=F)
write.csv(A,"04.02.2024_cleaned_datasets/data-regression.csv",row.names=F)

#############################Summary Statistics#############################################################

B = A %>%
  group_by(ghousecode)%>%
  mutate(total_income = sum(tot.inc,na.rm=T))%>%
  mutate(subs_total_income = sum(subs.tot.inc, na.rm = T)) %>%
  mutate(cash_total_income = sum(cash.tot.inc, na.rm = T)) %>%
  mutate(bm_income = sum(bm.inc,na.rm=T))%>%
  mutate(subs_bm_income = sum(subs.bm.inc,na.rm=T))%>%
  mutate(cash_bm_income = sum(cash.bm.inc,na.rm=T))%>%
  mutate(crop_income = sum(cro.inc,na.rm=T))%>%
  mutate(subs_crop_income = sum(subs.cro.inc,na.rm=T))%>%
  mutate(cash_crop_income = sum(cash.cro.inc,na.rm=T))%>%
  mutate(other_income = sum(oth.inc,na.rm=T))%>%
  mutate(subs_other_income = sum(subs.oth.inc,na.rm=T))%>%
  mutate(cash_other_income = sum(cash.oth.inc,na.rm=T))%>%
  select(ghousecode,total_income,bm_income,crop_income,other_income,country,region,
         subs_total_income,cash_total_income,subs_bm_income,cash_bm_income,subs_crop_income,
         cash_crop_income,subs_other_income,
         cash_other_income)%>%
  unique()%>%
  as.data.frame()

####################################### 2 Panel- Summary#################################################
q = as.data.frame(matrix(NA,ncol=1,nrow=4))
colnames(q)=c("Mean")
rownames(q)=c("Total income [USD/AEU/year]",
              "Bushmeat income share [%]",
              "Crop income share [%]",
              "Other income share [%]"
)

f=function(x) return(round(as.numeric(c(
  mean(x,na.rm=T)
)),0))


q[1,]=f(B$total_income)
q[2,]=f(B$bm_income/B$total_income*100)
q[3,]=f(B$crop_income/B$total_income*100)

q[4,]=f(B$other_income/B$total_income*100)
library(xtable)

print(xtable(q),type="html","01.12.2024_Results/Income_shares_summary.html")
print(xtable(q),type="latex","01.12.2024_Results/Income_shares_summary.tex")



###################################Full_Panel_summary######################################################################
q=as.data.frame(matrix(NA,ncol=3,nrow=7))
colnames(q)=c("Mean","Standard Deviation", "Median")
rownames(q)=c("Total income [USD/AEU/year]",
              "Crop income [USD/AEU/year]",
              "Crop income share [%]",
              "Bushmeat income [USD/AEU/year]",
              "Bushmeat income share [%]",
              "Other income [USD/AEU/year]",
              "Other income share [%]"
)

f=function(x) return(round(as.numeric(c(
  mean(x,na.rm=T),
  sd(x,na.rm=T),
  median(x,na.rm = T)
)),0))


q[1,]=f(B$total_income)
q[2,]=f(B$crop_income)
q[3,]=f(B$crop_income/B$total_income*100)

q[4,]=f(B$bm_income)
q[5,]=f(B$bm_income/B$total_income*100)

q[6,]=f(B$other_income)
q[7,]=f(B$other_income/B$total_income*100)
library(xtable)

## Remittances are 7 percent of total income
### Income does not add upto 100. It only adds upto 99.

print(xtable(q),type="html","01.07.2024_Results/sumary_statistics.html")
print(xtable(q),type="latex","01.07.2024_Results/sumary_statistics.tex")


####################################### Subsistence / Cash- Summary#################################################
#delete the row that has a outlier that seems to be a wrong entry
#B <- B %>% slice(-7304)

q=as.data.frame(matrix(NA,ncol=1,nrow=4))
colnames(q)=c("Mean")
rownames(q)=c("Bushmeat subsistence income share [%]",
              "Bushmeat cash income share [%]",
              "Crop subsistence income share [%]",
              "Crop cash income share [%]"
              
)

f=function(x) return(round(as.numeric(c(
  mean(x,na.rm=T)
)),0))


q[1,]=f(B$subs_bm_income/B$bm_income *100)
q[2,]=f(B$cash_bm_income/B$bm_income *100)

q[3,]=f(B$subs_crop_income/B$crop_income *100)
q[4,]=f(B$cash_crop_income/B$crop_income *100)

library(xtable)
print(xtable(q),type="html","01.12.2024_Results/cash-subsistence-sumary.html")
print(xtable(q),type="latex","01.12.2024_Results/cash-susbistence-sumary.tex")


###################################################################################

# Create a vector of country names excluding India and Ecuador
country_names <- unique(B$country[B$country != "India" & B$country != "Ecuador"])

# Create an empty dataframe to store the summary statistics
summary_table <- as.data.frame(matrix(NA, ncol = length(country_names), nrow = 1))
colnames(summary_table) <- country_names
rownames(summary_table) <- c("Bushmeat subsistence income share [%]")

# Function to calculate mean
mean_function <- function(x) {
  return(round(mean(x, na.rm = TRUE), 0))
}

# Calculate mean for subsistence and cash bushmeat income by country
for (i in 1:length(country_names)) {
  country <- country_names[i]
  subsistence_share <- mean_function(B$subs_bm_income[B$country == country] / B$bm_income[B$country == country] * 100)
  #cash_share <- mean_function(B$cash_bm_income[B$country == country] / B$bm_income[B$country == country] * 100)
  summary_table[1, country] <- subsistence_share
  #summary_table[2, country] <- cash_share
}
library(ggplot2)
library(tidyr)  # for gather function

# Reshape the summary table from wide to long format
summary_table_long <- gather(summary_table, key = "Country", value = "Subsistence_Income_Share", factor_key = TRUE)

# Plotting the bar graph for subsistence income shares
plot1<- ggplot(summary_table_long, aes(x = Country, y = Subsistence_Income_Share, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Country", y = "Subsistence % of Total Bushmeat Income", fill = "") +
  ggtitle("Subsistence Bushmeat Income Share") +
  theme_economist_white() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title = element_text(face = "bold")) +  # Making axis titles bold
  guides(fill = FALSE)

ggsave("04.02.2024_Results/SubsBushmeatIncomeShare.png", plot1, width = 15, height = 10, units = "in")

###################################Diagnostics#############################################################
subset_columns_crop <- B[, c(4, 13, 14)]
subset_columns_crop$perc = subset_columns_crop$subs_crop_income/subset_columns_crop$crop_income * 100

subset_columns_bm <- B[, c(3, 11, 12)]
subset_columns_bm$perc = subset_columns_bm$subs_bm_income/subset_columns_bm$bm_income * 100

#subset_columns_liv <- B[, c(5, 15, 16)]
#subset_columns_liv$perc = subset_columns_liv$subs_livestock_income/subset_columns_liv$livestock_income * 100
#subset_columns_liv$diff = subset_columns_liv$livestock_income - subset_columns_liv$subs_livestock_income
#delete the row that has a outlier that seems to be a wrong entry
#subset_columns_liv <- subset_columns_liv %>% slice(-7304)


#summary(subset_columns_liv)