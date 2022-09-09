## NUMBER 1 (DISTANCE)
## RECOMMENDED FORMULA for eREC - non-rapid studies, 21 studies total
#install.packages("stats")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("brms")
#install.packages("Rcpp")
#install.packages("reshape2")
#install.packages("data.table")
#update.packages()
library(utils)
library(stats)
library(dplyr)
library(tidyverse)
library(dplyr)
library(Rcpp)
library(brms) 
options(mc.cores = parallel::detectCores())
library(reshape2)
library(data.table)
setwd('C:/Users/Majdi/Desktop/Ali R')
data1<- read.csv("nonraabcrosswalkfinalWBI.csv") 
str(data1)
df2<- dplyr::mutate(data1, better_distance_acuity_presenting  = pmin(Pres_DVA_RE_logmar,Pres_DVA_LE_logmar, na.rm = T))
df1 <- dplyr::mutate(df2, better_distance_acuity_corrected  = pmin(BC_DVA_RE_logmar,BC_DVA_LE_logmar, na.rm = T))#shahroud
df<- dplyr::mutate(df1, better_distance_acuity_uncorrected  = pmin(Unc_DVA_RE_logmar,Unc_DVA_LE_logmar, na.rm = T))
##### exclude-6/12 #######################################################----
df<- df %>% mutate(
  a_6.12  =  case_when(df$Glasses_status_Pres_DVA== TRUE &df$better_distance_acuity_presenting <=0.3&df$better_distance_acuity_uncorrected>0.3 ~ TRUE, TRUE ~ FALSE),
  c_6.12= case_when(df$Glasses_status_Pres_DVA == TRUE & df$better_distance_acuity_presenting >0.3&df$better_distance_acuity_uncorrected>0.3 & df$better_distance_acuity_corrected <=0.3 ~ TRUE, TRUE ~ FALSE), 
  d_6.12= case_when(df$Glasses_status_Pres_DVA == FALSE & df$better_distance_acuity_uncorrected >0.3  &df$better_distance_acuity_corrected <=0.3 ~ TRUE, TRUE ~ FALSE),
  
  exclusion6.12.F = case_when(Glasses_status_Pres_DVA==FALSE & 
                                better_distance_acuity_presenting <= 0.3 ~ TRUE, TRUE ~ FALSE),
  exclusion6.12.D = case_when(Glasses_status_Pres_DVA==FALSE & 
                                better_distance_acuity_presenting > 0.3& better_distance_acuity_corrected> 0.3~ TRUE, TRUE ~ FALSE),
  exclusion6.12.E = case_when(Glasses_status_Pres_DVA==TRUE & 
                                better_distance_acuity_presenting > 0.3& better_distance_acuity_corrected> 0.3~ TRUE, TRUE ~ FALSE))
df<- df %>% filter(Age %in% (50:100) )
df<-df%>%mutate(ageD=
                  cut(Age,breaks=c(seq(49,90,10),150),labels=c("50-59","60-69","70-79","80-89","90+"),
                      include.lowest=T,right=F))
df$gender <- as.factor(df$gender) 
df$country <- as.factor(df$country) 
df$gbd_region <- as.factor(df$gbd_region) 
df$year = (df$year - 2010) / 10
df$gbd_region <- as.factor(df$gbd_region)
df$region <- as.factor(df$region)
df = df %>% mutate(category = factor(case_when(a_6.12 ~ "Met Need", c_6.12 ~ "Unmet Need", d_6.12 ~ "Unmet Need", TRUE ~ "No need"), 
                                     levels=c("Met Need", "Unmet Need", "No need"), ordered=T))

## TRUE ~ "No need" is wrong. need to exclude instead!
# df = df %>% mutate(category = factor(case_when(a_6.12 ~ "Met Need", c_6.12 ~ "Unmet Need", d_6.12 ~ "Unmet Need", TRUE ~ "No need"), 
# levels=c("Met Need", "Unmet Need", "No need"), ordered=T))


# can't be corrected < unmet need < met need < doesn't need to be corrected

df = df %>% mutate(category = factor(case_when(a_6.12 ~ "Met Need", c_6.12 ~ "Unmet Need", d_6.12 ~ "Unmet Need", 
                                               exclusion6.12.F ~ "No need",
                                               exclusion6.12.D ~ "Non-refractive",
                                               exclusion6.12.E ~ "Non-refractive"), 
                                     levels=c("No need","Met Need", "Unmet Need", "Non-refractive"), ordered=T))

table(df$category)
sum(is.na(df$category))


levels(df$category)
df[is.na(df$category),][1:5,]
df = df[!is.na(df$category),]

table(df$category)
levels(df$category)

### some direct estimates
data.table(df)[, list(eREC = round(sum(category == "Met Need")/sum(category != "No need"),2)), by=list(gbd_region)]
data.table(df)[, list(met = round(mean(category == "Met Need"),2),unmet=
                        round(mean(category == "Unmet Need"),2),noneed=round(mean(category == "No need"),2)), by=list(gbd_region)]
data.table(df)[, list(eREC = round(sum(category == "Met Need")/sum(category != "No need"),2),met = round(mean(category == "Met Need"),2),unmet=
                        round(mean(category == "Unmet Need"),2),noneed=round(mean(category == "No need"),2),
                      nonrefractive=round(mean(category == "Non-refractive"),2)), by=list(gbd_region)]

### fit models ---- 
M = brm(category | thres(gr = gbd_region) ~ ageD + gender + year + (-1+year|gbd_region),
        family = cumulative(),prior <- c(prior(normal(0,1), class = "b"))) #, coef=year))

table(df$year*10+2010)
M = brm(category | thres(gr = gbd_region) ~ ageD + gender + year, # + (-1+year|gbd_region),
        data = df,
        family = cumulative(),iter=200, prior <- c(prior(normal(0,1), class = "b"))) # prior=prior, 


#family = cumulative(),iter=200, prior=prior

summary(M)


PID = abs(round(rnorm(1)*1e6))
saveRDS(M,sprintf("results/M-dataset3-%d.rds",PID))
saveRDS(df,sprintf("results/df-dataset3-%d.rds",PID))
saveRDS(M,sprintf("results/M-dataset1-%d.rds",PID))
saveRDS(df,sprintf("results/df-dataset1-%d.rds",PID))

print(sprintf("saved in results/df-dataset3-%d.rds",PID))
print(sprintf("saved in results/df-dataset1-%d.rds",PID))
exit()

### setup data frame for predictions ----
ages = unique(df$ageD)
genders = unique(df$gender)
years = seq(2000,2020,5)
years = (years - 2010) / 10
gbd_regions = unique(df$gbd_region) 
countries = unique(df$country)
df.prediction = expand.grid(ageD=ages,gender=genders,year=years,gbd_region=gbd_regions)
## merge in countries
countries.to.regions = read.csv("covars/countries-to-regions.csv")
countries.to.regions$superregion[countries.to.regions$superregion == "High-income"] = "High Income"
countries.to.regions$superregion[countries.to.regions$superregion == "Central Europe, Eastern Europe, and Central Asia"] = 
  "Central Europe, Eastern Europe and Central Asia"
countries.to.regions$gbd_region = factor(countries.to.regions$superregion)
countries.to.regions = countries.to.regions %>% select(country,gbd_region)
df.prediction = df.prediction %>% right_join(countries.to.regions,by="gbd_region")
### make predictions ---
# out = posterior_predict(M)
# predictions <- posterior_linpred(M, df.prediction, cores = 16,transform=TRUE)
# predictions <- posterior_predict(M, newdata=df.prediction, cores = 16)
predictions <- posterior_epred(M, newdata=df.prediction, cores = parallel::detectCores())
dim(predictions) # number of mcmc draws in rows X number of age-gender-year-country-regions in columns X number of categories (3)
colMeans(predictions[,1451,])
#### merge with population data ---
df.population = read.csv("population-processed.csv")
df.prediction$year = df.prediction$year * 10 + 2010
merged = left_join(df.prediction,df.population,by=c("ageD","country","gender","year"))
head(merged)
metNeed = predictions[,,1]
unmetNeed = predictions[,,2]
noNeed = predictions[,,3]
iters = dim(predictions)[1]
metNeed = cbind(data.table(t(metNeed)),merged)
## missing countries:
unique(metNeed$country[which(is.na(metNeed$pop))])
metNeed = metNeed[!is.na(pop),]
unmetNeed = cbind(data.table(t(unmetNeed)),merged)
unmetNeed = unmetNeed[!is.na(pop),]
eREC = metNeed
eREC[,1:iters] = metNeed[,1:iters] / (metNeed[,1:iters] + unmetNeed[,1:iters])
df.regions = metNeed %>% group_by(gender, year, gbd_region) %>% select(-ageD,-country) %>% 
  summarise_at(colnames(.)[1:iters],
               list(~ weighted.mean(., pop))) %>% ungroup()
estimates1 = data.matrix(df.regions %>% select(-c(gender,year,gbd_region)))   
df.regions = unmetNeed %>% group_by(gender, year, gbd_region) %>% select(-ageD,-country) %>% 
  summarise_at(colnames(.)[1:iters],
               list(~ weighted.mean(., pop))) %>% ungroup()
estimates2 = data.matrix(df.regions %>% select(-c(gender,year,gbd_region)))   
estimates = estimates1 / (estimates1 + estimates2)
library(matrixStats)
df.summary = data.frame(mean=rowMeans(estimates), li=rowQuantiles(estimates,probs=.025), ui=rowQuantiles(estimates,probs=.975))
df.summary = cbind(df.summary,df.regions %>% select(c(gender,year,gbd_region)))
head(df.summary)
df.summary$mean = round(df.summary$mean,2)
df.summary$li = round(df.summary$li,2)
df.summary$ui = round(df.summary$ui,2)
write.csv(df.summary,"results.csv",row.names=F)
# 
# ## age=50
# df.regions = metNeed %>% filter(ageD == "50-59") %>% group_by(gender, year, gbd_region) %>% select(-ageD,-country) %>% 
#   summarise_at(colnames(.)[1:iters],
#                list(~ weighted.mean(., pop))) %>% ungroup()
# estimates1 = data.matrix(df.regions %>% select(-c(gender,year,gbd_region)))   
# 
# df.regions = unmetNeed %>% filter(ageD == "50-59") %>% group_by(gender, year, gbd_region) %>% select(-ageD,-country) %>% 
#   summarise_at(colnames(.)[1:iters],
#                list(~ weighted.mean(., pop))) %>% ungroup()
# estimates2 = data.matrix(df.regions %>% select(-c(gender,year,gbd_region)))   
# 
# estimates = estimates1 / (estimates1 + estimates2)
# df.summary = data.frame(mean=rowMeans(estimates), li=rowQuantiles(estimates,probs=.025), ui=rowQuantiles(estimates,probs=.975))
# df.summary = cbind(df.summary,df.regions %>% select(c(gender,year,gbd_region)))
# head(df.summary)
# df.summary$mean = round(df.summary$mean,2)
# df.summary$li = round(df.summary$li,2)
# df.summary$ui = round(df.summary$ui,2)
# write.csv(df.summary,"results.csv",row.names=F)
## country estimates
df.countries = metNeed %>% group_by(gender, year, country) %>% select(-ageD,-gbd_region) %>% 
  summarise_at(colnames(.)[1:iters], 
               list(~ weighted.mean(., pop))) %>% ungroup()
estimates1 = data.matrix(df.countries %>% select(-c(gender,year,country)))   
df.countries = unmetNeed %>% group_by(gender, year, country) %>% select(-ageD,-gbd_region) %>% 
  summarise_at(colnames(.)[1:iters], 
               list(~ weighted.mean(., pop))) %>% ungroup()
estimates2 = data.matrix(df.countries %>% select(-c(gender,year,country)))    
estimates.country = estimates1 / (estimates1 + estimates2)
df.country.summary = data.frame(mean=rowMeans(estimates.country), li=rowQuantiles(estimates.country,probs=.025), ui=rowQuantiles(estimates.country,probs=.975))
df.country.summary = cbind(df.country.summary,df.countries %>% select(c(country,gender,year)))
head(df.country.summary)
df.country.summary$mean = round(df.country.summary$mean,2)
df.country.summary$li = round(df.country.summary$li,2)
df.country.summary$ui = round(df.country.summary$ui,2)
write.csv(df.country.summary,"results-country.csv",row.names=F)
df.country.summary %>% filter(country == "United States of America")
## global estimates
df.world = metNeed %>% group_by(gender, year) %>% select(-ageD,-gbd_region,-country) %>% 
  summarise_at(colnames(.)[1:iters],
               list(~ weighted.mean(., pop))) %>% ungroup()
estimates1 = data.matrix(df.world %>% select(-c(gender,year)))   
df.world = unmetNeed %>% group_by(gender, year) %>% select(-ageD,-gbd_region,-country) %>% 
  summarise_at(colnames(.)[1:iters],
               list(~ weighted.mean(., pop))) %>% ungroup()
estimates2 = data.matrix(df.world %>% select(-c(gender,year)))   
estimates = estimates1 / (estimates1 + estimates2)
df.summary = data.frame(mean=rowMeans(estimates), li=rowQuantiles(estimates,probs=.025), ui=rowQuantiles(estimates,probs=.975))
df.summary = cbind(df.summary,df.world %>% select(c(gender,year)))
df.summary$mean = round(df.summary$mean,2)
df.summary$li = round(df.summary$li,2)
df.summary$ui = round(df.summary$ui,2)
write.csv(df.summary,"results-world.csv",row.names=F)


 