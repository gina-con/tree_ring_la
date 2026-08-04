library(glmmTMB)
library(splines)
library(dplyr)
#mean centring climate data and calculating quadratics
#####
fagus_dataset <- read.csv("Data/e_obs/climate_dataset_fagus.csv")

mean_centred_fagus_dataset <- data.frame()
fagus_populations <- unique(fagus_dataset$SiteID)

#mean centering to calculate within and among/between site effects
#among represents the average temperature at each site 
#within represents the deviation each year from the each site's average
for (i in 1:length(fagus_populations)){
  subset <- fagus_dataset %>% 
    filter(SiteID == fagus_populations[i])
  
  #vpd
  subset$AmongVPD <- mean(subset$Avg_VPD)#among is the mean of each site
  subset$WithinVPD <- subset$Avg_VPD-(mean(subset$Avg_VPD))#within is the deviation of each observation from the mean of each site
  subset$WithinVPDQuad4 <- (subset$Avg_VPD-(mean(subset$Avg_VPD)))^2 #quadratic calculated using equation 4 from Fay et al
  #equation 4 assumes that the effect of temperature varies among populations (which would be the case if locally adapted)
  subset$AmongVPDQuad <- mean(subset$Avg_VPD)^2
  
  #mean temp
  subset$AmongTemp <- mean(subset$Avg_meantemp)#among is the mean of each site
  subset$WithinTemp <- subset$Avg_meantemp-(mean(subset$Avg_meantemp))#within is the deviation of each observation from the mean of each site
  subset$WithinTempQuad4 <- (subset$Avg_meantemp-(mean(subset$Avg_meantemp)))^2 #quadratic calculated using equation 4 from Fay et al
  #equation 4 assumes that the effect of temperature varies among populations (which would be the case if locally adapted)
  subset$AmongTempQuad <- mean(subset$Avg_meantemp)^2
  
  #avg precip
  subset$AmongPrecip <- mean(subset$Avg_precip)#among is the mean max summer temperature of each site
  subset$WithinPrecip <- subset$Avg_precip-(mean(subset$Avg_precip))#within is the deviation of each observation from the mean of each site
  subset$WithinPrecipQuad4 <- (subset$Avg_precip-(mean(subset$Avg_precip)))^2 #quadratic calculated using equation 4 from Fay et al
  #equation 4 assumes that the effect of temperature varies among populations (which would be the case if locally adapted)
  subset$AmongPrecipQuad <- mean(subset$Avg_precip)^2
  
  mean_centred_fagus_dataset <- rbind(mean_centred_fagus_dataset, subset)
}

mean_centred_fagus_dataset <- read.csv("Data/e_obs/mean_centred_data_fagus.csv")

#calculating among deviation (deviation from global average)
mean_centred_fagus_dataset$AmongVPDDev<- mean_centred_fagus_dataset$AmongVPD - mean(mean_centred_fagus_dataset$AmongVPD)
mean_centred_fagus_dataset$AmongPrecipDev<- mean_centred_fagus_dataset$AmongPrecip - mean(mean_centred_fagus_dataset$AmongPrecip)
mean_centred_fagus_dataset$AmongTempDev<- mean_centred_fagus_dataset$AmongTemp - mean(mean_centred_fagus_dataset$AmongTemp)
#calculating quadratic (quadratic deviations from global average)
mean_centred_fagus_dataset$AmongVPDDevQuad<- mean_centred_fagus_dataset$AmongVPDDev^2
mean_centred_fagus_dataset$AmongPrecipDevQuad<- mean_centred_fagus_dataset$AmongPrecipDev^2
mean_centred_fagus_dataset$AmongTempDevQuad<- mean_centred_fagus_dataset$AmongTempDev^2
#calculating quadratic of original climate data for global optimum model
mean_centred_fagus_dataset$Avg_VPDQuad <- mean_centred_fagus_dataset$Avg_VPD^2
mean_centred_fagus_dataset$Avg_precipQuad<- mean_centred_fagus_dataset$Avg_precip^2
mean_centred_fagus_dataset$Avg_meantempQuad<- mean_centred_fagus_dataset$Avg_meantemp^2

#median centring year so that median year is 0, makes intercept easier to interpret
mean_centred_fagus_dataset <- mean_centred_fagus_dataset[,-c(1)]
mean_centred_fagus_dataset$Centred_year <- as.numeric(mean_centred_fagus_dataset$Year) - median(as.numeric(mean_centred_fagus_dataset$Year))

#excluding Spain sites as low confidence in accuracy of climate data; seems much colder (around 0-4 degrees)
#than likely for the region
red_fagus_dataset <- mean_centred_fagus_dataset %>% 
  filter(Country != 'Spain')

#null model with no climate drivers 
null_model <- glmmTMB(log1p(Ring.width)~Latitude+(1|Year)+(1|SiteID)+
                                             ns(Centred_year, df=3)+
                                             (1+ns(Centred_year, df=3)|TreeID)+
                                             (1|Centred_year:SiteID),
                                           data=red_fagus_dataset)

#dropping quadratics to check assumption of optimum climate value
no_quads <- glmmTMB(log1p(Ring.width)~AmongTempDev+WithinTemp+
                      AmongVPDDev+WithinVPD+
                      AmongPrecipDev+WithinPrecip+
                                             (WithinTemp*AmongTempDev)+
                                             Latitude+(1|Year)+(1|SiteID)+
                                             ns(Centred_year, df=3)+
                                             (1+ns(Centred_year, df=3)|TreeID)+
                                             (1|Centred_year:SiteID),
                                           data=red_fagus_dataset)
#dropping temperature quadratic
no_quadsTemp <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
                               AmongPrecipDevQuad+AmongVPDDevQuad+
                               WithinPrecip+WithinVPD+WithinTemp+
                               WithinPrecipQuad4+WithinVPDQuad4+
                               (AmongPrecipDev*WithinPrecip)+
                               (AmongVPDDev*WithinVPD)+
                               (AmongTempDev*WithinTemp)+
                               Latitude+
                               (1|Year)+(1|SiteID)+
                               ns(Centred_year, df=3)+
                               (1+ns(Centred_year, df=3)|TreeID)+
                               (1|Centred_year:SiteID),
                             data=red_fagus_dataset)
#dropping VPD quadratic
no_quadsVPD <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
                               AmongPrecipDevQuad+AmongTempDevQuad+
                               WithinPrecip+WithinVPD+WithinTemp+
                               WithinPrecipQuad4+WithinTempQuad4+
                               (AmongPrecipDev*WithinPrecip)+
                               (AmongVPDDev*WithinVPD)+
                               (AmongTempDev*WithinTemp)+
                               Latitude+
                               (1|Year)+(1|SiteID)+
                               ns(Centred_year, df=3)+
                               (1+ns(Centred_year, df=3)|TreeID)+
                               (1|Centred_year:SiteID),
                             data=red_fagus_dataset)
#dropping precipitation quadratic
no_quadsPrecip <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
                               AmongVPDDevQuad+AmongTempDevQuad+
                               WithinPrecip+WithinVPD+WithinTemp+
                               WithinVPDQuad4+WithinTempQuad4+
                               (AmongPrecipDev*WithinPrecip)+
                               (AmongVPDDev*WithinVPD)+
                               (AmongTempDev*WithinTemp)+
                               Latitude+
                               (1|Year)+(1|SiteID)+
                               ns(Centred_year, df=3)+
                               (1+ns(Centred_year, df=3)|TreeID)+
                               (1|Centred_year:SiteID),
                             data=red_fagus_dataset)

#global model to check splitting temporal and spatial adds value
global_model <- glmmTMB(log1p(Ring.width)~Avg_precip+Avg_meantemp+Avg_VPD+
                          WithinPrecip+WithinVPD+WithinTemp+
                          Avg_precipQuad+Avg_meantempQuad+Avg_VPDQuad+
                          WithinPrecipQuad4+WithinVPDQuad4+WithinTempQuad4+#left in as dummy
                          (Avg_precip*WithinPrecip)+
                          (Avg_VPD*WithinVPD)+
                          (Avg_meantemp*WithinTemp)+
                          Latitude+
                          (1|Year)+(1|SiteID)+
                          ns(Centred_year, df=3)+
                          (1+ns(Centred_year, df=3)|TreeID)+
                          (1|Centred_year:SiteID),
                        data=red_fagus_dataset)
#dropping quadratics
global_model_no_quads <- glmmTMB(log1p(Ring.width)~Avg_precip+Avg_meantemp+Avg_VPD+
                                   WithinPrecip+WithinVPD+WithinTemp+
                                   (Avg_precip*WithinPrecip)+
                                   (Avg_VPD*WithinVPD)+
                                   (Avg_meantemp*WithinTemp)+
                                   Latitude+
                                   (1|Year)+(1|SiteID)+
                                   ns(Centred_year, df=3)+
                                   (1+ns(Centred_year, df=3)|TreeID)+
                                   (1|Centred_year:SiteID),
                                 data=red_fagus_dataset)

summary(global_model)
summary(global_model_no_quads)


##final model
##latitude as fixed effect, year and siteID as random effect
#individual models for each climate driver 
#####
temperature_quad_model_lat_year <- glmmTMB(log1p(Ring.width)~AmongTempDev+WithinTemp+
                                             AmongTempDevQuad+WithinTempQuad4+
                                             (WithinTemp*AmongTempDev)+
                                             Latitude+(1|Year)+(1|SiteID)+
                                             ns(Centred_year, df=3)+
                                             (1+ns(Centred_year, df=3)|TreeID)+
                                             (1|Centred_year:SiteID),
                                           data=red_fagus_dataset)

VPD_quad_model_lat_year <- glmmTMB(log1p(Ring.width)~AmongVPDDev+WithinVPD+
                                     AmongVPDDevQuad+WithinVPDQuad4+
                                     (AmongVPDDev*WithinVPD)+
                                     Latitude+(1|Year)+(1|SiteID)+
                                     ns(Centred_year, df=3)+
                                     (1+ns(Centred_year, df=3)|TreeID)+
                                     (1|Centred_year:SiteID),
                                   data=red_fagus_dataset)

precip_quad_model_lat_year <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+WithinPrecip+
                                        AmongPrecipDevQuad+WithinPrecipQuad4+
                                        (AmongPrecipDev*WithinPrecip)+
                                        Latitude+(1|Year)+(1|SiteID)+
                                        ns(Centred_year, df=3)+
                                        (1+ns(Centred_year, df=3)|TreeID)+
                                        (1|Centred_year:SiteID),
                                      data=red_fagus_dataset)


#more complicated climate models 
#including all climatic variables
all_climate_model <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
                               AmongPrecipDevQuad+AmongVPDDevQuad+AmongTempDevQuad+
                               WithinPrecip+WithinVPD+WithinTemp+
                               WithinPrecipQuad4+WithinVPDQuad4+WithinTempQuad4+
                               (AmongPrecipDev*WithinPrecip)+
                               (AmongVPDDev*WithinVPD)+
                               (AmongTempDev*WithinTemp)+
                               Latitude+
                               (1|Year)+(1|SiteID)+
                               ns(Centred_year, df=3)+
                               (1+ns(Centred_year, df=3)|TreeID)+
                               (1|Centred_year:SiteID),
                             data=red_fagus_dataset)
summary(all_climate_model)
#combinations of different variables
#VPD + temp
VPD_temp_climate_model <- glmmTMB(log1p(Ring.width)~AmongVPDDev+AmongTempDev+
                                    AmongVPDDevQuad+AmongTempDevQuad+
                                    WithinVPD+WithinTemp+
                                    WithinVPDQuad4+WithinTempQuad4+
                                    (AmongVPDDev*WithinVPD)+
                                    (AmongTempDev*WithinTemp)+
                                    Latitude+
                                    (1|Year)+(1|SiteID)+
                                    ns(Centred_year, df=3)+
                                    (1+ns(Centred_year, df=3)|TreeID)+
                                    (1|Centred_year:SiteID),
                                  data=red_fagus_dataset)

#VPD + precip
VPD_precip_climate_model <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+
                                      AmongPrecipDevQuad+AmongVPDDevQuad+
                                      WithinPrecip+WithinVPD+
                                      WithinPrecipQuad4+WithinVPDQuad4+
                                      (AmongPrecipDev*WithinPrecip)+
                                      (AmongVPDDev*WithinVPD)+
                                      Latitude+
                                      (1|Year)+(1|SiteID)+
                                      ns(Centred_year, df=3)+
                                      (1+ns(Centred_year, df=3)|TreeID)+
                                      (1|Centred_year:SiteID),
                                    data=red_fagus_dataset)

#precip + temp
precip_temp_climate_model <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongTempDev+
                                       AmongPrecipDevQuad+AmongTempDevQuad+
                                       WithinPrecip+WithinTemp+
                                       WithinPrecipQuad4+WithinTempQuad4+
                                       (AmongPrecipDev*WithinPrecip)+
                                       (AmongTempDev*WithinTemp)+
                                       Latitude+
                                       (1|Year)+(1|SiteID)+
                                       ns(Centred_year, df=3)+
                                       (1+ns(Centred_year, df=3)|TreeID)+
                                       (1|Centred_year:SiteID),
                                     data=red_fagus_dataset)

#creating a dataframe to use in akaike.weights function 
AICdf_fagus <- matrix(nrow=14, ncol=5)
colnames(AICdf_fagus) <- c('Model', 'AIC', 'DeltaAIC', 'Weight', 'Species')

summary(null_model)
AICdf_fagus[1,1] <- c('Null model')
AICdf_fagus[1,2] <-  26134.6 #taken from that calculated by glmmTMB in summary

summary(no_quads)
AICdf_fagus[2,1] <- c('All climate variables, excluding quadratics')
AICdf_fagus[2,2] <-  26106.8 

summary(precip_quad_model_lat_year)
AICdf_fagus[3,1] <- c('Precipitation only')
AICdf_fagus[3,2] <-  26121.2 

summary(temperature_quad_model_lat_year)
AICdf_fagus[4,1] <- c('Mean temperature only')
AICdf_fagus[4,2] <- 26130.3

summary(VPD_quad_model_lat_year)
AICdf_fagus[5,1] <- c('VPD only')
AICdf_fagus[5,2] <- 26115.8

summary(all_climate_model)
AICdf_fagus[6,1] <- c('All climate variables')
AICdf_fagus[6,2] <- 26111.9

summary(VPD_temp_climate_model)
AICdf_fagus[7,1] <- c('VPD and mean temperature')
AICdf_fagus[7,2] <- 26114.3

summary(VPD_precip_climate_model)
AICdf_fagus[8,1] <- c('VPD and precipitation')
AICdf_fagus[8,2] <- 26115.4

summary(precip_temp_climate_model)
AICdf_fagus[9,1] <- c('Precipitation and mean temperature')
AICdf_fagus[9,2] <-  26121.9

summary(no_quadsTemp)
AICdf_fagus[10,1] <- c('All climate variables, excluding temperature quadratic')
AICdf_fagus[10,2] <-26108.2

summary(no_quadsVPD)
AICdf_fagus[11,1] <- c('All climate variables, excluding VPD quadratic')
AICdf_fagus[11,2] <-26110.2 

summary(no_quadsPrecip)
AICdf_fagus[12,1] <- c('All climate variables, excluding precipitation quadratic')
AICdf_fagus[12,2] <-26111.1

summary(global_model)
AICdf_fagus[13,1] <- c('Global optimum model')
AICdf_fagus[13,2] <-26111.9

summary(global_model_no_quads)
AICdf_fagus[14,1] <- c('Global optimum model, excluding quadratics')
AICdf_fagus[14,2] <- 26102.6 

#calculating delta AIC and weights
library(qpcR)
weights_fagus <- akaike.weights(as.numeric(AICdf_fagus[,2]))
AICdf_fagus[,3] <- weights_fagus$deltaAIC
AICdf_fagus[,4] <- weights_fagus$weights

AICdf_fagus[,5] <- "Fagus sylvatica"#adding species name for dataframe to create figure
write.csv(AICdf_fagus, "Figures/AIC_scores.csv")


##plotting 
library(dplyr)

###among quad fagus
#####
#skeleton dataset for prediction of VPD
#keeping everything fixed at median or at designated value apart from the among 
#deviation and among quad values so only thing that the only thing that varies
#is the among effect to show spatial relationship 
med_year <- median(red_fagus_dataset$Centred_year)
med_yr <- 2000
med_withinVPD <- median(red_fagus_dataset$WithinVPD)
med_withinVPDQuad <- med_withinVPD^2
MinAmongVPDDev <- min(red_fagus_dataset$AmongVPDDev)
MaxAmongVPDDev <- max(red_fagus_dataset$AmongVPDDev)
pred_amongVPDDev <- seq(MinAmongVPDDev, MaxAmongVPDDev+0.1, 0.1)#+0.1 to ensure that is capturing all data
pred_amongVPDDevQuad <- pred_amongVPDDev^2
tree_id <- as.character((red_fagus_dataset$TreeID[1])) 
site_id <- as.character((red_fagus_dataset$SiteID[1]))
med_lat <- red_fagus_dataset$Latitude[1]
med_amongTempDev <- median(red_fagus_dataset$AmongTempDev)
med_amongTempDevQuad <- med_amongTempDev^2
med_amongPrecipDev <- median(red_fagus_dataset$AmongPrecipDev)
med_amongPrecipDevQuad <- med_amongPrecipDev^2
med_withinTemp <- median(red_fagus_dataset$WithinTemp)
med_withinTempQuad <- med_withinTemp^2
med_withinPrecip <- median(red_fagus_dataset$WithinPrecip)
med_withinPrecipQuad <- med_withinPrecip^2

#creating the dataframes for the prediction
newdataAmongVPD <- data.frame(Year=rep(med_yr, length(pred_amongVPDDev)),
                              Centred_year=rep(med_year, length(pred_amongVPDDev)),
                              Latitude=rep(med_lat, length(pred_amongVPDDev)),
                              WithinVPD=rep(med_withinVPD, length(pred_amongVPDDev)),
                              WithinVPDQuad4=rep(med_withinVPDQuad, length(pred_amongVPDDev)),
                              TreeID=tree_id,
                              Year=rep(med_year, length(pred_amongVPDDev)),
                              SiteID=site_id,
                              AmongVPDDev=pred_amongVPDDev,
                              AmongVPDDevQuad = pred_amongVPDDevQuad,
                              AmongPrecipDev=rep(med_amongPrecipDev, length(pred_amongVPDDev)),
                              AmongPrecipDevQuad=rep(med_amongPrecipDevQuad, length(pred_amongVPDDev)),
                              WithinPrecip=rep(med_withinPrecip, length(pred_amongVPDDev)),
                              WithinPrecipQuad4=rep(med_withinPrecipQuad, length(pred_amongVPDDev)),
                              AmongTempDev=rep(med_amongTempDev, length(pred_amongVPDDev)),
                              AmongTempDevQuad=rep(med_amongTempDevQuad, length(pred_amongVPDDev)),
                              WithinTemp=rep(med_withinTemp, length(pred_amongVPDDev)),
                              WithinTempQuad4=rep(med_withinTempQuad, length(pred_amongVPDDev)))

#for prediction of temperature
med_year <- median(red_fagus_dataset$Centred_year)
med_yr <- 2000
med_withinTemp <- median(red_fagus_dataset$WithinTemp)
med_withinTempQuad <- med_withinTemp^2
MinAmongTempDev <- min(red_fagus_dataset$AmongTempDev)
MaxAmongTempDev <- max(red_fagus_dataset$AmongTempDev)
pred_amongTempDev <- seq(MinAmongTempDev, MaxAmongTempDev+1, 0.5)
pred_amongTempDevQuad <- pred_amongTempDev^2
tree_id <- as.character((red_fagus_dataset$TreeID[1])) 
site_id <- as.character((red_fagus_dataset$SiteID[1]))
med_lat <- red_fagus_dataset$Latitude[1]
med_amongVPDDev <- median(red_fagus_dataset$AmongVPDDev)
med_amongVPDDevQuad <- med_amongVPDDev^2
med_amongDevPrecip <- median(red_fagus_dataset$AmongPrecipDev)
med_amongPrecipDevQuad <- med_amongPrecipDev^2
med_withinVPD <- median(red_fagus_dataset$WithinVPD)
med_withinVPDquad <- med_withinVPD^2
med_withinPrecip <- median(red_fagus_dataset$WithinPrecip)
med_withinPrecipQuad <- med_withinPrecip^2

newdataAmongTemp <- data.frame(Year=rep(med_yr, length(pred_amongTempDev)),
                               Centred_year=rep(med_year, length(pred_amongTempDev)),
                               Latitude=rep(med_lat, length(pred_amongTempDev)),
                               WithinTemp=rep(med_withinTemp, length(pred_amongTempDev)),
                               WithinTempQuad4=rep(med_withinTempQuad, length(pred_amongTempDev)),
                               TreeID=rep(tree_id, length(pred_amongTempDev)),
                               SiteID=rep(site_id, length(pred_amongTempDev)),
                               AmongTempDev=pred_amongTempDev,
                               AmongTempDevQuad = pred_amongTempDevQuad,
                               AmongPrecipDev=rep(med_amongPrecipDev, length(pred_amongTempDev)),
                               AmongPrecipDevQuad=rep(med_amongPrecipDevQuad, length(pred_amongTempDev)),
                               WithinPrecip=rep(med_withinPrecip, length(pred_amongTempDev)),
                               WithinPrecipQuad4=rep(med_withinPrecipQuad, length(pred_amongTempDev)),
                               AmongVPDDev=rep(med_amongVPDDev, length(pred_amongTempDev)),
                               AmongVPDDevQuad=rep(med_amongVPDDevQuad, length(pred_amongTempDev)),
                               WithinVPD=rep(med_withinVPD, length(pred_amongTempDev)),
                               WithinVPDQuad4=rep(med_withinVPDQuad, length(pred_amongTempDev)))

#for prediction for precipitation
med_year <- median(red_fagus_dataset$Centred_year)
med_yr <- 2000
med_withinPrecip <- median(red_fagus_dataset$WithinPrecip)
med_withinPrecipQuad <- med_withinPrecip^2
MinAmongPrecipDev <- min(red_fagus_dataset$AmongPrecipDev)
MaxAmongPrecipDev <- max(red_fagus_dataset$AmongPrecipDev)
pred_amongPrecipDev <- seq(MinAmongPrecipDev, MaxAmongPrecipDev+0.2, 0.2)
pred_amongPrecipDevQuad <- pred_amongPrecipDev^2
tree_id <- as.character((red_fagus_dataset$TreeID[1])) 
site_id <- as.character((red_fagus_dataset$SiteID[1]))
med_lat <- red_fagus_dataset$Latitude[1]
med_amongVPDDev <- median(red_fagus_dataset$AmongVPDDev)
med_amongVPDDevQuad <- med_amongVPDDev^2
med_amongTempDev <- median(red_fagus_dataset$AmongTempDev)
med_amongTempDevQuad <- med_amongTempDev^2
med_withinVPD <- median(red_fagus_dataset$WithinVPD)
med_withinVPDQuad <- med_withinVPD^2
med_withinTemp <- median(red_fagus_dataset$WithinTemp)
med_withinTempQuad <- med_withinTemp^2


newdataAmongPrecip <- data.frame(Year=rep(med_yr, length(pred_amongPrecipDev)),
                                 Centred_year=rep(med_year, length(pred_amongPrecipDev)),
                                 Latitude=rep(med_lat, length(pred_amongPrecipDev)),
                                 WithinPrecip=rep(med_withinPrecip, length(pred_amongPrecipDev)),
                                 WithinPrecipQuad4=rep(med_withinPrecipQuad, length(pred_amongPrecipDev)),
                                 TreeID=tree_id,
                                 SiteID=site_id,
                                 AmongPrecipDev=pred_amongPrecipDev,
                                 AmongPrecipDevQuad = pred_amongPrecipDevQuad,
                                 AmongVPDDev=rep(med_amongVPDDev, length(pred_amongPrecipDev)),
                                 AmongVPDDevQuad=rep(med_amongVPDDevQuad, length(pred_amongPrecipDev)),
                                 WithinVPD=rep(med_withinVPD, length(pred_amongPrecipDev)),
                                 WithinVPDQuad4=rep(med_withinVPDQuad, length(pred_amongPrecipDev)),
                                 AmongTempDev=rep(med_amongTempDev, length(pred_amongPrecipDev)),
                                 AmongTempDevQuad=rep(med_amongTempDevQuad, length(pred_amongPrecipDev)),
                                 WithinTemp=rep(med_withinTemp, length(pred_amongPrecipDev)),
                                 WithinTempQuad4=rep(med_withinTempQuad, length(pred_amongPrecipDev)))

#predicting expected tree ring widths from the spatial relationship prediction dataframes created above
VPD_pred_among_fagus <-predict(all_climate_model, newdata = newdataAmongVPD, re.form=NULL, se.fit=TRUE)
temp_pred_among_fagus <-predict(all_climate_model, newdata = newdataAmongTemp, re.form=NULL, se.fit=TRUE)
precip_pred_among_fagus <-predict(all_climate_model, newdata = newdataAmongPrecip, re.form=NULL, se.fit=TRUE)

#adding predicted points to prediction dataframe and standard error for calculation of confidence intervals
newdataAmongVPD$Prediction <- as.numeric(VPD_pred_among_fagus$fit)
newdataAmongVPD$SE <- as.numeric(VPD_pred_among_fagus$se.fit)
newdataAmongTemp$Prediction  <- as.numeric(temp_pred_among_fagus$fit)
newdataAmongTemp$SE <- as.numeric(temp_pred_among_fagus$se.fit)
newdataAmongPrecip$Prediction  <- as.numeric(precip_pred_among_fagus$fit)
newdataAmongPrecip$SE <- as.numeric(precip_pred_among_fagus$se.fit)

#calculating prediction intervals of among slope
newdataAmongVPD$LCI <- newdataAmongVPD$Prediction-1.96*newdataAmongVPD$SE
newdataAmongVPD$HCI <- newdataAmongVPD$Prediction+1.96*newdataAmongVPD$SE
newdataAmongTemp$LCI <- newdataAmongTemp$Prediction-1.96*newdataAmongTemp$SE
newdataAmongTemp$HCI <- newdataAmongTemp$Prediction+1.96*newdataAmongTemp$SE
newdataAmongPrecip$LCI <- newdataAmongPrecip$Prediction-1.96*newdataAmongPrecip$SE
newdataAmongPrecip$HCI <- newdataAmongPrecip$Prediction+1.96*newdataAmongPrecip$SE

#calculating predictions for temporal/local/within slopes
######

#within slopes
#fixing among at specific values to create separate curves along the global gradient
#for mean temperature
amongmeantemps <- seq(MinAmongTempDev, MaxAmongTempDev, 2) 
within_points_df <- list()

#creating new dataframe where among is now fixed but within varies along a sequence ranging from
#the minimum to maximum deviation within the dataset
for(i in 1:length(amongmeantemps)){
  med_amongTempDev <- amongmeantemps[i]#fixing among effect to value i in sequence of 4 evenly spaced
  med_amongTempDevQuad <- med_amongTempDev^2
  
  MinWithinTemp <- min(red_fagus_dataset$WithinTemp)
  MaxWithinTemp <- max(red_fagus_dataset$WithinTemp)
  pred_withinTemp <- seq(MinWithinTemp, MaxWithinTemp, 0.5)#varying within
  pred_withinTempQuad <- pred_withinTemp^2
  
  newdatasubset <- data.frame(Year=rep(med_yr, length(pred_withinTemp)),
                              Centred_year=rep(med_year, length(pred_withinTemp)),
                              Latitude=rep(med_lat, length(pred_withinTemp)),
                              WithinTemp=pred_withinTemp,
                              WithinTempQuad4=pred_withinTempQuad,
                              TreeID=tree_id,
                              SiteID=site_id,
                              AmongTempDev=rep(med_amongTempDev, length(pred_withinTemp)),
                              AmongTempDevQuad=rep(med_amongTempDevQuad, length(pred_withinTemp)),
                              AmongPrecipDev=rep(med_amongPrecipDev, length(pred_withinTemp)),
                              AmongPrecipDevQuad=rep(med_amongPrecipDevQuad, length(pred_withinTemp)),
                              WithinPrecip=rep(med_withinPrecip, length(pred_withinTemp)),
                              WithinPrecipQuad4=rep(med_withinPrecipQuad, length(pred_withinTemp)),
                              AmongVPDDev=rep(med_amongVPDDev, length(pred_withinTemp)),
                              AmongVPDDevQuad=rep(med_amongVPDDevQuad, length(pred_withinTemp)),
                              WithinVPD=rep(med_withinVPD, length(pred_withinTemp)),
                              WithinVPDQuad4=rep(med_withinVPDQuad, length(pred_withinTemp)),
                              AvgAmongTemp = rep(unique(as.numeric(red_fagus_dataset$AvgAmongTemp)), length(pred_withinTemp)))
  #calculating deviation of within values from the global average for plotting
  newdatasubset$ActualAmongTemp <- newdatasubset$AvgAmongTemp+newdatasubset$AmongTempDev
  newdatasubset$ActualTemp <- newdatasubset$ActualAmongTemp+newdatasubset$WithinTemp
  newdatasubset$WithinGlobDev <- newdatasubset$ActualTemp-newdatasubset$AvgAmongTemp
  #predicting values for temporal curve i
  temp_pred_subset <-predict(all_climate_model, newdata = newdatasubset, re.form=NULL, se.fit=TRUE)
  temp_pred_subset_vals <- temp_pred_subset$fit
  #calculating confidence levels
  temp_pred_subset_HCI <- temp_pred_subset_vals+1.96*temp_pred_subset$se.fit
  temp_pred_subset_LCI <- temp_pred_subset_vals-1.96*temp_pred_subset$se.fit
  #creating dataframe with predicted values, confidence levels and climate values
  within_points_df[[i]] <- data.frame(Prediction=temp_pred_subset_vals, 
                                      AmongTempDev=newdatasubset$AmongTempDev,
                                      WithinTemp=newdatasubset$WithinTemp,
                                      WithinGlobDev=newdatasubset$WithinGlobDev,
                                      HCI=temp_pred_subset_HCI,
                                      LCI=temp_pred_subset_LCI)
}  

#moving dataframes from loop list into dataframe to be used to calculate curves from
within_curves1 <- within_points_df[[1]]
within_curves2 <- within_points_df[[2]]
within_curves3 <- within_points_df[[3]]
within_curves4 <- within_points_df[[4]]

#creating figure with a curve for among/spatial, and four curves for within/temporal
library(ggplot2)
library(cowplot)
fagus_temp <- ggplot(data=red_fagus_dataset, aes(x=AmongTemp, y=log1p(Ring.width)))+
  geom_line(data = data.frame(AmongTempDev = newdataAmongTemp$AmongTempDev, Ring.width = newdataAmongTemp$Prediction), aes(x = AmongTempDev, y = Ring.width), colour = "blue", linetype="dashed")+#spatial curve
  geom_line(data = data.frame(WithinGlobDev = within_curves1$WithinGlobDev, Ring.width = within_curves1$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype="dashed")+#temporal curve
  geom_line(data = data.frame(WithinGlobDev = within_curves1$WithinGlobDev, Ring.width = within_curves1$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+#higher prediction interval
  geom_line(data = data.frame(WithinGlobDev = within_curves1$WithinGlobDev, Ring.width = within_curves1$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+#lower prediction interval
  geom_line(data = data.frame(WithinGlobDev = within_curves2$WithinGlobDev, Ring.width = within_curves2$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype="dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves2$WithinGlobDev, Ring.width = within_curves2$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves2$WithinGlobDev, Ring.width = within_curves2$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves3$WithinGlobDev, Ring.width = within_curves3$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype="dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves3$WithinGlobDev, Ring.width = within_curves3$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves3$WithinGlobDev, Ring.width = within_curves3$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves4$WithinGlobDev, Ring.width = within_curves4$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype="dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves4$WithinGlobDev, Ring.width = within_curves4$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves4$WithinGlobDev, Ring.width = within_curves4$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  theme_cowplot()+
  xlab("Deviation from global average temperature (°C)")+
  ylab("Ring width (log+1)")+
  ggtitle("Fagus sylvatica")+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12), 
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))#+
  ylim(4, 8)+ #keeping axis consistent for figure containing all species
  xlim(-8, 6)

fagus_temp
#for VPD
amongVPDs <- seq(MinAmongVPDDev, MaxAmongVPDDev, 0.2) 
within_points_df <- list()

for(i in 1:length(amongVPDs)){
  med_amongVPDDev <- amongVPDs[i]
  med_amongVPDDevQuad <- med_amongVPDDev^2
  
  MinWithinVPD <- min(red_fagus_dataset$WithinVPD)
  MaxWithinVPD <- max(red_fagus_dataset$WithinVPD)
  pred_withinVPD <- seq(MinWithinVPD, MaxWithinVPD, 0.025)
  pred_withinVPDQuad <- pred_withinVPD^2
  
  med_withinTemp <- median(red_fagus_dataset$WithinTemp)
  med_withinTempQuad <- med_withinTemp^2
  med_amongTempDev <- median(red_fagus_dataset$AmongTempDev)
  med_amongTempDevQuad <- med_amongTempDev^2
  
  newdatasubset <- data.frame(Year=rep(med_yr, length(pred_withinVPD)),
                              Centred_year=rep(med_year, length(pred_withinVPD)),
                              Latitude=rep(med_lat, length(pred_withinVPD)),
                              WithinVPD=pred_withinVPD,
                              WithinVPDQuad4=pred_withinVPDQuad,
                              TreeID=tree_id,
                              SiteID=site_id,
                              AmongVPDDev=rep(med_amongVPDDev, length(pred_withinVPD)),
                              AmongVPDDevQuad=rep(med_amongVPDDevQuad, length(pred_withinVPD)),
                              AmongPrecipDev=rep(med_amongPrecipDev, length(pred_withinVPD)),
                              AmongPrecipDevQuad=rep(med_amongPrecipDevQuad, length(pred_withinVPD)),
                              WithinPrecip=rep(med_withinPrecip, length(pred_withinVPD)),
                              WithinPrecipQuad4=rep(med_withinPrecipQuad, length(pred_withinVPD)),
                              AmongTempDev=rep(med_amongTempDev, length(pred_withinVPD)),
                              AmongTempDevQuad=rep(med_amongTempDevQuad, length(pred_withinVPD)),
                              WithinTemp=rep(med_withinVPD, length(pred_withinVPD)),
                              WithinTempQuad4=rep(med_withinVPDQuad, length(pred_withinVPD)),
                              AvgAmongVPD = rep(unique(as.numeric(red_fagus_dataset$AvgAmongVPD)), length(pred_withinVPD)))
  
  newdatasubset$ActualAmongVPD <- newdatasubset$AvgAmongVPD+newdatasubset$AmongVPDDev
  newdatasubset$ActualVPD <- newdatasubset$ActualAmongVPD+newdatasubset$WithinVPD
  newdatasubset$WithinGlobDev <- newdatasubset$ActualVPD-newdatasubset$AvgAmongVPD
  
  VPD_pred_subset <-predict(all_climate_model, newdata = newdatasubset, re.form=NULL, se.fit=TRUE)
  VPD_pred_subset_vals <- VPD_pred_subset$fit
  VPD_pred_subset_HCI <- VPD_pred_subset_vals+1.96*VPD_pred_subset$se.fit
  VPD_pred_subset_LCI <- VPD_pred_subset_vals-1.96*VPD_pred_subset$se.fit
  within_points_df[[i]] <- data.frame(Prediction=VPD_pred_subset_vals, 
                                      AmongVPDDev=newdatasubset$AmongVPDDev,
                                      WithinVPD=newdatasubset$WithinVPD,
                                      WithinGlobDev=newdatasubset$WithinGlobDev,
                                      HCI=VPD_pred_subset_HCI,
                                      LCI=VPD_pred_subset_LCI)
}  

#moving dataframes from loop list into dataframe to be used to calculate curves from
within_curves1 <- within_points_df[[1]]
within_curves2 <- within_points_df[[2]]
within_curves3 <- within_points_df[[3]]
within_curves4 <- within_points_df[[4]]

#creating figure with a curve for among/spatial, and four curves for within/VPDoral
library(ggplot2)
library(cowplot)
fagus_VPD <- ggplot(data=red_fagus_dataset, aes(x=AmongVPD, y=log1p(Ring.width)))+
  geom_line(data = data.frame(AmongVPDDev = newdataAmongVPD$AmongVPDDev, Ring.width = newdataAmongVPD$Prediction), aes(x = AmongVPDDev, y = Ring.width), colour = "blue", linetype = "dotted")+
  geom_line(data = data.frame(WithinGlobDev = within_curves1$WithinGlobDev, Ring.width = within_curves1$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves1$WithinGlobDev, Ring.width = within_curves1$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+#Higher confidence interval
  geom_line(data = data.frame(WithinGlobDev = within_curves1$WithinGlobDev, Ring.width = within_curves1$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+#Lower confidence interval
  geom_line(data = data.frame(WithinGlobDev = within_curves2$WithinGlobDev, Ring.width = within_curves2$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves2$WithinGlobDev, Ring.width = within_curves2$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves2$WithinGlobDev, Ring.width = within_curves2$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves3$WithinGlobDev, Ring.width = within_curves3$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves3$WithinGlobDev, Ring.width = within_curves3$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves3$WithinGlobDev, Ring.width = within_curves3$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves4$WithinGlobDev, Ring.width = within_curves4$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves4$WithinGlobDev, Ring.width = within_curves4$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves4$WithinGlobDev, Ring.width = within_curves4$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  theme_cowplot()+
  xlab("Deviation from global average VPD (kPa)")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))#+
  #ylim(2,6)+
  #xlim(-0.5, 1)#putting on the same axis

fagus_VPD

#for precip
amongPrecips <- seq(MinAmongPrecipDev, MaxAmongPrecipDev, 1.5) 
within_points_df <- list()

for(i in 1:length(amongPrecips)){
  med_year <- median(red_fagus_dataset$Centred_year)
  med_yr <- 2000
  med_amongPrecipDev <- amongPrecips[i]
  med_amongPrecipDevQuad <- med_amongPrecipDev^2
  tree_id <- as.character((red_fagus_dataset$TreeID[1]))
  site_id <- as.character((red_fagus_dataset$SiteID[1]))
  med_lat <- red_fagus_dataset$Latitude[1]
  
  MinWithinPrecip <- min(red_fagus_dataset$WithinPrecip)
  MaxWithinPrecip <- max(red_fagus_dataset$WithinPrecip)
  pred_withinPrecip <- seq(MinWithinPrecip, MaxWithinPrecip, 0.25)
  pred_withinPrecipQuad <- pred_withinPrecip^2
  
  med_withinVPD <- median(red_fagus_dataset$WithinVPD)
  med_withinVPDquad <- med_withinVPD^2
  med_amongVPDDev <- median(red_fagus_dataset$AmongVPDDev)
  med_amongVPDDevQuad <- med_amongVPDDev^2
  
  med_withinTemp <- median(red_fagus_dataset$WithinTemp)
  med_withinTempQuad <- med_withinTemp^2
  med_amongTempDev <- median(red_fagus_dataset$AmongTempDev)
  med_amongTempDevQuad <- med_amongTempDev^2
  
  newdatasubset <- data.frame(Year=rep(med_yr, length(pred_withinPrecip)),
                              Centred_year=rep(med_year, length(pred_withinPrecip)),
                              Latitude=rep(med_lat, length(pred_withinPrecip)),
                              WithinPrecip=pred_withinPrecip,
                              WithinPrecipQuad4=pred_withinPrecipQuad,
                              AmongPrecipDev=rep(med_amongPrecipDev, length(pred_withinPrecip)),
                              AmongPrecipDevQuad=rep(med_amongPrecipDevQuad, length(pred_withinPrecip)),
                              TreeID=tree_id,
                              SiteID=site_id,
                              AmongTempDev=rep(med_amongTempDev, length(pred_withinPrecip)),
                              AmongTempDevQuad=rep(med_amongTempDevQuad, length(pred_withinPrecip)),
                              AmongVPDDev=rep(med_amongVPDDev, length(pred_withinPrecip)),
                              AmongVPDDevQuad=rep(med_amongVPDDevQuad, length(pred_withinPrecip)),
                              WithinVPD=rep(med_withinVPD, length(pred_withinPrecip)),
                              WithinVPDQuad4=rep(med_withinVPDQuad, length(pred_withinPrecip)),
                              WithinTemp=rep(med_withinTemp, length(pred_withinPrecip)),
                              WithinTempQuad4=rep(med_withinTempQuad, length(pred_withinPrecip)),
                              AvgAmongPrecip = rep(unique(as.numeric(red_fagus_dataset$AvgAmongPrecip)), length(pred_withinPrecip)))
  
  newdatasubset$ActualAmongPrecip <- newdatasubset$AvgAmongPrecip+newdatasubset$AmongPrecipDev
  newdatasubset$ActualPrecip <- newdatasubset$ActualAmongPrecip+newdatasubset$WithinPrecip
  newdatasubset$WithinGlobDev <- newdatasubset$ActualPrecip-newdatasubset$AvgAmongPrecip
  
  Precip_pred_subset <-predict(all_climate_model, newdata = newdatasubset, re.form=NULL, se.fit=TRUE)
  Precip_pred_subset_vals <- Precip_pred_subset$fit
  Precip_pred_subset_HCI <- Precip_pred_subset_vals+1.96*Precip_pred_subset$se.fit
  Precip_pred_subset_LCI <- Precip_pred_subset_vals-1.96*Precip_pred_subset$se.fit
  within_points_df[[i]] <- data.frame(Prediction=Precip_pred_subset_vals, 
                                      AmongPrecipDev=newdatasubset$AmongPrecipDev,
                                      WithinPrecip=newdatasubset$WithinPrecip,
                                      WithinGlobDev=newdatasubset$WithinGlobDev,
                                      HCI=Precip_pred_subset_HCI,
                                      LCI=Precip_pred_subset_LCI)
}  

within_curves1 <- within_points_df[[1]]
within_curves2 <- within_points_df[[2]]
within_curves3 <- within_points_df[[3]]
within_curves4 <- within_points_df[[4]]

library(ggplot2)
fagus_Precip <- ggplot(data=red_fagus_dataset, aes(x=AmongPrecip, y=log1p(Ring.width)))+
  geom_line(data = data.frame(AmongPrecipDev = newdataAmongPrecip$AmongPrecipDev, Ring.width = newdataAmongPrecip$Prediction), aes(x = AmongPrecipDev, y = Ring.width), colour = "blue", linetype = "dotted")+
  geom_line(data = data.frame(WithinGlobDev = within_curves1$WithinGlobDev, Ring.width = within_curves1$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves1$WithinGlobDev, Ring.width = within_curves1$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves1$WithinGlobDev, Ring.width = within_curves1$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves2$WithinGlobDev, Ring.width = within_curves2$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves2$WithinGlobDev, Ring.width = within_curves2$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves2$WithinGlobDev, Ring.width = within_curves2$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves3$WithinGlobDev, Ring.width = within_curves3$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves3$WithinGlobDev, Ring.width = within_curves3$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves3$WithinGlobDev, Ring.width = within_curves3$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves4$WithinGlobDev, Ring.width = within_curves4$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves4$WithinGlobDev, Ring.width = within_curves4$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves4$WithinGlobDev, Ring.width = within_curves4$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  theme_cowplot()+
  xlab("Deviation from global average precipitation (mm)")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))#+
  #ylim(4,6)+
  #xlim(-3, 6)

fagus_Precip

#grids
#all climate in one plot
library(cowplot)

plot_grid(fagus_temp, quercus_temp, betula_temp, 
          fagus_VPD, quercus_VPD, betula_VPD, 
          fagus_Precip, quercus_Precip, betula_Precip)

summary(all_climate_model)
summary(all_climate_model_quercus)
summary(all_climate_model_betula)

