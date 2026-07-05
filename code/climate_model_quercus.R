library(glmmTMB)
library(splines)
library(dplyr)
#mean centring climate data and calculating quadratics
#####
quercus_dataset <- read.csv("Data/e_obs/climate_dataset_quercus.csv")

quercus_populations <- unique(quercus_dataset$SiteID)

#scaling 
scaled_quercus_dataset <- quercus_dataset
scaled_quercus_dataset$Scaled_Elevation <- scale(scaled_quercus_dataset$Elevation)
scaled_quercus_dataset$Scaled_Age <- scale(scaled_quercus_dataset$Age)

#centering year
scaled_quercus_dataset$Centred_year <- as.numeric(scaled_quercus_dataset$Year) - median(as.numeric(scaled_quercus_dataset$Year))

#precipitation has data for all sites
mean_centred_quercus_dataset <- data.frame()
for (i in 1:length(quercus_populations)){
  subset <- scaled_quercus_dataset %>% 
    filter(SiteID == quercus_populations[i])
  #avg precip
  subset$AmongPrecip <- mean(subset$Avg_precip)#among is the mean max summer temperature of each site
  subset$WithinPrecip <- (subset$Avg_precip)-(mean(subset$Avg_precip))#within is the deviation of each observation from the mean of each site
  subset$WithinPrecipQuad4 <- ((subset$Avg_precip)-(mean(subset$Avg_precip)))^2 #quadratic calculated using equation 4 from Fay et al
  #equation 4 assumes that the effect of temperature varies among populations (which would be the case if locally adapted)
  subset$AmongPrecipQuad <- (mean(subset$Avg_precip))^2
  
  #vpd
  subset$AmongVPD <- mean(subset$Avg_VPD)#among is the mean max summer temperature of each site
  subset$WithinVPD <- (subset$Avg_VPD)-(mean(subset$Avg_VPD))#within is the deviation of each observation from the mean of each site
  subset$WithinVPDQuad4 <- ((subset$Avg_VPD)-(mean(subset$Avg_VPD)))^2 #quadratic calculated using equation 4 from Fay et al
  #equation 4 assumes that the effect of temperature varies among populations (which would be the case if locally adapted)
  subset$AmongVPDQuad <- (mean(subset$Avg_VPD))^2
  
  #mean temp
  subset$AmongTemp <- mean(subset$Avg_meantemp)#among is the mean max summer temperature of each site
  subset$WithinTemp <- (subset$Avg_meantemp)-(mean(subset$Avg_meantemp))#within is the deviation of each observation from the mean of each site
  subset$WithinTempQuad4 <- ((subset$Avg_meantemp)-(mean(subset$Avg_meantemp)))^2 #quadratic calculated using equation 4 from Fay et al
  #equation 4 assumes that the effect of temperature varies among populations (which would be the case if locally adapted)
  subset$AmongTempQuad <- (mean(subset$Avg_meantemp))^2
  
  mean_centred_quercus_dataset <- rbind(mean_centred_quercus_dataset, subset)
}

summary(mean_centred_quercus_dataset)

write.csv(mean_centred_quercus_dataset, "Data/e_obs/mean_centred_data_quercus.csv")
mean_centred_quercus_dataset <- read.csv("Data/e_obs/mean_centred_data_quercus.csv")

#ITQP9 and ITQP10 no VPD or temp data so removing trees from this site from the dataset
library(dplyr)
reduced_dataset_quercus <- mean_centred_quercus_dataset %>%
  filter(SiteID != "ITQP09") %>%
  filter(SiteID != "ITQP10")

#####
##latitude and random year 
#####
temperature_quad_model_lat_year_quercus <- glmmTMB(log1p(Ring.width)~AmongTemp+WithinTemp+
                                             AmongTempQuad+WithinTempQuad4+
                                               (AmongTemp*WithinTemp)+
                                             Latitude+(1|Year)+(1|SiteID)+
                                             ns(Centred_year, df=3)+
                                             (1+ns(Centred_year, df=3)|TreeID)+
                                             (1|Centred_year:SiteID),
                                           data=reduced_dataset_quercus)

#VPD model 
VPD_quad_model_lat_year_quercus <- glmmTMB(log1p(Ring.width)~AmongVPD+WithinVPD+
                                     AmongVPDQuad+WithinVPDQuad4+
                                       (AmongVPD*WithinVPD)+
                                     Latitude+(1|Year)+(1|SiteID)+
                                     ns(Centred_year, df=3)+
                                     (1+ns(Centred_year, df=3)|TreeID)+
                                     (1|Centred_year:SiteID),
                                   data=reduced_dataset_quercus)

#precip model 
precip_quad_model_lat_year_quercus <- glmmTMB(log1p(Ring.width)~AmongPrecip+WithinPrecip+
                                        AmongPrecipQuad+WithinPrecipQuad4+
                                          (AmongPrecip*WithinPrecip)+
                                        Latitude+(1|Year)+(1|SiteID)+
                                        ns(Centred_year, df=3)+
                                        (1+ns(Centred_year, df=3)|TreeID)+
                                        (1|Centred_year:SiteID),
                                      data=reduced_dataset_quercus)


#more complicated climate models 
all_climate_model_quercus <- glmmTMB(log1p(Ring.width)~AmongPrecip+AmongVPD+AmongTemp+
                               AmongPrecipQuad+AmongVPDQuad+AmongTempQuad+
                               WithinPrecip+WithinVPD+WithinTemp+
                               WithinPrecipQuad4+WithinVPDQuad4+WithinTempQuad4+
                               (AmongPrecip*WithinPrecip)+
                               (AmongVPD*WithinVPD)+
                               (AmongTemp*WithinTemp)+
                               Latitude+
                               (1|Year)+(1|SiteID)+
                               ns(Centred_year, df=3)+
                               (1+ns(Centred_year, df=3)|TreeID)+
                               (1|Centred_year:SiteID),
                             data=reduced_dataset_quercus)
summary(all_climate_model_quercus)
#VPD + temp
VPD_temp_climate_model_quercus <- glmmTMB(log1p(Ring.width)~AmongVPD+AmongTemp+
                                    AmongVPDQuad+AmongTempQuad+
                                    WithinVPD+WithinTemp+
                                    WithinVPDQuad4+WithinTempQuad4+
                                    (AmongVPD*WithinVPD)+
                                    (AmongTemp*WithinTemp)+
                                    Latitude+
                                    (1|Year)+(1|SiteID)+
                                    ns(Centred_year, df=3)+
                                    (1+ns(Centred_year, df=3)|TreeID)+
                                    (1|Centred_year:SiteID),
                                  data=reduced_dataset_quercus)

#VPD + precip
VPD_precip_climate_model_quercus <- glmmTMB(log1p(Ring.width)~AmongPrecip+AmongVPD+
                                      AmongPrecipQuad+AmongVPDQuad+
                                      WithinPrecip+WithinVPD+
                                      WithinPrecipQuad4+WithinVPDQuad4+
                                      (AmongPrecip*WithinPrecip)+
                                      (AmongVPD*WithinVPD)+
                                      Latitude+
                                      (1|Year)+(1|SiteID)+
                                      ns(Centred_year, df=3)+
                                      (1+ns(Centred_year, df=3)|TreeID)+
                                      (1|Centred_year:SiteID),
                                    data=reduced_dataset_quercus)

#precip + temp
precip_temp_climate_model_quercus <- glmmTMB(log1p(Ring.width)~AmongPrecip+AmongTemp+
                                       AmongPrecipQuad+AmongTempQuad+
                                       WithinPrecip+WithinTemp+
                                       WithinPrecipQuad4+WithinTempQuad4+
                                       (AmongPrecip*WithinPrecip)+
                                       (AmongTemp*WithinTemp)+
                                       Latitude+
                                       (1|Year)+(1|SiteID)+
                                       ns(Centred_year, df=3)+
                                       (1+ns(Centred_year, df=3)|TreeID)+
                                       (1|Centred_year:SiteID),
                                     data=reduced_dataset_quercus)


#the above model but with interactions between climate variables
#interaction between temperature and precipitation
#consequently have decided not to factor in interactions due to added complexity of interpretation
#and taking away from trying to distinguish which is the most important
temperature_precip_int_quad_model_lat_year_quercus <- glmmTMB(log1p(Ring.width)~AmongTemp+WithinTemp+
                                                        AmongTempQuad+WithinTempQuad4+
                                                        (AmongTemp*AmongPrecip)+
                                                        (AmongTempQuad*AmongPrecipQuad)+
                                                        (WithinTemp*WithinPrecip)+
                                                        (WithinTempQuad4*WithinPrecipQuad4)+
                                                        Latitude+(1|Year)+(1|SiteID)+
                                                        ns(Centred_year, df=3)+
                                                        (1+ns(Centred_year, df=3)|TreeID)+
                                                        (1|Centred_year:SiteID),
                                                      data=reduced_dataset_quercus)

#interaction between temperature and VPD 
temperature_VPD_int_quad_model_lat_year_quercus <- glmmTMB(log1p(Ring.width)~AmongTemp+WithinTemp+
                                                     AmongTempQuad+WithinTempQuad4+
                                                     (AmongTemp*AmongVPD)+
                                                     (AmongTempQuad*AmongVPDQuad)+
                                                     (WithinTemp*WithinVPD)+
                                                     (WithinTempQuad4*WithinVPDQuad4)+
                                                     Latitude+(1|Year)+(1|SiteID)+
                                                     ns(Centred_year, df=3)+
                                                     (1+ns(Centred_year, df=3)|TreeID)+
                                                     (1|Centred_year:SiteID),
                                                   data=reduced_dataset_quercus)

#interaction between precipitation and VPD 
precip_VPD_int_quad_model_lat_year_quercus <- glmmTMB(log1p(Ring.width)~AmongPrecip+WithinPrecip+
                                                AmongPrecipQuad+WithinPrecipQuad4+
                                                (AmongPrecip*AmongVPD)+
                                                (AmongPrecipQuad*AmongVPDQuad)+
                                                (WithinPrecip*WithinVPD)+
                                                (WithinPrecipQuad4*WithinVPDQuad4)+
                                                Latitude+(1|Year)+(1|SiteID)+
                                                ns(Centred_year, df=3)+
                                                (1+ns(Centred_year, df=3)|TreeID)+
                                                (1|Centred_year:SiteID),
                                              data=reduced_dataset_quercus)

#interaction between all?
all_int_quad_model_lat_year_quercus <- glmmTMB(log1p(Ring.width)~AmongPrecip+WithinPrecip+
                                                        AmongPrecipQuad+WithinPrecipQuad4+
                                                        (AmongPrecip*AmongVPD)+
                                                        (AmongPrecipQuad*AmongVPDQuad)+
                                                        (WithinPrecip*WithinVPD)+
                                                        (WithinPrecipQuad4*WithinVPDQuad4)+
                                                 (AmongTemp*AmongPrecip)+
                                                 (AmongTempQuad*AmongPrecipQuad)+
                                                 (WithinTemp*WithinPrecip)+
                                                 (WithinTempQuad4*WithinPrecipQuad4)+
                                                 (AmongTemp*AmongVPD)+
                                                 (AmongTempQuad*AmongVPDQuad)+
                                                 (WithinTemp*WithinVPD)+
                                                 (WithinTempQuad4*WithinVPDQuad4)+
                                                        Latitude+(1|Year)+(1|SiteID)+
                                                        ns(Centred_year, df=3)+
                                                        (1+ns(Centred_year, df=3)|TreeID)+
                                                        (1|Centred_year:SiteID),
                                                      data=reduced_dataset_quercus)

#####

#latitude only 
#####
temperature_quad_model_lat <- glmmTMB(log1p(Ring.width)~AmongTemp+WithinTemp+
                                        AmongTempQuad+WithinTempQuad4+
                                        Latitude+
                                        ns(Centred_year, df=3)+
                                        (1+ns(Centred_year, df=3)|TreeID)+
                                        (1|Centred_year:SiteID),
                                      data=mean_centred_quercus_dataset)

#lat onlyVPD model 
VPD_quad_model_lat <- glmmTMB(log1p(Ring.width)~AmongVPD+WithinVPD+
                                AmongVPDQuad+WithinVPDQuad4+
                                Latitude+
                                ns(Centred_year, df=3)+
                                (1+ns(Centred_year, df=3)|SiteID)+
                                (1+ns(Centred_year, df=3)|TreeID)+
                                (1|Centred_year:SiteID),
                              data=reduced_dataset_quercus)


#lat onlyprecip model 
precip_quad_model_lat <- glmmTMB(log1p(Ring.width)~AmongPrecip+WithinPrecip+
                                   AmongPrecipQuad+WithinPrecipQuad4+
                                   Latitude+
                                   ns(Centred_year, df=3)+
                                   (1+ns(Centred_year, df=3)|SiteID)+
                                   (1+ns(Centred_year, df=3)|TreeID)+
                                   (1|Centred_year:SiteID),
                                 data=reduced_dataset_quercus)
######

#latitude, elev, year
#####
temperature_quad_model_lat_elev_yr <- glmmTMB(log1p(Ring.width)~AmongTemp+WithinTemp+
                                                AmongTempQuad+WithinTempQuad4+
                                                Latitude+Elevation+(1|Year)+
                                                ns(Centred_year, df=3)+
                                                (1+ns(Centred_year, df=3)|SiteID)+
                                                (1+ns(Centred_year, df=3)|TreeID)+
                                                (1|Centred_year:SiteID),
                                              data=mean_centred_quercus_dataset)

#Elevation onlyVPD model 
VPD_quad_model_lat_elev_yr <- glmmTMB(log1p(Ring.width)~AmongVPD+WithinVPD+
                                        AmongVPDQuad+WithinVPDQuad4+
                                        Elevation+Latitude+(1|Year)+
                                        ns(Centred_year, df=3)+
                                        (1+ns(Centred_year, df=3)|SiteID)+
                                        (1+ns(Centred_year, df=3)|TreeID)+
                                        (1|Centred_year:SiteID),
                                      data=mean_centred_quercus_dataset)

#Elevation onlyprecip model 
precip_quad_model_lat_elev_yr <- glmmTMB(log1p(Ring.width)~AmongPrecip+WithinPrecip+
                                           AmongPrecipQuad+WithinPrecipQuad4+
                                           Elevation+Latitude+(1|Year)+
                                           ns(Centred_year, df=3)+
                                           (1+ns(Centred_year, df=3)|SiteID)+
                                           (1+ns(Centred_year, df=3)|TreeID)+
                                           (1|Centred_year:SiteID),
                                         data=mean_centred_quercus_dataset)
#####

#baseline mean temp model
######
temperature_quad_model <- glmmTMB(log1p(Ring.width)~AmongTemp+WithinTemp+
                                    AmongTempQuad+WithinTempQuad4+
                                    ns(Centred_year, df=3)+
                                    (1+ns(Centred_year, df=3)|SiteID)+
                                    (1+ns(Centred_year, df=3)|TreeID)+
                                    (1|Centred_year:SiteID),
                                  data=mean_centred_quercus_dataset)


#baseline VPD model 
VPD_quad_model <- glmmTMB(log1p(Ring.width)~AmongVPD+WithinVPD+
                            AmongVPDQuad+WithinVPDQuad4+
                            ns(Centred_year, df=3)+
                            (1+ns(Centred_year, df=3)|SiteID)+
                            (1+ns(Centred_year, df=3)|TreeID)+
                            (1|Centred_year:SiteID),
                          REML=FALSE,
                          data=mean_centred_quercus_dataset)

#baseline precip model 
precip_quad_model <- glmmTMB(log1p(Ring.width)~AmongPrecip+WithinPrecip+
                               AmongPrecipQuad+WithinPrecipQuad4+
                               ns(Centred_year, df=3)+
                               (1+ns(Centred_year, df=3)|SiteID)+
                               (1+ns(Centred_year, df=3)|TreeID)+
                               (1|Centred_year:SiteID),
                             REML=FALSE,
                             data=mean_centred_quercus_dataset)
######

#AIC vals 
summary(precip_quad_model)
#AIC: 
summary(temperature_quad_model)
#AIC: 
summary(VPD_quad_model)
#AIC: 
summary(precip_quad_model_lat)
#AIC: 
summary(temperature_quad_model_lat)
#AIC:
summary(VPD_quad_model_lat)
#AIC: 
summary(precip_quad_model_lat_elev_yr)
#AIC: 
summary(temperature_quad_model_lat_elev_yr)
#AIC: 
summary(VPD_quad_model_lat_elev_yr)


##final model
#AIC: 
AICdf <- matrix(nrow=7, ncol=4)
colnames(AICdf) <- c('Model', 'AIC', 'DeltaAIC', 'Weight')

summary(precip_quad_model_lat_year_quercus)
AICdf[1,1] <- c('Precipitation only')
AICdf[1,2] <- 12084.9
AICdf[1,3] <- 45.5
AICdf[1,4] <- 1.317631e-10

summary(temperature_quad_model_lat_year_quercus)
AICdf[2,1] <- c('Mean temperature only')
AICdf[2,2] <- 12159.3
AICdf[2,3] <- 119.9
AICdf[2,4] <- 9.205326e-27

summary(VPD_quad_model_lat_year_quercus)
AICdf[3,1] <- c('VPD only')
AICdf[3,2] <- 12091.2
AICdf[3,3] <- 51.8
AICdf[3,4] <- 5.646331e-12

summary(all_climate_model_quercus)
AICdf[4,1] <- c('All climate variables')
AICdf[4,2] <- 12039.4
AICdf[4,3] <- 0.0
AICdf[4,4] <- 9.999848e-01


summary(VPD_temp_climate_model_quercus)
AICdf[5,1] <- c('VPD and mean temperature')
AICdf[5,2] <- 12072.5
AICdf[5,3] <- 33.1
AICdf[5,4] <- 6.492616e-08


summary(VPD_precip_climate_model_quercus)
AICdf[6,1] <- c('VPD and precipitation')
AICdf[6,2] <- 12061.6
AICdf[6,3] <- 22.2
AICdf[6,4] <- 1.511209e-05


summary(precip_temp_climate_model_quercus)
AICdf[7,1] <- c('Precipitation and mean temperature')
AICdf[7,2] <- 12072.9 
AICdf[7,3] <- 33.5
AICdf[7,4] <- 5.315704e-08

#calculating weighted AIC 
library(qpcR)

#had previously assigned the AIC to vectors for each model and then inputted them directly into 
#akaike.weights and manually created a dataframe including the calculated delta AIC and weights
akaike.weights(c(precipAIC, tempAIC, VPDAIC, allAIC, VPDTempAIC, VPDPrecipAIC, TempPrecipAIC))

#interactions very complex to untangle therefore have chosen to focus on independent models only
summary(precip_VPD_int_quad_model_lat_year_quercus)
#AIC: 12069.4
summary(temperature_precip_int_quad_model_lat_year_quercus)
#AIC: 12088.7
summary(temperature_VPD_int_quad_model_lat_year_quercus)
#AIC: 12067.4
summary(all_int_quad_model_lat_year_quercus)
#AIC: 12039.6