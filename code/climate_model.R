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
  
  #avg precip
  subset$AmongPrecip <- mean(subset$Avg_precip)#among is the mean max summer temperature of each site
  subset$WithinPrecip <- (subset$Avg_precip)-(mean(subset$Avg_precip))#within is the deviation of each observation from the mean of each site
  subset$WithinPrecipQuad4 <- ((subset$Avg_precip)-(mean(subset$Avg_precip)))^2 #quadratic calculated using equation 4 from Fay et al
  #equation 4 assumes that the effect of temperature varies among populations (which would be the case if locally adapted)
  subset$AmongPrecipQuad <- (mean(subset$Avg_precip))^2
  
  mean_centred_fagus_dataset <- rbind(mean_centred_fagus_dataset, subset)
}

#median centring year so that median year is 0, makes intercept easier to interpret
mean_centred_fagus_dataset$Centred_year <- as.numeric(mean_centred_fagus_dataset$Year) - median(as.numeric(mean_centred_fagus_dataset$Year))
write.csv(mean_centred_fagus_dataset, "Data/e_obs/mean_centred_data_fagus.csv")

#climate modelling
mean_centred_fagus_dataset <- read.csv("Data/e_obs/mean_centred_data_fagus.csv")

#excluding Spain sites as low confidence in accuracy of climate data; seems much colder (around 0-4 degrees)
#than likely for the region
red_fagus_dataset <- mean_centred_fagus_dataset %>% 
  filter(Country != 'Spain')


#elevation mean temp model
#consequently decided to leave elevation out of the model due to the entanglement with climate
#and thus difficulty in disentangling the two.
#####
temperature_quad_model_elev <- glmmTMB(log1p(Ring.width)~AmongTemp+WithinTemp+
                               AmongTempQuad+WithinTempQuad4+
                                 Elevation+
                               ns(Centred_year, df=3)+
                               (1+ns(Centred_year, df=3)|SiteID)+
                               (1+ns(Centred_year, df=3)|TreeID)+
                               (1|Centred_year:SiteID),
                             data=mean_centred_fagus_dataset)

#Elevation onlyVPD model 
VPD_quad_model_elev <- glmmTMB(log1p(Ring.width)~AmongVPD+WithinVPD+
                                    AmongVPDQuad+WithinVPDQuad4+
                            Elevation+
                                    ns(Centred_year, df=3)+
                                    (1+ns(Centred_year, df=3)|SiteID)+
                                    (1+ns(Centred_year, df=3)|TreeID)+
                                    (1|Centred_year:SiteID),
                            REML=FALSE,
                                  data=mean_centred_fagus_dataset)

#Elevation onlyprecip model 
precip_quad_model_elev <- glmmTMB(log1p(Ring.width)~AmongPrecip+WithinPrecip+
                                    AmongPrecipQuad+WithinPrecipQuad4+
                                    Elevation+
                                    ns(Centred_year, df=3)+
                                    (1+ns(Centred_year, df=3)|SiteID)+
                                    (1+ns(Centred_year, df=3)|TreeID)+
                                    (1|Centred_year:SiteID),
                                  REML=FALSE,
                                  data=mean_centred_fagus_dataset)

#####

##final model
##latitude as fixed effect, year and siteID as random effect
#individual models for each climate driver 
#models with multiple climate drivers 
#####
temperature_quad_model_lat_year <- glmmTMB(log1p(Ring.width)~AmongTemp+WithinTemp+
                                         AmongTempQuad+WithinTempQuad4+
                                           (WithinTemp*AmongTemp)+
                                         Latitude+(1|Year)+(1|SiteID)+
                                         ns(Centred_year, df=3)+
                                         (1+ns(Centred_year, df=3)|TreeID)+
                                         (1|Centred_year:SiteID),
                                       data=red_fagus_dataset)

VPD_quad_model_lat_year <- glmmTMB(log1p(Ring.width)~AmongVPD+WithinVPD+
                                 AmongVPDQuad+WithinVPDQuad4+
                                   (AmongVPD*WithinVPD)+
                                 Latitude+(1|Year)+(1|SiteID)+
                                 ns(Centred_year, df=3)+
                                 (1+ns(Centred_year, df=3)|TreeID)+
                                 (1|Centred_year:SiteID),
                               data=red_fagus_dataset)

precip_quad_model_lat_year <- glmmTMB(log1p(Ring.width)~AmongPrecip+WithinPrecip+
                                    AmongPrecipQuad+WithinPrecipQuad4+
                                      (AmongPrecip*WithinPrecip)+
                                    Latitude+(1|Year)+(1|SiteID)+
                                    ns(Centred_year, df=3)+
                                    (1+ns(Centred_year, df=3)|TreeID)+
                                    (1|Centred_year:SiteID),
                                  data=red_fagus_dataset)


#more complicated climate models 
all_climate_model <- glmmTMB(log1p(Ring.width)~AmongPrecip+AmongVPD+AmongTemp+
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
                             data=red_fagus_dataset)
summary(all_climate_model)
#VPD + temp
VPD_temp_climate_model <- glmmTMB(log1p(Ring.width)~AmongVPD+AmongTemp+
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
                             data=red_fagus_dataset)

#VPD + precip
VPD_precip_climate_model <- glmmTMB(log1p(Ring.width)~AmongPrecip+AmongVPD+
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
                             data=red_fagus_dataset)

#precip + temp
precip_temp_climate_model <- glmmTMB(log1p(Ring.width)~AmongPrecip+AmongTemp+
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
                             data=red_fagus_dataset)

#the above model but with interactions between climate variables
#interactions difficult to interpret so consequently removed from final analysis
#####
#interaction between temperature and precipitation
temperature_precip_int_quad_model_lat_year <- glmmTMB(log1p(Ring.width)~AmongTemp+WithinTemp+
                                             AmongTempQuad+WithinTempQuad4+
                                               AmongTemp*AmongPrecip+
                                               AmongTempQuad*AmongPrecipQuad+
                                               WithinTemp*WithinPrecip+
                                               WithinTempQuad4*WithinPrecipQuad4+
                                             Latitude+(1|Year)+(1|SiteID)+
                                             ns(Centred_year, df=3)+
                                             (1+ns(Centred_year, df=3)|TreeID)+
                                             (1|Centred_year:SiteID),
                                           data=mean_centred_fagus_dataset)

#interaction between temperature and VPD 
temperature_VPD_int_quad_model_lat_year <- glmmTMB(log1p(Ring.width)~AmongTemp+WithinTemp+
                                                        AmongTempQuad+WithinTempQuad4+
                                                        (AmongTemp*AmongVPD)+
                                                        (AmongTempQuad*AmongVPDQuad)+
                                                        (WithinTemp*WithinVPD)+
                                                        (WithinTempQuad4*WithinVPDQuad4)+
                                                        Latitude+(1|Year)+(1|SiteID)+
                                                        ns(Centred_year, df=3)+
                                                        (1+ns(Centred_year, df=3)|TreeID)+
                                                        (1|Centred_year:SiteID),
                                                      data=mean_centred_fagus_dataset)

#interaction between precipitation and VPD 
precip_VPD_int_quad_model_lat_year <- glmmTMB(log1p(Ring.width)~AmongPrecip+WithinPrecip+
                                                     AmongPrecipQuad+WithinPrecipQuad4+
                                                     (AmongPrecip*AmongVPD)+
                                                     (AmongPrecipQuad*AmongVPDQuad)+
                                                     (WithinPrecip*WithinVPD)+
                                                     (WithinPrecipQuad4*WithinVPDQuad4)+
                                                     Latitude+(1|Year)+(1|SiteID)+
                                                     ns(Centred_year, df=3)+
                                                     (1+ns(Centred_year, df=3)|TreeID)+
                                                     (1|Centred_year:SiteID),
                                                   data=mean_centred_fagus_dataset)

#interaction between all
all_int_quad_model_lat_year <- glmmTMB(log1p(Ring.width)~AmongPrecip+WithinPrecip+
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
                                               data=mean_centred_fagus_dataset)
#####

#latitude only 
#####
temperature_quad_model_lat <- glmmTMB(log1p(Ring.width)~AmongTemp+WithinTemp+
                                                AmongTempQuad+WithinTempQuad4+
                                                Latitude+
                                                ns(Centred_year, df=3)+
                                                (1+ns(Centred_year, df=3)|SiteID)+
                                                (1+ns(Centred_year, df=3)|TreeID)+
                                                (1|Centred_year:SiteID),
                                              data=mean_centred_fagus_dataset)
#lat onlyVPD model 
VPD_quad_model_lat <- glmmTMB(log1p(Ring.width)~AmongVPD+WithinVPD+
                                 AmongVPDQuad+WithinVPDQuad4+
                                 Latitude+
                                 ns(Centred_year, df=3)+
                                 (1+ns(Centred_year, df=3)|SiteID)+
                                 (1+ns(Centred_year, df=3)|TreeID)+
                                 (1|Centred_year:SiteID),
                               data=mean_centred_fagus_dataset)

#lat onlyprecip model 
precip_quad_model_lat <- glmmTMB(log1p(Ring.width)~AmongPrecip+WithinPrecip+
                                    AmongPrecipQuad+WithinPrecipQuad4+
                                    Latitude+
                                    ns(Centred_year, df=3)+
                                    (1+ns(Centred_year, df=3)|SiteID)+
                                    (1+ns(Centred_year, df=3)|TreeID)+
                                    (1|Centred_year:SiteID),
                                  data=mean_centred_fagus_dataset)

######



#latitude, elev, year

#lat elev year

#Latitude, elevation and year effects - consequently left elevation out due to correlation with climate
#####
temperature_quad_model_lat_elev_yr <- glmmTMB(log1p(Ring.width)~AmongTemp+WithinTemp+
                                         AmongTempQuad+WithinTempQuad4+
                                         Latitude+Elevation+(1|Year)+
                                         ns(Centred_year, df=3)+
                                         (1+ns(Centred_year, df=3)|SiteID)+
                                         (1+ns(Centred_year, df=3)|TreeID)+
                                         (1|Centred_year:SiteID),
                                       data=mean_centred_fagus_dataset)

#Elevation onlyVPD model 
VPD_quad_model_lat_elev_yr <- glmmTMB(log1p(Ring.width)~AmongVPD+WithinVPD+
                                 AmongVPDQuad+WithinVPDQuad4+
                                 Elevation+Latitude+(1|Year)+
                                 ns(Centred_year, df=3)+
                                 (1+ns(Centred_year, df=3)|SiteID)+
                                 (1+ns(Centred_year, df=3)|TreeID)+
                                 (1|Centred_year:SiteID),
                               data=mean_centred_fagus_dataset)

#Elevation onlyprecip model 
precip_quad_model_lat_elev_yr <- glmmTMB(log1p(Ring.width)~AmongPrecip+WithinPrecip+
                                    AmongPrecipQuad+WithinPrecipQuad4+
                                    Elevation+Latitude+(1|Year)+
                                    ns(Centred_year, df=3)+
                                    (1+ns(Centred_year, df=3)|SiteID)+
                                    (1+ns(Centred_year, df=3)|TreeID)+
                                    (1|Centred_year:SiteID),
                                  data=mean_centred_fagus_dataset)

##elevation and random year 
temperature_quad_model_elev_year <- glmmTMB(log1p(Ring.width)~AmongTemp+WithinTemp+
                                              AmongTempQuad+WithinTempQuad4+
                                              Elevation+(1|Year)+
                                              ns(Centred_year, df=3)+
                                              (1+ns(Centred_year, df=3)|SiteID)+
                                              (1+ns(Centred_year, df=3)|TreeID)+
                                              (1|Centred_year:SiteID),
                                            data=mean_centred_fagus_dataset)

#Elevation onlyVPD model 
VPD_quad_model_elev_year <- glmmTMB(log1p(Ring.width)~AmongVPD+WithinVPD+
                                      AmongVPDQuad+WithinVPDQuad4+
                                      Elevation+(1|Year)+
                                      ns(Centred_year, df=3)+
                                      (1+ns(Centred_year, df=3)|SiteID)+
                                      (1+ns(Centred_year, df=3)|TreeID)+
                                      (1|Centred_year:SiteID),
                                    data=mean_centred_fagus_dataset)


#Elevation onlyprecip model
#didn't converge 
precip_quad_model_elev_year <- glmmTMB(log1p(Ring.width)~AmongPrecip+WithinPrecip+
                                         AmongPrecipQuad+WithinPrecipQuad4+
                                         Elevation+(1|Year)+
                                         ns(Centred_year, df=3)+
                                         (1+ns(Centred_year, df=3)|SiteID)+
                                         (1+ns(Centred_year, df=3)|TreeID)+
                                         (1|Centred_year:SiteID),
                                       data=mean_centred_fagus_dataset)
#####

#climate modelling 
#baseline mean temp model
#####
temperature_quad_model <- glmmTMB(log1p(Ring.width)~AmongTemp+WithinTemp+
                                    AmongTempQuad+WithinTempQuad4+
                                    ns(Centred_year, df=3)+
                                    (1+ns(Centred_year, df=3)|TreeID)+
                                    (1|Centred_year:SiteID),
                                  data=mean_centred_fagus_dataset)


#baseline VPD model 
VPD_quad_model <- glmmTMB(log1p(Ring.width)~AmongVPD+WithinVPD+
                            AmongVPDQuad+WithinVPDQuad4+
                            ns(Centred_year, df=3)+
                            (1+ns(Centred_year, df=3)|SiteID)+
                            (1+ns(Centred_year, df=3)|TreeID)+
                            (1|Centred_year:SiteID),
                          REML=FALSE,
                          data=mean_centred_fagus_dataset)

#baseline precip model 
precip_quad_model <- glmmTMB(log1p(Ring.width)~AmongPrecip+WithinPrecip+
                               AmongPrecipQuad+WithinPrecipQuad4+
                               ns(Centred_year, df=3)+
                               (1+ns(Centred_year, df=3)|SiteID)+
                               (1+ns(Centred_year, df=3)|TreeID)+
                               (1|Centred_year:SiteID),
                             REML=FALSE,
                             data=mean_centred_fagus_dataset)
#####

#checking AICs for each model as calculated by glmmTMB 
#####
summary(precip_quad_model)
#AIC: 46653
summary(temperature_quad_model)
#AIC: 46653
summary(VPD_quad_model)
#AIC: 46657
summary(precip_quad_model_lat)
#AIC: NA, didn't converge 
summary(temperature_quad_model_lat)
#AIC:46580
summary(VPD_quad_model_lat)
#AIC: 46559

##final model
summary(temperature_quad_model_lat_year)
#AIC: 44376.4
summary(VPD_quad_model_lat_year) #VPD provides best fit to data
#AIC: 44351.8
summary(precip_quad_model_lat_year)
#AIC: 44363.0
summary(all_climate_model)
#AIC: 44336.2
summary(VPD_temp_climate_model)
#AIC: 44343.0
summary(precip_temp_climate_model)
#AIC: 44358.3
summary(VPD_precip_climate_model)
#AIC: 44346.5

#creating a dataframe to use in akaike.weights function 
AICdf_fagus <- matrix(nrow=7, ncol=4)
colnames(AICdf_fagus) <- c('Model', 'AIC', 'DeltaAIC', 'Weight')

summary(precip_quad_model_lat_year_quercus)
AICdf_fagus[1,1] <- c('Precipitation only')
AICdf_fagus[1,2] <- 44363.0

summary(temperature_quad_model_lat_year_quercus)
AICdf_fagus[2,1] <- c('Mean temperature only')
AICdf_fagus[2,2] <- 44376.4

summary(VPD_quad_model_lat_year_quercus)
AICdf_fagus[3,1] <- c('VPD only')
AICdf_fagus[3,2] <- 44351.8

summary(all_climate_model_quercus)
AICdf_fagus[4,1] <- c('All climate variables')
AICdf_fagus[4,2] <- 44336.2


summary(VPD_temp_climate_model_quercus)
AICdf_fagus[5,1] <- c('VPD and mean temperature')
AICdf_fagus[5,2] <- 44343.0


summary(VPD_precip_climate_model_quercus)
AICdf_fagus[6,1] <- c('VPD and precipitation')
AICdf_fagus[6,2] <- 44346.5


summary(precip_temp_climate_model_quercus)
AICdf_fagus[7,1] <- c('Precipitation and mean temperature')
AICdf_fagus[7,2] <- 44358.3

#calculating delta AIC and weights
library(qpcR)
weights_fagus <- akaike.weights(as.numeric(AICdf_fagus[,2]))
AICdf_fagus[,3] <- weights_fagus$deltaAIC
AICdf_fagus[,4] <- weights_fagus$weights

#interactions very complex to untangle therefore have chosen to focus on independent models only
summary(precip_VPD_int_quad_model_lat_year)
#AIC: 46667.6
summary(temperature_precip_int_quad_model_lat_year)
#AIC: 46686.2
summary(temperature_VPD_int_quad_model_lat_year)
#AIC: 46668.7
summary(all_int_quad_model_lat_year)
#AIC: 46641.3