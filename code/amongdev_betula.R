library(dplyr)
#mean centring climate data and calculating quadratics
#####
betula_dataset <- read.csv("Data/e_obs/climate_dataset_betula.csv")

#these sites had no rel hum readings so VPD could not be calculated and therefore
#excluding from dataset

mean_centred_betula_dataset <- data.frame()
betula_populations <- unique(betula_dataset$SiteID)
betula_populations[21]
betula_populations <- betula_populations[-c(13,20,21)]

#mean centering to calculate within and among/between site effects
#among represents the average temperature at each site 
#within represents the deviation each year from the each site's average
for (i in 1:length(betula_populations)){
  subset <- betula_dataset %>% 
    filter(SiteID == betula_populations[i])
  
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
  
  mean_centred_betula_dataset<- rbind(mean_centred_betula_dataset, subset)
}

#median centring year so that median year is 0, makes intercept easier to interpret
mean_centred_betula_dataset <- mean_centred_betula_dataset[,-c(1)]
mean_centred_betula_dataset$Centred_year <- as.numeric(mean_centred_betula_dataset$Year) - median(as.numeric(mean_centred_betula_dataset$Year))
write.csv(mean_centred_betula_dataset, "Data/e_obs/mean_centred_data_betula.csv")

#climate modelling
mean_centred_betula_dataset <- read.csv("Data/e_obs/mean_centred_data_betula.csv")

#calculating average among (global average)
mean_centred_betula_dataset$AvgAmongVPD <- mean(mean_centred_betula_dataset$AmongVPD)
mean_centred_betula_dataset$AvgAmongPrecip <- mean(mean_centred_betula_dataset$AmongPrecip)
mean_centred_betula_dataset$AvgAmongTemp <- mean(mean_centred_betula_dataset$AmongTemp)
#calculating among deviation (deviation of each site's average from the global average)
mean_centred_betula_dataset$AmongVPDDev<- mean_centred_betula_dataset$AmongVPD - mean(mean_centred_betula_dataset$AmongVPD)
mean_centred_betula_dataset$AmongPrecipDev<- mean_centred_betula_dataset$AmongPrecip - mean(mean_centred_betula_dataset$AmongPrecip)
mean_centred_betula_dataset$AmongTempDev<- mean_centred_betula_dataset$AmongTemp - mean(mean_centred_betula_dataset$AmongTemp)
#calculating quadratic (quadratic deviation)
mean_centred_betula_dataset$AmongVPDDevQuad<- mean_centred_betula_dataset$AmongVPDDev^2
mean_centred_betula_dataset$AmongPrecipDevQuad<- mean_centred_betula_dataset$AmongPrecipDev^2
mean_centred_betula_dataset$AmongTempDevQuad<- mean_centred_betula_dataset$AmongTempDev^2

#calculating global quadratic (for global optimum models)
mean_centred_betula_dataset$Avg_VPDQuad <- mean_centred_betula_dataset$Avg_VPD^2
mean_centred_betula_dataset$Avg_precipQuad<- mean_centred_betula_dataset$Avg_precip^2
mean_centred_betula_dataset$Avg_meantempQuad<- mean_centred_betula_dataset$Avg_meantemp^2


#null model with no climate drivers 
null_model_betula <- glmmTMB(log1p(Ring.width)~Latitude+(1|Year)+(1|SiteID)+
                                ns(Centred_year, df=3)+
                                (1+ns(Centred_year, df=3)|TreeID)+
                                (1|Centred_year:SiteID),
                              data=mean_centred_betula_dataset)

#models without quadratics 
no_quads_betula <- glmmTMB(log1p(Ring.width)~AmongTempDev+WithinTemp+
                              AmongVPDDev+WithinVPD+
                              AmongPrecipDev+WithinPrecip+
                              (WithinTemp*AmongTempDev)+
                              Latitude+(1|Year)+(1|SiteID)+
                              ns(Centred_year, df=3)+
                              (1+ns(Centred_year, df=3)|TreeID)+
                              (1|Centred_year:SiteID),
                            data=mean_centred_betula_dataset)
#all variables, dropping temperature quadratic
no_quadsTemp_betula <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
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
                                data=mean_centred_betula_dataset)
#all variables dropping VPD quadratic
no_quadsVPD_betula <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
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
                               data=mean_centred_betula_dataset)
#all variables dropping precipitation quadratic
no_quadsPrecip_betula <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
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
                                  data=mean_centred_betula_dataset)

#global model no within
basic_global_model_betula <- glmmTMB(log1p(Ring.width)~Avg_precip+Avg_meantemp+Avg_VPD+
                                      Avg_precipQuad+Avg_meantempQuad+Avg_VPDQuad+
                                      Latitude+
                                      (1|Year)+(1|SiteID)+
                                      ns(Centred_year, df=3)+
                                      (1+ns(Centred_year, df=3)|TreeID)+
                                      (1|Centred_year:SiteID),
                                    data=mean_centred_betula_dataset)
#global model 
global_model_betula <- glmmTMB(log1p(Ring.width)~Avg_precip+Avg_meantemp+
                                      WithinPrecip+WithinVPD+WithinTemp+
                                      Avg_precipQuad+Avg_meantempQuad+
                                      WithinPrecipQuad4+WithinTempQuad4+
                                      (Avg_precip*WithinPrecip)+
                                      (Avg_meantemp*WithinTemp)+
                                      Latitude+
                                      (1|Year)+(1|SiteID)+
                                      ns(Centred_year, df=3)+
                                      (1+ns(Centred_year, df=3)|TreeID)+
                                      (1|Centred_year:SiteID),
                                    data=mean_centred_betula_dataset)
#global model without quadratics
global_model_no_quads_betula <- glmmTMB(log1p(Ring.width)~Avg_precip+Avg_meantemp+
                                 WithinPrecip+WithinTemp+
                                 (Avg_precip*WithinPrecip)+
                                 (Avg_meantemp*WithinTemp)+
                                 Latitude+
                                 (1|Year)+(1|SiteID)+
                                 ns(Centred_year, df=3)+
                                 (1+ns(Centred_year, df=3)|TreeID)+
                                 (1|Centred_year:SiteID),
                               data=mean_centred_betula_dataset)

summary(basic_global_model_betula)
summary(global_model_betula)
summary(global_model_no_quads_betula)


##final models
##latitude as fixed effect, year and siteID as random effect
#individual models for each climate driver 
#models with multiple climate drivers 
#####
temperature_quad_model_lat_year_betula <- glmmTMB(log1p(Ring.width)~AmongTempDev+WithinTemp+
                                             AmongTempDevQuad+WithinTempQuad4+
                                             (WithinTemp*AmongTempDev)+
                                             Latitude+(1|Year)+(1|SiteID)+
                                             ns(Centred_year, df=3)+
                                             (1+ns(Centred_year, df=3)|TreeID)+
                                             (1|Centred_year:SiteID),
                                           data=mean_centred_betula_dataset)

VPD_quad_model_lat_year_betula <- glmmTMB(log1p(Ring.width)~AmongVPDDev+WithinVPD+
                                     AmongVPDDevQuad+WithinVPDQuad4+
                                     (AmongVPDDev*WithinVPD)+
                                     Latitude+(1|Year)+(1|SiteID)+
                                     ns(Centred_year, df=3)+
                                     (1+ns(Centred_year, df=3)|TreeID)+
                                     (1|Centred_year:SiteID),
                                   data=mean_centred_betula_dataset)

precip_quad_model_lat_year_betula <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+WithinPrecip+
                                        AmongPrecipDevQuad+WithinPrecipQuad4+
                                        (AmongPrecipDev*WithinPrecip)+
                                        Latitude+(1|Year)+(1|SiteID)+
                                        ns(Centred_year, df=3)+
                                        (1+ns(Centred_year, df=3)|TreeID)+
                                        (1|Centred_year:SiteID),
                                      data=mean_centred_betula_dataset)


#climate model with all variables
all_climate_model_betula <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
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
                             data=mean_centred_betula_dataset)

#VPD + temp
VPD_temp_climate_model_betula <- glmmTMB(log1p(Ring.width)~AmongVPDDev+AmongTempDev+
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
                                  data=mean_centred_betula_dataset)

#VPD + precip
VPD_precip_climate_model_betula <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+
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
                                    data=mean_centred_betula_dataset)

library(glmmTMB)
library(splines)
#precip + temp
precip_temp_climate_model_betula <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongTempDev+
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
                                     data=mean_centred_betula_dataset)

summary(precip_temp_climate_model_betula)
#creating a dataframe to use in akaike.weights function 
AICdf_betula <- matrix(nrow=14, ncol=5)
colnames(AICdf_betula) <- c('Model', 'AIC', 'DeltaAIC', 'Weight', 'Species')

summary(null_model_betula)
AICdf_betula[1,1] <- c('Null model')
AICdf_betula[1,2] <-  19597.9

summary(no_quads_betula)
AICdf_betula[2,1] <- c('All climate variables, excluding quadratics')
AICdf_betula[2,2] <-  19591.9

summary(precip_quad_model_lat_year_betula)
AICdf_betula[3,1] <- c('Precipitation only')
AICdf_betula[3,2] <-  19594.6 

summary(temperature_quad_model_lat_year_betula)
AICdf_betula[4,1] <- c('Mean temperature only')
AICdf_betula[4,2] <- 19594.0

summary(VPD_quad_model_lat_year_betula)
AICdf_betula[5,1] <- c('VPD only')
AICdf_betula[5,2] <- 19597.5

summary(all_climate_model_betula)
AICdf_betula[6,1] <- c('All climate variables')
AICdf_betula[6,2] <- 19598.8

summary(VPD_temp_climate_model_betula)
AICdf_betula[7,1] <- c('VPD and mean temperature')
AICdf_betula[7,2] <- 19596.0

summary(VPD_precip_climate_model_betula)
AICdf_betula[8,1] <- c('VPD and precipitation')
AICdf_betula[8,2] <- 19599.5 

summary(precip_temp_climate_model_betula)
AICdf_betula[9,1] <- c('Precipitation and mean temperature')
AICdf_betula[9,2] <-  19592.4 

summary(no_quadsTemp_betula)
AICdf_betula[10,1] <- c('All climate variables, excluding temperature quadratic')
AICdf_betula[10,2] <-19597.9 

summary(no_quadsVPD_betula)
AICdf_betula[11,1] <- c('All climate variables, excluding VPD quadratic')
AICdf_betula[11,2] <- 19595.4 

summary(no_quadsPrecip_betula)
AICdf_betula[12,1] <- c('All climate variables, excluding precipitation quadratic')
AICdf_betula[12,2] <- 19597.7 

summary(global_model_betula)
AICdf_betula[13,1] <- c('Global optimum model')
AICdf_betula[13,2] <- 19591.6 

summary(global_model_no_quads_betula)
AICdf_betula[14,1] <- c('Global optimum model, excluding quadratics')
AICdf_betula[14,2] <-  19592.9

#calculating delta AIC and weights
library(qpcR)
weights_betula <- akaike.weights(as.numeric(AICdf_betula[,2]))
AICdf_betula[,3] <- weights_betula$deltaAIC
AICdf_betula[,4] <- weights_betula$weights

AICdf_betula[,5] <- "Betula pendula"
AIC_df <- read.csv("Figures/AIC_scores.csv")
AIC_df <- AIC_df[,-1]
AIC_df <- rbind(AIC_df, AICdf_betula)
write.csv(AIC_df, "Figures/AIC_scores.csv")

##AIC plot 
AIC_df <- read.csv("Figures/AIC_scores.csv")
AIC_df$Weight <- as.numeric(AIC_df$Weight)
AIC_df$DeltaAIC <- as.numeric(AIC_df$DeltaAIC)
AIC_df$DeltaAIC <- format(round(AIC_df$DeltaAIC, 2), nsmall = 2)
AIC_df$DeltaAIC <- as.numeric(AIC_df$DeltaAIC)

#ordering the models in a more intuitive way
AIC_df$Model <- factor(AIC_df$Model, levels=c("Null model", "Mean temperature only",
                                                         "Precipitation only", "VPD only",
                                                         "Precipitation and mean temperature", "VPD and mean temperature",
                                                         "VPD and precipitation", "All climate variables",
                                                         "All climate variables, excluding quadratics",
                                                         "All climate variables, excluding temperature quadratic",
                                                         "All climate variables, excluding precipitation quadratic",
                                                         "All climate variables, excluding VPD quadratic",
                                                         "Global optimum model",
                                                         "Global optimum model, excluding quadratics"))


library(ggplot2)
library(cowplot)
library(viridis)
#creating figure
#because of large number of models, discrete function for viridis not working and has to be scale
ggplot(data=AIC_df, aes(x=Species, y=DeltaAIC, fill=Model))+
  geom_bar(position="dodge", stat="identity")+
  theme_cowplot()+
  ylim(0,125)+
  theme(axis.text=element_text(size=10), legend.text = element_text(size=10))+
  geom_text(aes(label=DeltaAIC), vjust=-0.5, size = 2, position = position_dodge(0.9))+
  ylab("Delta AIC")+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(face="italic"),
        legend.text=element_text(size=10))+
  scale_fill_viridis(discrete=TRUE)


##plotting 
library(dplyr)

###among quad betula
#####
#skeleton dataset for prediction of VPD
#keeping everything fixed apart from the among and among quad values so only thing that 
#varies is the average climatic predictor, to show the global/spatial relationship
med_year <- median(mean_centred_betula_dataset$Centred_year)
med_yr <- 2000
med_withinVPD <- median(mean_centred_betula_dataset$WithinVPD)
med_withinVPDQuad <- med_withinVPD^2
MinAmongVPDDev <- min(mean_centred_betula_dataset$AmongVPDDev)
MaxAmongVPDDev <- max(mean_centred_betula_dataset$AmongVPDDev)
pred_amongVPDDev <- seq(MinAmongVPDDev, MaxAmongVPDDev+0.1, 0.1)
pred_amongVPDDevQuad <- pred_amongVPDDev^2
tree_id <- as.character((mean_centred_betula_dataset$TreeID[1])) 
site_id <- as.character((mean_centred_betula_dataset$SiteID[1]))
med_lat <- mean_centred_betula_dataset$Latitude[1]
med_amongTempDev <- median(mean_centred_betula_dataset$AmongTempDev)
med_amongTempDevQuad <- med_amongTempDev^2
med_amongPrecipDev <- median(mean_centred_betula_dataset$AmongPrecipDev)
med_amongPrecipDevQuad <- med_amongPrecipDev^2
med_withinTemp <- median(mean_centred_betula_dataset$WithinTemp)
med_withinTempQuad <- med_withinTemp^2
med_withinPrecip <- median(mean_centred_betula_dataset$WithinPrecip)
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
med_year <- median(mean_centred_betula_dataset$Centred_year)
med_yr <- 2000
med_withinTemp <- median(mean_centred_betula_dataset$WithinTemp)
med_withinTempQuad <- med_withinTemp^2
MinAmongTempDev <- min(mean_centred_betula_dataset$AmongTempDev)
MaxAmongTempDev <- max(mean_centred_betula_dataset$AmongTempDev)
pred_amongTempDev <- seq(MinAmongTempDev, MaxAmongTempDev+1, 1)
pred_amongTempDevQuad <- pred_amongTempDev^2
tree_id <- as.character((mean_centred_betula_dataset$TreeID[1])) 
site_id <- as.character((mean_centred_betula_dataset$SiteID[1]))
med_lat <- mean_centred_betula_dataset$Latitude[1]
med_amongVPDDev <- median(mean_centred_betula_dataset$AmongVPDDev)
med_amongVPDDevQuad <- med_amongVPDDev^2
med_amongDevPrecip <- median(mean_centred_betula_dataset$AmongPrecipDev)
med_amongPrecipDevQuad <- med_amongPrecipDev^2
med_withinVPD <- median(mean_centred_betula_dataset$WithinVPD)
med_withinVPDquad <- med_withinVPD^2
med_withinPrecip <- median(mean_centred_betula_dataset$WithinPrecip)
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
med_year <- median(mean_centred_betula_dataset$Centred_year)
med_yr <- 2000
med_withinPrecip <- median(mean_centred_betula_dataset$WithinPrecip)
med_withinPrecipQuad <- med_withinPrecip^2
MinAmongPrecipDev <- min(mean_centred_betula_dataset$AmongPrecipDev)
MaxAmongPrecipDev <- max(mean_centred_betula_dataset$AmongPrecipDev)
pred_amongPrecipDev <- seq(MinAmongPrecipDev, MaxAmongPrecipDev+0.2, 0.2)
pred_amongPrecipDevQuad <- pred_amongPrecipDev^2
tree_id <- as.character((mean_centred_betula_dataset$TreeID[1])) 
site_id <- as.character((mean_centred_betula_dataset$SiteID[1]))
med_lat <- mean_centred_betula_dataset$Latitude[1]
med_amongVPDDev <- median(mean_centred_betula_dataset$AmongVPDDev)
med_amongVPDDevQuad <- med_amongVPDDev^2
med_amongTempDev <- median(mean_centred_betula_dataset$AmongTempDev)
med_amongTempDevQuad <- med_amongTempDev^2
med_withinVPD <- median(mean_centred_betula_dataset$WithinVPD)
med_withinVPDQuad <- med_withinVPD^2
med_withinTemp <- median(mean_centred_betula_dataset$WithinTemp)
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

#predicting expected tree ring widths from the prediction dataframes created above
VPD_pred_among_betula <-predict(all_climate_model_betula, newdata = newdataAmongVPD, re.form=NULL, se.fit=TRUE)
temp_pred_among_betula <-predict(all_climate_model_betula, newdata = newdataAmongTemp, re.form=NULL, se.fit=TRUE)
precip_pred_among_betula <-predict(all_climate_model_betula, newdata = newdataAmongPrecip, re.form=NULL, se.fit=TRUE)

#adding predicted points to prediction dataframe
newdataAmongVPD$Prediction <- as.numeric(VPD_pred_among_betula$fit)
newdataAmongVPD$SE <- as.numeric(VPD_pred_among_betula$se.fit)
newdataAmongTemp$Prediction  <- as.numeric(temp_pred_among_betula$fit)
newdataAmongTemp$SE <- as.numeric(temp_pred_among_betula$se.fit)
newdataAmongPrecip$Prediction  <- as.numeric(precip_pred_among_betula$fit)
newdataAmongPrecip$SE <- as.numeric(precip_pred_among_betula$se.fit)

#calculating prediction error of among slope
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
amongmeantemps <- seq(MinAmongTempDev, MaxAmongTempDev, 2.5) 
within_points_df <- list()

#creating new dataframe where among is now fixed but within varies along a sequence ranging from
#the minimum to maximum deviation within the dataset
for(i in 1:length(amongmeantemps)){
  med_amongTempDev <- amongmeantemps[i]
  med_amongTempDevQuad <- med_amongTempDev^2
  
  MinWithinTemp <- min(mean_centred_betula_dataset$WithinTemp)
  MaxWithinTemp <- max(mean_centred_betula_dataset$WithinTemp)
  pred_withinTemp <- seq(MinWithinTemp, MaxWithinTemp, 0.5)
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
                              AvgAmongTemp = rep(unique(as.numeric(mean_centred_betula_dataset$AvgAmongTemp)), length(pred_withinTemp)))
  
  newdatasubset$ActualAmongTemp <- newdatasubset$AvgAmongTemp+newdatasubset$AmongTempDev
  newdatasubset$ActualTemp <- newdatasubset$ActualAmongTemp+newdatasubset$WithinTemp
  newdatasubset$WithinGlobDev <- newdatasubset$ActualTemp-newdatasubset$AvgAmongTemp
  
  temp_pred_subset <-predict(all_climate_model_betula, newdata = newdatasubset, re.form=NULL, se.fit=TRUE)
  temp_pred_subset_vals <- temp_pred_subset$fit
  temp_pred_subset_HCI <- temp_pred_subset_vals+1.96*temp_pred_subset$se.fit
  temp_pred_subset_LCI <- temp_pred_subset_vals-1.96*temp_pred_subset$se.fit
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
betula_temp <- ggplot(data=mean_centred_betula_dataset, aes(x=AmongTemp, y=log1p(Ring.width)))+
  geom_line(data = data.frame(AmongTempDev = newdataAmongTemp$AmongTempDev, Ring.width = newdataAmongTemp$Prediction), aes(x = AmongTempDev, y = Ring.width), colour = "blue", linetype = "dotted")+
  geom_line(data = data.frame(WithinGlobDev = within_curves1$WithinGlobDev, Ring.width = within_curves1$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dotted")+
  geom_line(data = data.frame(WithinGlobDev = within_curves1$WithinGlobDev, Ring.width = within_curves1$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves1$WithinGlobDev, Ring.width = within_curves1$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves2$WithinGlobDev, Ring.width = within_curves2$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dotted")+
  geom_line(data = data.frame(WithinGlobDev = within_curves2$WithinGlobDev, Ring.width = within_curves2$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves2$WithinGlobDev, Ring.width = within_curves2$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves3$WithinGlobDev, Ring.width = within_curves3$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dotted")+
  geom_line(data = data.frame(WithinGlobDev = within_curves3$WithinGlobDev, Ring.width = within_curves3$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves3$WithinGlobDev, Ring.width = within_curves3$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves4$WithinGlobDev, Ring.width = within_curves4$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dotted")+
  geom_line(data = data.frame(WithinGlobDev = within_curves4$WithinGlobDev, Ring.width = within_curves4$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves4$WithinGlobDev, Ring.width = within_curves4$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  theme_cowplot()+
  xlab("Deviation from global average temperature (°C)")+
  ylab("Ring width (log+1)")+
  ggtitle("Betula pendula")+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))#+
  #ylim(2, 6)+
  #xlim(-8, 6)

betula_temp
#for VPD
amongVPDs <- seq(MinAmongVPDDev, MaxAmongVPDDev, 0.125) 
within_points_df <- list()

for(i in 1:length(amongVPDs)){
  med_amongVPDDev <- amongVPDs[i]
  med_amongVPDDevQuad <- med_amongVPDDev^2
  
  MinWithinVPD <- min(mean_centred_betula_dataset$WithinVPD)
  MaxWithinVPD <- max(mean_centred_betula_dataset$WithinVPD)
  pred_withinVPD <- seq(MinWithinVPD, MaxWithinVPD, 0.5)
  pred_withinVPDQuad <- pred_withinVPD^2
  
  med_withinTemp <- median(mean_centred_betula_dataset$WithinTemp)
  med_withinTempQuad <- med_withinTemp^2
  med_amongTempDev <- median(mean_centred_betula_dataset$AmongTempDev)
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
                              AvgAmongVPD = rep(unique(as.numeric(mean_centred_betula_dataset$AvgAmongVPD)), length(pred_withinVPD)))
  
  newdatasubset$ActualAmongVPD <- newdatasubset$AvgAmongVPD+newdatasubset$AmongVPDDev
  newdatasubset$ActualVPD <- newdatasubset$ActualAmongVPD+newdatasubset$WithinVPD
  newdatasubset$WithinGlobDev <- newdatasubset$ActualVPD-newdatasubset$AvgAmongVPD
  
  VPD_pred_subset <-predict(all_climate_model_betula, newdata = newdatasubset, re.form=NULL, se.fit=TRUE)
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
betula_VPD <- ggplot(data=mean_centred_betula_dataset, aes(x=AmongVPD, y=log1p(Ring.width)))+
  geom_line(data = data.frame(AmongVPDDev = newdataAmongVPD$AmongVPDDev, Ring.width = newdataAmongVPD$Prediction), aes(x = AmongVPDDev, y = Ring.width), colour = "blue", linetype = "dotted")+
  geom_line(data = data.frame(WithinGlobDev = within_curves1$WithinGlobDev, Ring.width = within_curves1$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dotted")+
  geom_line(data = data.frame(WithinGlobDev = within_curves1$WithinGlobDev, Ring.width = within_curves1$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves1$WithinGlobDev, Ring.width = within_curves1$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves2$WithinGlobDev, Ring.width = within_curves2$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dotted")+
  geom_line(data = data.frame(WithinGlobDev = within_curves2$WithinGlobDev, Ring.width = within_curves2$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves2$WithinGlobDev, Ring.width = within_curves2$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves3$WithinGlobDev, Ring.width = within_curves3$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dotted")+
  geom_line(data = data.frame(WithinGlobDev = within_curves3$WithinGlobDev, Ring.width = within_curves3$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves3$WithinGlobDev, Ring.width = within_curves3$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves4$WithinGlobDev, Ring.width = within_curves4$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dotted")+
  geom_line(data = data.frame(WithinGlobDev = within_curves4$WithinGlobDev, Ring.width = within_curves4$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves4$WithinGlobDev, Ring.width = within_curves4$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  theme_cowplot()+
  xlab("Deviation from global average VPD (kPa)")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))#+
  #ylim(2,6)+
  #xlim(-0.5, 1)

betula_VPD

#for precip
amongPrecips <- seq(MinAmongPrecipDev, MaxAmongPrecipDev, 1) 
within_points_df <- list()

for(i in 1:length(amongPrecips)){
  med_year <- median(mean_centred_betula_dataset$Centred_year)
  med_yr <- 2000
  med_amongPrecipDev <- amongPrecips[i]
  med_amongPrecipDevQuad <- med_amongPrecipDev^2
  tree_id <- as.character((mean_centred_betula_dataset$TreeID[1]))
  site_id <- as.character((mean_centred_betula_dataset$SiteID[1]))
  med_lat <- mean_centred_betula_dataset$Latitude[1]
  
  MinWithinPrecip <- min(mean_centred_betula_dataset$WithinPrecip)
  MaxWithinPrecip <- max(mean_centred_betula_dataset$WithinPrecip)
  pred_withinPrecip <- seq(MinWithinPrecip, MaxWithinPrecip, 0.25)
  pred_withinPrecipQuad <- pred_withinPrecip^2
  
  med_withinVPD <- median(mean_centred_betula_dataset$WithinVPD)
  med_withinVPDquad <- med_withinVPD^2
  med_amongVPDDev <- median(mean_centred_betula_dataset$AmongVPDDev)
  med_amongVPDDevQuad <- med_amongVPDDev^2
  
  med_withinTemp <- median(mean_centred_betula_dataset$WithinTemp)
  med_withinTempQuad <- med_withinTemp^2
  med_amongTempDev <- median(mean_centred_betula_dataset$AmongTempDev)
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
                              AvgAmongPrecip = rep(unique(as.numeric(mean_centred_betula_dataset$AvgAmongPrecip)), length(pred_withinPrecip)))
  
  newdatasubset$ActualAmongPrecip <- newdatasubset$AvgAmongPrecip+newdatasubset$AmongPrecipDev
  newdatasubset$ActualPrecip <- newdatasubset$ActualAmongPrecip+newdatasubset$WithinPrecip
  newdatasubset$WithinGlobDev <- newdatasubset$ActualPrecip-newdatasubset$AvgAmongPrecip
  
  Precip_pred_subset <-predict(all_climate_model_betula, newdata = newdatasubset, re.form=NULL, se.fit=TRUE)
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
betula_Precip <- ggplot(data=mean_centred_betula_dataset, aes(x=AmongPrecip, y=log1p(Ring.width)))+
  geom_line(data = data.frame(AmongPrecipDev = newdataAmongPrecip$AmongPrecipDev, Ring.width = newdataAmongPrecip$Prediction), aes(x = AmongPrecipDev, y = Ring.width), colour = "blue", linetype = "dotted")+
  geom_line(data = data.frame(WithinGlobDev = within_curves1$WithinGlobDev, Ring.width = within_curves1$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dotted")+
  geom_line(data = data.frame(WithinGlobDev = within_curves1$WithinGlobDev, Ring.width = within_curves1$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves1$WithinGlobDev, Ring.width = within_curves1$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves2$WithinGlobDev, Ring.width = within_curves2$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dotted")+
  geom_line(data = data.frame(WithinGlobDev = within_curves2$WithinGlobDev, Ring.width = within_curves2$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves2$WithinGlobDev, Ring.width = within_curves2$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves3$WithinGlobDev, Ring.width = within_curves3$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dotted")+
  geom_line(data = data.frame(WithinGlobDev = within_curves3$WithinGlobDev, Ring.width = within_curves3$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves3$WithinGlobDev, Ring.width = within_curves3$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves4$WithinGlobDev, Ring.width = within_curves4$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype = "dotted")+
  geom_line(data = data.frame(WithinGlobDev = within_curves4$WithinGlobDev, Ring.width = within_curves4$HCI), aes(x = WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  geom_line(data = data.frame(WithinGlobDev = within_curves4$WithinGlobDev, Ring.width = within_curves4$LCI), aes(x= WithinGlobDev, y = Ring.width), colour = "pink", linetype = "dashed")+
  theme_cowplot()+
  xlab("Deviation from global average precipitation (mm)")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))#+
  #ylim(2,4)+
  #xlim(-4, 6)

betula_Precip


