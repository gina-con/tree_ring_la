#for more detailed annotation, see amongdev_fagus
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
  subset$WithinPrecip <- subset$Avg_precip-(mean(subset$Avg_precip))#within is the deviation of each observation from the mean of each site
  subset$WithinPrecipQuad4 <- (subset$Avg_precip-(mean(subset$Avg_precip)))^2 #quadratic calculated using equation 4 from Fay et al
  #equation 4 assumes that the effect of temperature varies among populations (which would be the case if locally adapted)
  subset$AmongPrecipQuad <- mean(subset$Avg_precip)^2
  
  #vpd
  subset$AmongVPD <- mean(subset$Avg_VPD)#among is the mean max summer temperature of each site
  subset$WithinVPD <- subset$Avg_VPD-(mean(subset$Avg_VPD))#within is the deviation of each observation from the mean of each site
  subset$WithinVPDQuad4 <- (subset$Avg_VPD-(mean(subset$Avg_VPD)))^2 #quadratic calculated using equation 4 from Fay et al
  #equation 4 assumes that the effect of temperature varies among populations (which would be the case if locally adapted)
  subset$AmongVPDQuad <- mean(subset$Avg_VPD)^2
  
  #mean temp
  subset$AmongTemp <- mean(subset$Avg_meantemp)#among is the mean max summer temperature of each site
  subset$WithinTemp <- subset$Avg_meantemp-(mean(subset$Avg_meantemp))#within is the deviation of each observation from the mean of each site
  subset$WithinTempQuad4 <- (subset$Avg_meantemp-(mean(subset$Avg_meantemp)))^2 #quadratic calculated using equation 4 from Fay et al
  #equation 4 assumes that the effect of temperature varies among populations (which would be the case if locally adapted)
  subset$AmongTempQuad <- mean(subset$Avg_meantemp)^2
  
  mean_centred_quercus_dataset <- rbind(mean_centred_quercus_dataset, subset)
}

mean_centred_quercus_dataset <- mean_centred_quercus_dataset[,-c(1,2)]

write.csv(mean_centred_quercus_dataset, "Data/e_obs/mean_centred_data_quercus.csv")
mean_centred_quercus_dataset <- read.csv("Data/e_obs/mean_centred_data_quercus.csv")

#ITQP9 and ITQP10 no VPD or temp data so removing trees from this site from the dataset
library(dplyr)
red_quercus_dataset <- mean_centred_quercus_dataset %>%
  filter(SiteID != "ITQP09") %>%
  filter(SiteID != "ITQP10")

#checking for correlation between climate variables
cor.test(red_quercus_dataset$Avg_meantemp, red_quercus_dataset$Avg_VPD)
cor.test(red_quercus_dataset$Avg_precip, red_quercus_dataset$Avg_VPD)
cor.test(red_quercus_dataset$Avg_meantemp, red_quercus_dataset$Avg_precip)
#checking correlation in space and time 
cor.test(red_quercus_dataset$AmongTemp, red_quercus_dataset$AmongVPD)
cor.test(red_quercus_dataset$WithinTemp, red_quercus_dataset$WithinVPD)
cor.test(red_quercus_dataset$AmongTemp, red_quercus_dataset$AmongPrecip)
cor.test(red_quercus_dataset$WithinTemp, red_quercus_dataset$WithinPrecip)
cor.test(red_quercus_dataset$AmongPrecip, red_quercus_dataset$AmongVPD)
cor.test(red_quercus_dataset$WithinPrecip, red_quercus_dataset$WithinVPD)

quer_cor <- ggplot(data=red_quercus_dataset, aes(x=Avg_VPD, y=Avg_meantemp))+
  geom_point()+
  theme_cowplot()

#calculating average among (global average)
red_quercus_dataset$AvgAmongVPD <- mean(red_quercus_dataset$AmongVPD)
red_quercus_dataset$AvgAmongPrecip <- mean(red_quercus_dataset$AmongPrecip)
red_quercus_dataset$AvgAmongTemp <- mean(red_quercus_dataset$AmongTemp)
#calculating among deviation (deviation from global average)
red_quercus_dataset$AmongVPDDev<- red_quercus_dataset$AmongVPD - mean(red_quercus_dataset$AmongVPD)
red_quercus_dataset$AmongPrecipDev<- red_quercus_dataset$AmongPrecip - mean(red_quercus_dataset$AmongPrecip)
red_quercus_dataset$AmongTempDev<- red_quercus_dataset$AmongTemp - mean(red_quercus_dataset$AmongTemp)
#calculating quadratic (quadratic deviation)
red_quercus_dataset$AmongVPDDevQuad<- red_quercus_dataset$AmongVPDDev^2
red_quercus_dataset$AmongPrecipDevQuad<- red_quercus_dataset$AmongPrecipDev^2
red_quercus_dataset$AmongTempDevQuad<- red_quercus_dataset$AmongTempDev^2
#calculating quadratics for global optimum model
red_quercus_dataset$Avg_VPDQuad <- red_quercus_dataset$Avg_VPD^2
red_quercus_dataset$Avg_precipQuad<- red_quercus_dataset$Avg_precip^2
red_quercus_dataset$Avg_meantempQuad<- red_quercus_dataset$Avg_meantemp^2

#null model with no climate drivers 
null_model_quercus <- glmmTMB(log1p(Ring.width)~Latitude+(1|Year)+(1|SiteID)+
                        ns(Centred_year, df=3)+
                        (1+ns(Centred_year, df=3)|TreeID)+
                        (1|Centred_year:SiteID),
                      data=red_quercus_dataset)

#dropping quadratics to check assumption of optimum climate value
no_quads_spat_quercus <- glmmTMB(log1p(Ring.width)~AmongTempDev+WithinTemp+WithinTempQuad4+
                           AmongVPDDev+WithinVPD+WithinVPDQuad4+
                           AmongPrecipDev+WithinPrecip+WithinPrecipQuad4+
                           (WithinTemp*AmongTempDev)+
                             (AmongPrecipDev*WithinPrecip)+
                             (AmongVPDDev*WithinVPD)+
                           Latitude+(1|Year)+(1|SiteID)+
                           ns(Centred_year, df=3)+
                           (1+ns(Centred_year, df=3)|TreeID)+
                           (1|Centred_year:SiteID),
                         data=red_quercus_dataset)
#dropping temperature quadratic (spatial)
no_quadsTemp_spat_quercus <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
                               AmongPrecipDevQuad+AmongVPDDevQuad+
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
                             data=red_quercus_dataset)
#dropping VPD quadratic (spatial)
no_quadsVPD_spat_quercus <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
                              AmongPrecipDevQuad+AmongTempDevQuad+
                              WithinPrecip+WithinVPD+WithinTemp+
                              WithinPrecipQuad4+WithinTempQuad4+WithinVPDQuad4+
                              (AmongPrecipDev*WithinPrecip)+
                              (AmongVPDDev*WithinVPD)+
                              (AmongTempDev*WithinTemp)+
                              Latitude+
                              (1|Year)+(1|SiteID)+
                              ns(Centred_year, df=3)+
                              (1+ns(Centred_year, df=3)|TreeID)+
                              (1|Centred_year:SiteID),
                            data=red_quercus_dataset)
#dropping precipitation quadratic (spatial)
no_quadsPrecip_spat_quercus <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
                                 AmongVPDDevQuad+AmongTempDevQuad+
                                 WithinPrecip+WithinVPD+WithinTemp+
                                 WithinVPDQuad4+WithinTempQuad4+WithinPrecipQuad4+
                                 (AmongPrecipDev*WithinPrecip)+
                                 (AmongVPDDev*WithinVPD)+
                                 (AmongTempDev*WithinTemp)+
                                 Latitude+
                                 (1|Year)+(1|SiteID)+
                                 ns(Centred_year, df=3)+
                                 (1+ns(Centred_year, df=3)|TreeID)+
                                 (1|Centred_year:SiteID),
                               data=red_quercus_dataset)


#dropping quadratics to check assumption of optimum climate value (temporal)
no_quads_temp_quercus <- glmmTMB(log1p(Ring.width)~AmongTempDev+WithinTemp+AmongTempDevQuad+
                           AmongVPDDev+WithinVPD+AmongVPDDevQuad+
                           AmongPrecipDev+WithinPrecip+AmongPrecipDevQuad+
                           (WithinTemp*AmongTempDev)+
                             (AmongPrecipDev*WithinPrecip)+
                             (AmongVPDDev*WithinVPD)+
                           Latitude+(1|Year)+(1|SiteID)+
                           ns(Centred_year, df=3)+
                           (1+ns(Centred_year, df=3)|TreeID)+
                           (1|Centred_year:SiteID),
                         data=red_quercus_dataset)
#dropping temperature quadratic (temporal)
no_quadsTemp_temp_quercus <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
                               AmongPrecipDevQuad+AmongVPDDevQuad+AmongTempDevQuad+
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
                             data=red_quercus_dataset)
#dropping VPD quadratic (temporal)
no_quadsVPD_temp_quercus <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
                              AmongPrecipDevQuad+AmongTempDevQuad+AmongVPDDevQuad+
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
                            data=red_quercus_dataset)
#dropping precipitation quadratic (temporal)
no_quadsPrecip_temp_quercus <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
                                 AmongVPDDevQuad+AmongTempDevQuad+AmongPrecipDevQuad+
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
                               data=red_quercus_dataset)

#global model to check splitting temporal and spatial adds value
#took out within as well
global_model_quercus <- glmmTMB(log1p(Ring.width)~Avg_precip+Avg_meantemp+Avg_VPD+
                          Avg_precipQuad+Avg_meantempQuad+Avg_VPDQuad+
                          Latitude+
                          (1|Year)+(1|SiteID)+
                          ns(Centred_year, df=3)+
                          (1+ns(Centred_year, df=3)|TreeID)+
                          (1|Centred_year:SiteID),
                        data=red_quercus_dataset)

summary(global_model_quercus)

#creating the fixed skeleton dataframes for the prediction
med_yr <- 2000
med_year <- median(red_quercus_dataset$Centred_year)
med_lat <- median(red_quercus_dataset$Latitude)
tree_id <- red_quercus_dataset$SiteID[1]
site_id <- red_quercus_dataset$TreeID[1]
med_temp <- median(red_quercus_dataset$Avg_meantemp)
med_VPD <- median(red_quercus_dataset$Avg_VPD)
med_precip <- median(red_quercus_dataset$Avg_precip)
med_temp_quad <- med_temp^2
med_VPD_quad <- med_VPD^2
med_precip_quad <- med_precip^2
pred_temp <- seq(min(red_quercus_dataset$Avg_meantemp), max(red_quercus_dataset$Avg_meantemp), 2)
pred_VPD <- seq(min(red_quercus_dataset$Avg_VPD), max(red_quercus_dataset$Avg_VPD), 0.1)
pred_precip <- seq(min(red_quercus_dataset$Avg_precip), max(red_quercus_dataset$Avg_precip), 0.5)
pred_temp_quad <- pred_temp^2
pred_VPD_quad <- pred_VPD^2
pred_precip_quad <- pred_precip^2

global_model_dataset_temp_quercus <- data.frame(Year=rep(med_yr, length(pred_temp)),
                                               Centred_year=rep(med_year, length(pred_temp)),
                                               Latitude=rep(med_lat, length(pred_temp)),
                                               TreeID=tree_id,
                                               Year=rep(med_year, length(pred_temp)),
                                               SiteID=site_id,
                                               Avg_meantemp=pred_temp,
                                               Avg_meantempQuad=pred_temp_quad,
                                               Avg_VPD=rep(med_VPD, length(pred_temp)),
                                               Avg_VPDQuad=rep(med_VPD_quad, length(pred_temp)),
                                               Avg_precip=rep(med_precip, length(pred_temp)),
                                               Avg_precipQuad=rep(med_precip_quad, length(pred_temp)))

global_model_dataset_VPD_quercus <- data.frame(Year=rep(med_yr, length(pred_VPD)),
                                              Centred_year=rep(med_year, length(pred_VPD)),
                                              Latitude=rep(med_lat, length(pred_VPD)),
                                              TreeID=tree_id,
                                              Year=rep(med_year, length(pred_VPD)),
                                              SiteID=site_id,
                                              Avg_meantemp=rep(med_temp, length(pred_VPD)),
                                              Avg_meantempQuad=rep(med_temp_quad, length(pred_VPD)),
                                              Avg_VPD=pred_VPD,
                                              Avg_VPDQuad=pred_VPD_quad,
                                              Avg_precip=rep(med_precip, length(pred_VPD)),
                                              Avg_precipQuad=rep(med_precip_quad, length(pred_VPD)))

global_model_dataset_precip_quercus <- data.frame(Year=rep(med_yr, length(pred_precip)),
                                                 Centred_year=rep(med_year, length(pred_precip)),
                                                 Latitude=rep(med_lat, length(pred_precip)),
                                                 TreeID=tree_id,
                                                 Year=rep(med_year, length(pred_precip)),
                                                 SiteID=site_id,
                                                 Avg_meantemp=rep(med_temp, length(pred_precip)),
                                                 Avg_meantempQuad=rep(med_temp_quad, length(pred_precip)),
                                                 Avg_VPD=rep(med_VPD, length(pred_precip)),
                                                 Avg_VPDQuad=rep(med_VPD_quad, length(pred_precip)),
                                                 Avg_precip=pred_precip,
                                                 Avg_precipQuad=pred_precip_quad)
#predictions with datasets from above
global_prediction_temp_quercus <- predict(global_model_quercus, newdata = global_model_dataset_temp_quercus, re.form=NULL, se.fit=TRUE)
global_prediction_precip_quercus <- predict(global_model_quercus, newdata = global_model_dataset_precip_quercus, re.form=NULL, se.fit=TRUE)
global_prediction_VPD_quercus <- predict(global_model_quercus, newdata = global_model_dataset_VPD_quercus, re.form=NULL, se.fit=TRUE)
#putting predictions and se into dataframes
global_model_dataset_temp_quercus$pred <- global_prediction_temp_quercus$fit
global_model_dataset_precip_quercus$pred <- global_prediction_precip_quercus$fit
global_model_dataset_VPD_quercus$pred <- global_prediction_VPD_quercus$fit
global_model_dataset_temp_quercus$SE <- global_prediction_temp_quercus$se.fit
global_model_dataset_precip_quercus$SE <- global_prediction_precip_quercus$se.fit
global_model_dataset_VPD_quercus$SE <- global_prediction_VPD_quercus$se.fit
#calculating confidence intervals
global_model_dataset_temp_quercus$LCI <- global_model_dataset_temp_quercus$pred-1.96*global_model_dataset_temp_quercus$SE
global_model_dataset_temp_quercus$HCI <- global_model_dataset_temp_quercus$pred+1.96*global_model_dataset_temp_quercus$SE
global_model_dataset_VPD_quercus$LCI <- global_model_dataset_VPD_quercus$pred-1.96*global_model_dataset_VPD_quercus$SE
global_model_dataset_VPD_quercus$HCI <- global_model_dataset_VPD_quercus$pred+1.96*global_model_dataset_VPD_quercus$SE
global_model_dataset_precip_quercus$LCI <- global_model_dataset_precip_quercus$pred-1.96*global_model_dataset_precip_quercus$SE
global_model_dataset_precip_quercus$HCI <- global_model_dataset_precip_quercus$pred+1.96*global_model_dataset_precip_quercus$SE

#making figures for this
querc_raw_precip <- ggplot(data=global_model_dataset_precip_quercus, aes(x=Avg_precip, y=pred))+
  geom_line(colour="mediumpurple3", linetype="dashed")+
  geom_line(data=global_model_dataset_precip_quercus, aes(x=Avg_precip, y=HCI), colour = "pink", linetype = "dashed")+
  geom_line(data=global_model_dataset_precip_quercus, aes(x=Avg_precip, y=LCI), colour = "pink", linetype = "dashed")+
  xlab("Average daily precipitation (mm)")+
  ylab("Ring width (log+1)")+
  theme_cowplot()+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))+
  ylim(3, 7)+
  xlim(1, 8)

querc_raw_temp <- ggplot(data=global_model_dataset_temp_quercus, aes(x=Avg_meantemp, y=pred))+
  geom_line(col="mediumpurple3")+
  geom_line(data=global_model_dataset_temp_quercus, aes(x=Avg_meantemp, y=HCI), colour = "pink", linetype = "dashed")+
  geom_line(data=global_model_dataset_temp_quercus, aes(x=Avg_meantemp, y=LCI), colour = "pink", linetype = "dashed")+
  xlab("Average daily mean temperature (°C)")+
  ylab("Ring width (log+1)")+
  theme_cowplot()+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))+
  ylim(3, 7)+
  xlim(9, 21)

querc_raw_VPD <- ggplot(data=global_model_dataset_VPD_quercus, aes(x=Avg_VPD, y=pred))+
  geom_line(colour="mediumpurple3", linetype="dashed")+
  geom_line(data=global_model_dataset_VPD_quercus, aes(x=Avg_VPD, y=HCI), colour = "pink", linetype = "dashed")+
  geom_line(data=global_model_dataset_VPD_quercus, aes(x=Avg_VPD, y=LCI), colour = "pink", linetype = "dashed")+
  xlab("Average daily VPD (kPa)")+
  ylab("Ring width (log+1)")+
  ggtitle("Quercus petraea")+
  theme_cowplot()+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))+
  ylim(3, 7)+
  xlim(0.2, 1)


#dropping quadratics
global_model_no_quads_quercus <- glmmTMB(log1p(Ring.width)~Avg_precip+Avg_meantemp+Avg_VPD+
                                   Latitude+
                                   (1|Year)+(1|SiteID)+
                                   ns(Centred_year, df=3)+
                                   (1+ns(Centred_year, df=3)|TreeID)+
                                   (1|Centred_year:SiteID),
                                 data=red_quercus_dataset)

##final model
##latitude as fixed effect, year and siteID as random effect
#individual models for each climate driver 
#####
temperature_quad_model_lat_year_quercus <- glmmTMB(log1p(Ring.width)~AmongTempDev+WithinTemp+
                                             AmongTempDevQuad+WithinTempQuad4+
                                             (WithinTemp*AmongTempDev)+
                                             Latitude+(1|Year)+(1|SiteID)+
                                             ns(Centred_year, df=3)+
                                             (1+ns(Centred_year, df=3)|TreeID)+
                                             (1|Centred_year:SiteID),
                                           data=red_quercus_dataset)

VPD_quad_model_lat_year_quercus <- glmmTMB(log1p(Ring.width)~AmongVPDDev+WithinVPD+
                                     AmongVPDDevQuad+WithinVPDQuad4+
                                     (AmongVPDDev*WithinVPD)+
                                     Latitude+(1|Year)+(1|SiteID)+
                                     ns(Centred_year, df=3)+
                                     (1+ns(Centred_year, df=3)|TreeID)+
                                     (1|Centred_year:SiteID),
                                   data=red_quercus_dataset)

precip_quad_model_lat_year_quercus <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+WithinPrecip+
                                        AmongPrecipDevQuad+WithinPrecipQuad4+
                                        (AmongPrecipDev*WithinPrecip)+
                                        Latitude+(1|Year)+(1|SiteID)+
                                        ns(Centred_year, df=3)+
                                        (1+ns(Centred_year, df=3)|TreeID)+
                                        (1|Centred_year:SiteID),
                                      data=red_quercus_dataset)


#more complicated climate models 
#including all climatic variables
all_climate_model_quercus <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
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
                             data=red_quercus_dataset)

#combinations of different variables
#VPD + temp
VPD_temp_climate_model_quercus <- glmmTMB(log1p(Ring.width)~AmongVPDDev+AmongTempDev+
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
                                  data=red_quercus_dataset)

#VPD + precip
VPD_precip_climate_model_quercus <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+
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
                                    data=red_quercus_dataset)

#precip + temp
precip_temp_climate_model_quercus <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongTempDev+
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
                                     data=red_quercus_dataset)

#creating a dataframe to use in akaike.weights function 
AICdf_quercus <- matrix(nrow=18, ncol=6)
colnames(AICdf_quercus) <- c('Model', 'Number of Parameters (Fixed)', 'AIC', 'DeltaAIC', 'Weight', 'Species')

summary(null_model_quercus)
library(insight)
get_parameters(null_model_quercus)
AICdf_quercus[1,1] <- c('Null model')
AICdf_quercus[1,2] <- c('6')
AICdf_quercus[1,3] <- 9003.3 #taken from that calculated by glmmTMB in summary

summary(no_quads_temp_quercus)
get_parameters(no_quads_temp_quercus)
AICdf_quercus[2,1] <- c('All climate variables, excluding temporal quadratics')
AICdf_quercus[2,2] <- c('18')
AICdf_quercus[2,3] <- 8880.9

summary(no_quads_spat_quercus)
get_parameters(no_quads_spat_quercus)
AICdf_quercus[3,1] <- c('All climate variables, excluding spatial quadratics')
AICdf_quercus[3,2] <- c('18')
AICdf_quercus[3,3] <-  8891.4

summary(precip_quad_model_lat_year_quercus)
get_parameters(precip_quad_model_lat_year_quercus)
AICdf_quercus[4,1] <- c('Precipitation only')
AICdf_quercus[4,2] <- c('11')
AICdf_quercus[4,3] <-  8927.1 

summary(temperature_quad_model_lat_year_quercus)
AICdf_quercus[5,1] <- c('Mean temperature only')
AICdf_quercus[5,2] <- c('11')
AICdf_quercus[5,3] <- 8987.6

summary(VPD_quad_model_lat_year_quercus)
AICdf_quercus[6,1] <- c('VPD only')
AICdf_quercus[6,2] <- c('11')
AICdf_quercus[6,3] <- 8931.7 

summary(all_climate_model_quercus)
get_parameters(all_climate_model_quercus)
AICdf_quercus[7,1] <- c('All climate variables')
AICdf_quercus[7,2] <- c('21')
AICdf_quercus[7,3] <- 8882.5

summary(VPD_temp_climate_model_quercus)
get_parameters(VPD_temp_climate_model_quercus)
AICdf_quercus[8,1] <- c('VPD and mean temperature')
AICdf_quercus[8,2] <- c('16')
AICdf_quercus[8,3] <- 8912.7

summary(VPD_precip_climate_model_quercus)
AICdf_quercus[9,1] <- c('VPD and precipitation')
AICdf_quercus[9,2] <- c('16')
AICdf_quercus[9,3] <- 8905.7

summary(precip_temp_climate_model_quercus)
AICdf_quercus[10,1] <- c('Precipitation and mean temperature')
AICdf_quercus[10,2] <- c('16')
AICdf_quercus[10,3] <-  8914.3

summary(no_quadsTemp_temp_quercus)
get_parameters(no_quadsTemp_temp_quercus)
AICdf_quercus[11,1] <- c('All climate variables, excluding temporal temperature quadratic')
AICdf_quercus[11,2] <- c('20')
AICdf_quercus[11,3] <-8880.8

summary(no_quadsTemp_spat_quercus)
AICdf_quercus[12,1] <- c('All climate variables, excluding spatial temperature quadratic')
AICdf_quercus[12,2] <- c('20')
AICdf_quercus[12,3] <-8894.2

summary(no_quadsVPD_temp_quercus)
AICdf_quercus[13,1] <- c('All climate variables, excluding temporal VPD quadratic')
AICdf_quercus[13,2] <- c('20')
AICdf_quercus[13,3] <- 8880.5

summary(no_quadsVPD_spat_quercus)
AICdf_quercus[14,1] <- c('All climate variables, excluding spatial VPD quadratic')
AICdf_quercus[14,2] <- c('20')
AICdf_quercus[14,3] <- 8887.6

summary(no_quadsPrecip_temp_quercus)
AICdf_quercus[15,1] <- c('All climate variables, excluding temporal precipitation quadratic')
AICdf_quercus[15,2] <- c('20')
AICdf_quercus[15,3] <- 8884.3

summary(no_quadsPrecip_spat_quercus)
AICdf_quercus[16,1] <- c('All climate variables, excluding spatial precipitation quadratic')
AICdf_quercus[16,2] <- c('20')
AICdf_quercus[16,3] <- 8884.2

summary(global_model_quercus)
get_parameters(global_model_quercus)
AICdf_quercus[17,1] <- c('Raw climate model')
AICdf_quercus[17,2] <- c('12')
AICdf_quercus[17,3] <- 8899.0 

summary(global_model_no_quads_quercus)
get_parameters(global_model_no_quads_quercus)
AICdf_quercus[18,1] <- c('Raw climate model, excluding quadratics')
AICdf_quercus[18,2] <- c('9')
AICdf_quercus[18,3] <- 8899.6 

#calculating delta AIC and weights
library(qpcR)
weights_quercus <- akaike.weights(as.numeric(AICdf_quercus[,3]))
AICdf_quercus[,4] <- weights_quercus$deltaAIC
AICdf_quercus[,5] <- weights_quercus$weights
AICdf_quercus[,6] <- c("Quercus petraea")

AICdf_quercus <- as.data.frame(AICdf_quercus)

#adding quercus to excel sheet
AIC_df <- read.csv("Figures/AIC_scores.csv")
AIC_df <- AIC_df[,-1]
AIC_df <- rbind(AICdf_quercus, AIC_df)
write.csv(AICdf_quercus, "Figures/AIC_scores_2.csv")

#creating figure
library(ggplot2)
library(cowplot)
#nicer colour scheme
library(viridis)
#making delta AIC numeric
AICdf_quercus$DeltaAIC <- as.numeric(AICdf_quercus$DeltaAIC)
#rounding delta AIC to reasonable dp
AICdf_quercus$DeltaAIC_lab <- format(round(AICdf_quercus$DeltaAIC, 2), nsmall = 1)
#rearranging order of model
AICdf_quercus$Model <- factor(AICdf_quercus$Model, levels=c("Null model", "Mean temperature only",
                                                        "Precipitation only", "VPD only",
                                                        "Precipitation and mean temperature", "VPD and mean temperature",
                                                        "VPD and precipitation", "All climate variables",
                                                        "All climate variables, excluding temporal quadratics",
                                                        "All climate variables, excluding temporal temperature quadratic",
                                                        "All climate variables, excluding temporal precipitation quadratic",
                                                        "All climate variables, excluding temporal VPD quadratic",
                                                        "All climate variables, excluding spatial quadratics",
                                                        "All climate variables, excluding spatial temperature quadratic",
                                                        "All climate variables, excluding spatial precipitation quadratic",
                                                        "All climate variables, excluding spatial VPD quadratic",
                                                        "Raw climate model",
                                                        "Raw climate model, excluding quadratics"))

#creating figure
library(ggplot2)
library(cowplot)
library(viridis)
AIC_quercus_fig <- ggplot(data=AICdf_quercus, aes(x=Model, y=DeltaAIC, fill=Model))+
  geom_bar(stat="identity")+
  theme_cowplot()+
  theme(axis.text=element_text(size=10))+
  geom_text(aes(label=DeltaAIC_lab), vjust=-0.5, size = 2.5, position = position_dodge(0.9))+
  ylab("Delta AIC")+
  scale_fill_viridis(discrete=TRUE)+
  theme(legend.position="none")+
  ggtitle("Quercus petraea")+
  scale_x_discrete(labels= c(1:18))+
  theme(title=element_text(size=12),
        axis.title.x=element_blank())+
  theme(plot.title = element_text(face = "italic"))+
  ylim(0,130)

##plotting 
library(dplyr)

###among quad quercus
#####
#skeleton dataset for prediction of VPD
#keeping everything fixed apart from the among and among quad values so only thing that 
#varies is the average climatic predictor, to show the global/spatial relationship
med_year <- median(red_quercus_dataset$Centred_year)
med_yr <- 2000
med_withinVPD <- median(red_quercus_dataset$WithinVPD)
med_withinVPDQuad <- med_withinVPD^2
MinAmongVPDDev <- min(red_quercus_dataset$AmongVPDDev)
MaxAmongVPDDev <- max(red_quercus_dataset$AmongVPDDev)
pred_amongVPDDev <- seq(MinAmongVPDDev, MaxAmongVPDDev+0.05, 0.05)
pred_amongVPDDevQuad <- pred_amongVPDDev^2
tree_id <- as.character((red_quercus_dataset$TreeID[1])) 
site_id <- as.character((red_quercus_dataset$SiteID[1]))
med_lat <- red_quercus_dataset$Latitude[1]
med_amongTempDev <- median(red_quercus_dataset$AmongTempDev)
med_amongTempDevQuad <- med_amongTempDev^2
med_amongPrecipDev <- median(red_quercus_dataset$AmongPrecipDev)
med_amongPrecipDevQuad <- med_amongPrecipDev^2
med_withinTemp <- median(red_quercus_dataset$WithinTemp)
med_withinTempQuad <- med_withinTemp^2
med_withinPrecip <- median(red_quercus_dataset$WithinPrecip)
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
med_year <- median(red_quercus_dataset$Centred_year)
med_yr <- 2000
med_withinTemp <- median(red_quercus_dataset$WithinTemp)
med_withinTempQuad <- med_withinTemp^2
MinAmongTempDev <- min(red_quercus_dataset$AmongTempDev)
MaxAmongTempDev <- max(red_quercus_dataset$AmongTempDev)
pred_amongTempDev <- seq(MinAmongTempDev, MaxAmongTempDev+1, 0.5)
pred_amongTempDevQuad <- pred_amongTempDev^2
tree_id <- as.character((red_quercus_dataset$TreeID[1])) 
site_id <- as.character((red_quercus_dataset$SiteID[1]))
med_lat <- red_quercus_dataset$Latitude[1]
med_amongVPDDev <- median(red_quercus_dataset$AmongVPDDev)
med_amongVPDDevQuad <- med_amongVPDDev^2
med_amongDevPrecip <- median(red_quercus_dataset$AmongPrecipDev)
med_amongPrecipDevQuad <- med_amongPrecipDev^2
med_withinVPD <- median(red_quercus_dataset$WithinVPD)
med_withinVPDquad <- med_withinVPD^2
med_withinPrecip <- median(red_quercus_dataset$WithinPrecip)
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
med_year <- median(red_quercus_dataset$Centred_year)
med_yr <- 2000
med_withinPrecip <- median(red_quercus_dataset$WithinPrecip)
med_withinPrecipQuad <- med_withinPrecip^2
MinAmongPrecipDev <- min(red_quercus_dataset$AmongPrecipDev)
MaxAmongPrecipDev <- max(red_quercus_dataset$AmongPrecipDev)
pred_amongPrecipDev <- seq(MinAmongPrecipDev, MaxAmongPrecipDev+0.1, 0.1)
pred_amongPrecipDevQuad <- pred_amongPrecipDev^2
tree_id <- as.character((red_quercus_dataset$TreeID[1])) 
site_id <- as.character((red_quercus_dataset$SiteID[1]))
med_lat <- red_quercus_dataset$Latitude[1]
med_amongVPDDev <- median(red_quercus_dataset$AmongVPDDev)
med_amongVPDDevQuad <- med_amongVPDDev^2
med_amongTempDev <- median(red_quercus_dataset$AmongTempDev)
med_amongTempDevQuad <- med_amongTempDev^2
med_withinVPD <- median(red_quercus_dataset$WithinVPD)
med_withinVPDQuad <- med_withinVPD^2
med_withinTemp <- median(red_quercus_dataset$WithinTemp)
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
VPD_pred_among_quercus <-predict(all_climate_model_quercus, newdata = newdataAmongVPD, re.form=NULL, se.fit=TRUE)
temp_pred_among_quercus <-predict(all_climate_model_quercus, newdata = newdataAmongTemp, re.form=NULL, se.fit=TRUE)
precip_pred_among_quercus <-predict(all_climate_model_quercus, newdata = newdataAmongPrecip, re.form=NULL, se.fit=TRUE)

#adding predicted points to prediction dataframe
newdataAmongVPD$Prediction <- as.numeric(VPD_pred_among_quercus$fit)
newdataAmongVPD$SE <- as.numeric(VPD_pred_among_quercus$se.fit)
newdataAmongTemp$Prediction  <- as.numeric(temp_pred_among_quercus$fit)
newdataAmongTemp$SE <- as.numeric(temp_pred_among_quercus$se.fit)
newdataAmongPrecip$Prediction  <- as.numeric(precip_pred_among_quercus$fit)
newdataAmongPrecip$SE <- as.numeric(precip_pred_among_quercus$se.fit)

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
amongmeantemps <- seq(MinAmongTempDev, MaxAmongTempDev, 2) 
within_points_df <- list()

#creating new dataframe where among is now fixed but within varies along a sequence ranging from
#the minimum to maximum deviation within the dataset
for(i in 1:length(amongmeantemps)){
  med_amongTempDev <- amongmeantemps[i]
  med_amongTempDevQuad <- med_amongTempDev^2
  
  MinWithinTemp <- min(red_quercus_dataset$WithinTemp)
  MaxWithinTemp <- max(red_quercus_dataset$WithinTemp)
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
                              AvgAmongTemp = rep(unique(as.numeric(red_quercus_dataset$AvgAmongTemp)), length(pred_withinTemp)))
  
  newdatasubset$ActualAmongTemp <- newdatasubset$AvgAmongTemp+newdatasubset$AmongTempDev
  newdatasubset$ActualTemp <- newdatasubset$ActualAmongTemp+newdatasubset$WithinTemp
  newdatasubset$WithinGlobDev <- newdatasubset$ActualTemp-newdatasubset$AvgAmongTemp
  
  temp_pred_subset <-predict(all_climate_model_quercus, newdata = newdatasubset, re.form=NULL, se.fit=TRUE)
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
quercus_temp <- ggplot(data=red_quercus_dataset, aes(x=AmongTemp, y=log1p(Ring.width)))+
  geom_line(data = data.frame(AmongTempDev = newdataAmongTemp$AmongTempDev, Ring.width = newdataAmongTemp$Prediction), aes(x = AmongTempDev, y = Ring.width), colour = "blue")+
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
  xlab("Deviation from global average temperature (°C)")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12))+
  ylim(4,8)+
  xlim(-8, 6)

quercus_temp
#for VPD
amongVPDs <- seq(MinAmongVPDDev, MaxAmongVPDDev, 0.15) 
within_points_df <- list()

for(i in 1:length(amongVPDs)){
  med_amongVPDDev <- amongVPDs[i]
  med_amongVPDDevQuad <- med_amongVPDDev^2
  
  MinWithinVPD <- min(red_quercus_dataset$WithinVPD)
  MaxWithinVPD <- max(red_quercus_dataset$WithinVPD)
  pred_withinVPD <- seq(MinWithinVPD, MaxWithinVPD, 0.025)
  pred_withinVPDQuad <- pred_withinVPD^2
  
  med_withinTemp <- median(red_quercus_dataset$WithinTemp)
  med_withinTempQuad <- med_withinTemp^2
  med_amongTempDev <- median(red_quercus_dataset$AmongTempDev)
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
                              AvgAmongVPD = rep(unique(as.numeric(red_quercus_dataset$AvgAmongVPD)), length(pred_withinVPD)))
  
  newdatasubset$ActualAmongVPD <- newdatasubset$AvgAmongVPD+newdatasubset$AmongVPDDev
  newdatasubset$ActualVPD <- newdatasubset$ActualAmongVPD+newdatasubset$WithinVPD
  newdatasubset$WithinGlobDev <- newdatasubset$ActualVPD-newdatasubset$AvgAmongVPD
  
  VPD_pred_subset <-predict(all_climate_model_quercus, newdata = newdatasubset, re.form=NULL, se.fit=TRUE)
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

#creating figure with a curve for among/spatial, and four curves for within/temporal
library(ggplot2)
library(cowplot)
quercus_VPD <- ggplot(data=red_quercus_dataset, aes(x=AmongVPD, y=log1p(Ring.width)))+
  geom_line(data = data.frame(AmongVPDDev = newdataAmongVPD$AmongVPDDev, Ring.width = newdataAmongVPD$Prediction), aes(x = AmongVPDDev, y = Ring.width), colour = "blue")+
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
  xlab("Deviation from global average VPD (kPa)")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  ggtitle("Quercus petraea")+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12), 
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))+
  ylim(2,6)+
  xlim(-0.5, 1)

quercus_VPD

#for precip
amongPrecips <- seq(MinAmongPrecipDev+0.2, MaxAmongPrecipDev, 0.4) 
within_points_df <- list()


for(i in 1:length(amongPrecips)){
  med_year <- median(red_quercus_dataset$Centred_year)
  med_yr <- 2000
  med_amongPrecipDev <- amongPrecips[i]
  med_amongPrecipDevQuad <- med_amongPrecipDev^2
  tree_id <- as.character((red_quercus_dataset$TreeID[1]))
  site_id <- as.character((red_quercus_dataset$SiteID[1]))
  med_lat <- median(red_quercus_dataset$Latitude)
  
  MinWithinPrecip <- min(red_quercus_dataset$WithinPrecip)
  MaxWithinPrecip <- max(red_quercus_dataset$WithinPrecip)
  pred_withinPrecip <- seq(MinWithinPrecip, MaxWithinPrecip, 0.25)
  pred_withinPrecipQuad <- pred_withinPrecip^2
  
  med_withinVPD <- median(red_quercus_dataset$WithinVPD)
  med_withinVPDquad <- med_withinVPD^2
  med_amongVPDDev <- median(red_quercus_dataset$AmongVPDDev)
  med_amongVPDDevQuad <- med_amongVPDDev^2
  
  med_withinTemp <- median(red_quercus_dataset$WithinTemp)
  med_withinTempQuad <- med_withinTemp^2
  med_amongTempDev <- median(red_quercus_dataset$AmongTempDev)
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
                              AvgAmongPrecip = rep(unique(as.numeric(red_quercus_dataset$AvgAmongPrecip)), length(pred_withinPrecip)))
  
  newdatasubset$ActualAmongPrecip <- newdatasubset$AvgAmongPrecip+newdatasubset$AmongPrecipDev
  newdatasubset$ActualPrecip <- newdatasubset$ActualAmongPrecip+newdatasubset$WithinPrecip
  newdatasubset$WithinGlobDev <- newdatasubset$ActualPrecip-newdatasubset$AvgAmongPrecip
  
  Precip_pred_subset <-predict(all_climate_model_quercus, newdata = newdatasubset, re.form=NULL, se.fit=TRUE)
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
quercus_Precip <- ggplot(data=red_quercus_dataset, aes(x=AmongPrecip, y=log1p(Ring.width)))+
  geom_line(data = data.frame(AmongPrecipDev = newdataAmongPrecip$AmongPrecipDev, Ring.width = newdataAmongPrecip$Prediction), aes(x = AmongPrecipDev, y = Ring.width), colour = "blue")+
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
  theme(axis.title.y=element_text(size=12))+
  ylim(4,6)+
  xlim(-4, 6)

quercus_Precip

