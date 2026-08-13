#for more detailed annotation, see amongdev_fagus
library(dplyr)
#mean centring climate data and calculating quadratics
#####
betula_dataset <- read.csv("Data/e_obs/climate_dataset_betula.csv")

#these sites had no rel hum readings so VPD could not be calculated and therefore
#excluding from dataset

mean_centred_betula_dataset <- data.frame()
betula_populations <- unique(betula_dataset$SiteID)
betula_populations[13]
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

#checking for correlation between climate variables
cor.test(mean_centred_betula_dataset$Avg_meantemp, mean_centred_betula_dataset$Avg_VPD)
cor.test(mean_centred_betula_dataset$Avg_precip, mean_centred_betula_dataset$Avg_VPD)
cor.test(mean_centred_betula_dataset$Avg_meantemp, mean_centred_betula_dataset$Avg_precip)
cor.test(mean_centred_betula_dataset$WithinTemp, mean_centred_betula_dataset$WithinVPD)
cor.test(mean_centred_betula_dataset$AmongTemp, mean_centred_betula_dataset$AmongVPD)
cor.test(mean_centred_betula_dataset$AmongTemp, mean_centred_betula_dataset$AmongPrecip)
cor.test(mean_centred_betula_dataset$WithinTemp, mean_centred_betula_dataset$WithinPrecip)
cor.test(mean_centred_betula_dataset$AmongPrecip, mean_centred_betula_dataset$AmongVPD)
cor.test(mean_centred_betula_dataset$WithinPrecip, mean_centred_betula_dataset$WithinVPD)

#null model with no climate drivers 
null_model_betula <- glmmTMB(log1p(Ring.width)~Latitude+(1|Year)+(1|SiteID)+
                                ns(Centred_year, df=3)+
                                (1+ns(Centred_year, df=3)|TreeID)+
                                (1|Centred_year:SiteID),
                              data=mean_centred_betula_dataset)

#dropping quadratics to check assumption of optimum climate value
no_quads_spat_betula <- glmmTMB(log1p(Ring.width)~AmongTempDev+WithinTemp+WithinTempQuad4+
                                   AmongVPDDev+WithinVPD+WithinVPDQuad4+
                                   AmongPrecipDev+WithinPrecip+WithinPrecipQuad4+
                                   (WithinTemp*AmongTempDev)+
                                  (WithinPrecip*AmongPrecipDev)+
                                  (WithinVPD*AmongVPDDev)+
                                   Latitude+(1|Year)+(1|SiteID)+
                                   ns(Centred_year, df=3)+
                                   (1+ns(Centred_year, df=3)|TreeID)+
                                   (1|Centred_year:SiteID),
                                 data=mean_centred_betula_dataset)
#dropping temperature quadratic (spatial)
no_quadsTemp_spat_betula <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
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
                                     data=mean_centred_betula_dataset)
#dropping VPD quadratic (spatial)
no_quadsVPD_spat_betula <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
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
                                    data=mean_centred_betula_dataset)
#dropping precipitation quadratic (spatial)
no_quadsPrecip_spat_betula <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
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
                                       data=mean_centred_betula_dataset)


#dropping quadratics to check assumption of optimum climate value (temporal)
no_quads_temp_betula <- glmmTMB(log1p(Ring.width)~AmongTempDev+WithinTemp+AmongTempDevQuad+
                                   AmongVPDDev+WithinVPD+AmongVPDDevQuad+
                                   AmongPrecipDev+WithinPrecip+AmongPrecipDevQuad+
                                   (WithinTemp*AmongTempDev)+
                                  (AmongPrecipDev*WithinPrecip)+
                                  (AmongVPDDev*WithinVPD)+
                                   Latitude+(1|Year)+(1|SiteID)+
                                   ns(Centred_year, df=3)+
                                   (1+ns(Centred_year, df=3)|TreeID)+
                                   (1|Centred_year:SiteID),
                                 data=mean_centred_betula_dataset)
#dropping temperature quadratic (temporal)
no_quadsTemp_temp_betula <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
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
                                     data=mean_centred_betula_dataset)
#dropping VPD quadratic (temporal)
no_quadsVPD_temp_betula <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
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
                                    data=mean_centred_betula_dataset)
#dropping precipitation quadratic (temporal)
no_quadsPrecip_temp_betula <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
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
                                       data=mean_centred_betula_dataset)

#global model to check splitting temporal and spatial adds value
#took out within as well
global_model_betula <- glmmTMB(log1p(Ring.width)~Avg_precip+Avg_meantemp+Avg_VPD+
                                  Avg_precipQuad+Avg_meantempQuad+Avg_VPDQuad+
                                  Latitude+
                                  (1|Year)+(1|SiteID)+
                                  ns(Centred_year, df=3)+
                                  (1+ns(Centred_year, df=3)|TreeID)+
                                  (1|Centred_year:SiteID),
                                data=mean_centred_betula_dataset)

summary(global_model_betula)

#creating the fixed skeleton dataframes for the prediction
med_yr <- 2000
med_year <- median(mean_centred_betula_dataset$Centred_year)
med_lat <- median(mean_centred_betula_dataset$Latitude)
tree_id <- mean_centred_betula_dataset$SiteID[1]
site_id <- mean_centred_betula_dataset$TreeID[1]
med_temp <- median(mean_centred_betula_dataset$Avg_meantemp)
med_VPD <- median(mean_centred_betula_dataset$Avg_VPD)
med_precip <- median(mean_centred_betula_dataset$Avg_precip)
med_temp_quad <- med_temp^2
med_VPD_quad <- med_VPD^2
med_precip_quad <- med_precip^2
pred_temp <- seq(min(mean_centred_betula_dataset$Avg_meantemp), max(mean_centred_betula_dataset$Avg_meantemp), 2)
pred_VPD <- seq(min(mean_centred_betula_dataset$Avg_VPD), max(mean_centred_betula_dataset$Avg_VPD), 0.1)
pred_precip <- seq(min(mean_centred_betula_dataset$Avg_precip), max(mean_centred_betula_dataset$Avg_precip), 0.5)
pred_temp_quad <- pred_temp^2
pred_VPD_quad <- pred_VPD^2
pred_precip_quad <- pred_precip^2

global_model_dataset_temp_betula <- data.frame(Year=rep(med_yr, length(pred_temp)),
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

global_model_dataset_VPD_betula <- data.frame(Year=rep(med_yr, length(pred_VPD)),
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

global_model_dataset_precip_betula <- data.frame(Year=rep(med_yr, length(pred_precip)),
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
global_prediction_temp_betula <- predict(global_model_betula, newdata = global_model_dataset_temp_betula, re.form=NULL, se.fit=TRUE)
global_prediction_precip_betula <- predict(global_model_betula, newdata = global_model_dataset_precip_betula, re.form=NULL, se.fit=TRUE)
global_prediction_VPD_betula <- predict(global_model_betula, newdata = global_model_dataset_VPD_betula, re.form=NULL, se.fit=TRUE)
#putting predictions and se into dataframes
global_model_dataset_temp_betula$pred <- global_prediction_temp_betula$fit
global_model_dataset_precip_betula$pred <- global_prediction_precip_betula$fit
global_model_dataset_VPD_betula$pred <- global_prediction_VPD_betula$fit
global_model_dataset_temp_betula$SE <- global_prediction_temp_betula$se.fit
global_model_dataset_precip_betula$SE <- global_prediction_precip_betula$se.fit
global_model_dataset_VPD_betula$SE <- global_prediction_VPD_betula$se.fit
#calculating confidence intervals
global_model_dataset_temp_betula$LCI <- global_model_dataset_temp_betula$pred-1.96*global_model_dataset_temp_betula$SE
global_model_dataset_temp_betula$HCI <- global_model_dataset_temp_betula$pred+1.96*global_model_dataset_temp_betula$SE
global_model_dataset_VPD_betula$LCI <- global_model_dataset_VPD_betula$pred-1.96*global_model_dataset_VPD_betula$SE
global_model_dataset_VPD_betula$HCI <- global_model_dataset_VPD_betula$pred+1.96*global_model_dataset_VPD_betula$SE
global_model_dataset_precip_betula$LCI <- global_model_dataset_precip_betula$pred-1.96*global_model_dataset_precip_betula$SE
global_model_dataset_precip_betula$HCI <- global_model_dataset_precip_betula$pred+1.96*global_model_dataset_precip_betula$SE

#making figures for this
bet_raw_precip <- ggplot(data=global_model_dataset_precip_betula, aes(x=Avg_precip, y=pred))+
  geom_line(colour="mediumpurple3", linetype="dotted")+
  geom_line(data=global_model_dataset_precip_betula, aes(x=Avg_precip, y=HCI), colour = "pink", linetype = "dashed")+
  geom_line(data=global_model_dataset_precip_betula, aes(x=Avg_precip, y=LCI), colour = "pink", linetype = "dashed")+
  xlab("Average daily precipitation (mm)")+
  ylab("Ring width (log+1)")+
  theme_cowplot()+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))+
  ylim(3, 7)+
  xlim(1, 8)

bet_raw_temp <- ggplot(data=global_model_dataset_temp_betula, aes(x=Avg_meantemp, y=pred))+
  geom_line(col="mediumpurple3")+
  geom_line(data=global_model_dataset_temp_betula, aes(x=Avg_meantemp, y=HCI), colour = "pink", linetype = "dashed")+
  geom_line(data=global_model_dataset_temp_betula, aes(x=Avg_meantemp, y=LCI), colour = "pink", linetype = "dashed")+
  xlab("Average daily mean temperature (°C)")+
  ylab("Ring width (log+1)")+
  theme_cowplot()+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))+
  ylim(3, 7)+
  xlim(9, 21)

bet_raw_VPD <- ggplot(data=global_model_dataset_VPD_betula, aes(x=Avg_VPD, y=pred))+
  geom_line(colour="mediumpurple3", linetype="dotted")+
  geom_line(data=global_model_dataset_VPD_betula, aes(x=Avg_VPD, y=HCI), colour = "pink", linetype = "dashed")+
  geom_line(data=global_model_dataset_VPD_betula, aes(x=Avg_VPD, y=LCI), colour = "pink", linetype = "dashed")+
  xlab("Average daily VPD (kPa)")+
  ylab("Ring width (log+1)")+
  ggtitle("Betula pendula")+
  theme_cowplot()+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))+
  ylim(3, 7)+
  xlim(0.2, 1)


#dropping quadratics
global_model_no_quads_betula <- glmmTMB(log1p(Ring.width)~Avg_precip+Avg_meantemp+Avg_VPD+
                                           Latitude+
                                           (1|Year)+(1|SiteID)+
                                           ns(Centred_year, df=3)+
                                           (1+ns(Centred_year, df=3)|TreeID)+
                                           (1|Centred_year:SiteID),
                                         data=mean_centred_betula_dataset)

##final model
##latitude as fixed effect, year and siteID as random effect
#individual models for each climate driver 
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


#more complicated climate models 
#including all climatic variables
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

#combinations of different variables
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

#creating a dataframe to use in akaike.weights function 
AICdf_betula <- matrix(nrow=18, ncol=6)
colnames(AICdf_betula) <- c('Model', 'Number of Parameters (Fixed)', 'AIC', 'DeltaAIC', 'Weight', 'Species')

summary(null_model_betula)
library(insight)
get_parameters(null_model_betula)
AICdf_betula[1,1] <- c('Null model')
AICdf_betula[1,2] <- c('6')
AICdf_betula[1,3] <- 19597.9 #taken from that calculated by glmmTMB in summary

summary(no_quads_temp_betula)
get_parameters(no_quads_temp_betula)
AICdf_betula[2,1] <- c('All climate variables, excluding temporal quadratics')
AICdf_betula[2,2] <- c('18')
AICdf_betula[2,3] <- 19596.3

summary(no_quads_spat_betula)
get_parameters(no_quads_spat_betula)
AICdf_betula[3,1] <- c('All climate variables, excluding spatial quadratics')
AICdf_betula[3,2] <- c('18')
AICdf_betula[3,3] <-  19597.2

summary(precip_quad_model_lat_year_betula)
get_parameters(precip_quad_model_lat_year_betula)
AICdf_betula[4,1] <- c('Precipitation only')
AICdf_betula[4,2] <- c('11')
AICdf_betula[4,3] <-  19594.6 

summary(temperature_quad_model_lat_year_betula)
AICdf_betula[5,1] <- c('Mean temperature only')
AICdf_betula[5,2] <- c('11')
AICdf_betula[5,3] <- 19594.0

summary(VPD_quad_model_lat_year_betula)
AICdf_betula[6,1] <- c('VPD only')
AICdf_betula[6,2] <- c('11')
AICdf_betula[6,3] <- 19597.5

summary(all_climate_model_betula)
get_parameters(all_climate_model_betula)
AICdf_betula[7,1] <- c('All climate variables')
AICdf_betula[7,2] <- c('21')
AICdf_betula[7,3] <- 19598.8

summary(VPD_temp_climate_model_betula)
get_parameters(VPD_temp_climate_model_betula)
AICdf_betula[8,1] <- c('VPD and mean temperature')
AICdf_betula[8,2] <- c('16')
AICdf_betula[8,3] <- 19596.0

summary(VPD_precip_climate_model_betula)
AICdf_betula[9,1] <- c('VPD and precipitation')
AICdf_betula[9,2] <- c('16')
AICdf_betula[9,3] <- 19599.5

summary(precip_temp_climate_model_betula)
AICdf_betula[10,1] <- c('Precipitation and mean temperature')
AICdf_betula[10,2] <- c('16')
AICdf_betula[10,3] <-  19592.4 

summary(no_quadsTemp_temp_betula)
get_parameters(no_quadsTemp_temp_betula)
AICdf_betula[11,1] <- c('All climate variables, excluding temporal temperature quadratic')
AICdf_betula[11,2] <- c('20')
AICdf_betula[11,3] <- 19596.8

summary(no_quadsTemp_spat_betula)
AICdf_betula[12,1] <- c('All climate variables, excluding spatial temperature quadratic')
AICdf_betula[12,2] <- c('20')
AICdf_betula[12,3] <- 19599.9

summary(no_quadsVPD_temp_betula)
AICdf_betula[13,1] <- c('All climate variables, excluding temporal VPD quadratic')
AICdf_betula[13,2] <- c('20')
AICdf_betula[13,3] <- 19597.2

summary(no_quadsVPD_spat_betula)
AICdf_betula[14,1] <- c('All climate variables, excluding spatial VPD quadratic')
AICdf_betula[14,2] <- c('20')
AICdf_betula[14,3] <- 19597.0 

summary(no_quadsPrecip_temp_betula)
AICdf_betula[15,1] <- c('All climate variables, excluding temporal precipitation quadratic')
AICdf_betula[15,2] <- c('20')
AICdf_betula[15,3] <- 19599.1

summary(no_quadsPrecip_spat_betula)
AICdf_betula[16,1] <- c('All climate variables, excluding spatial precipitation quadratic')
AICdf_betula[16,2] <- c('20')
AICdf_betula[16,3] <- 19597.4 

summary(global_model_betula)
get_parameters(global_model_betula)
AICdf_betula[17,1] <- c('Raw climate model')
AICdf_betula[17,2] <- c('12')
AICdf_betula[17,3] <- 19589.6

summary(global_model_no_quads_betula)
get_parameters(global_model_no_quads_betula)
AICdf_betula[18,1] <- c('Raw climate model, excluding quadratics')
AICdf_betula[18,2] <- c('9')
AICdf_betula[18,3] <- 19591.2 

#calculating delta AIC and weights
library(qpcR)
weights_betula <- akaike.weights(as.numeric(AICdf_betula[,3]))
AICdf_betula[,4] <- weights_betula$deltaAIC
AICdf_betula[,5] <- weights_betula$weights
AICdf_betula[,6] <- c("Betula pendula")

#adding betula to excel sheet
AIC_df <- read.csv("Figures/AIC_scores.csv")
AIC_df <- AIC_df[,-1]
AIC_df <- rbind(AIC_df, AICdf_betula)
write.csv(AICdf_betula, "Figures/AIC_scores_3.csv")

#creating figure
library(ggplot2)
library(cowplot)
#nicer colour scheme
library(viridis)

#going from matrix to df
AICdf_betula <- as.data.frame(AICdf_betula)
#making delta AIC numeric
AICdf_betula$DeltaAIC <- as.numeric(AICdf_betula$DeltaAIC)
#rounding delta AIC to reasonable dp
AICdf_betula$DeltaAIC_lab <- format(round(AICdf_betula$DeltaAIC, 2), nsmall = 1)
#rearranging model order
AICdf_betula$Model <- factor(AICdf_betula$Model, levels=c("Null model", "Mean temperature only",
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
AIC_betula_fig <- ggplot(data=AICdf_betula, aes(x=Model, y=DeltaAIC, fill=Model))+
  geom_bar(position="dodge", stat="identity")+
  theme_cowplot()+
  theme(axis.text=element_text(size=10), legend.text = element_text(size=10))+
  geom_text(aes(label=DeltaAIC_lab), vjust=-0.5, size = 2.5, position = position_dodge(0.9))+
  ylab("Delta AIC")+
  theme(legend.position="none")+
  scale_fill_viridis(discrete=TRUE)+
  ggtitle("Betula pendula")+
  scale_x_discrete(labels= c(1:18))+
  theme(title=element_text(size=12),
        axis.title.x=element_blank())+
  theme(plot.title = element_text(face = "italic"))+
  ylim(0,130)

#creating grid 
plot_grid(AIC_fagus_fig, AIC_quercus_fig, AIC_betula_fig, ncol=3, nrow=1)+
  draw_label("Model", x=0.5, y=  0, vjust=-0.5, angle= 0) +
  draw_label("Delta AIC", x=  0, y=0.5, vjust= 1.5, angle=90)
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
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        title=element_text(size=12))+
  ylim(2, 6)+
  xlim(-8, 6)

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
  ggtitle("Betula pendula")+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))+
  ylim(2,6)+
  xlim(-0.5, 1)

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
  theme(axis.title.y=element_text(size=12))+
  ylim(2,4)+
  xlim(-4, 6)

betula_Precip

summary(all_climate_model_betula)

