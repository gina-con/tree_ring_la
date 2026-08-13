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

#checking correlation between climatic variables by calculating Pearson correlation coefficients 
cor.test(red_fagus_dataset$Avg_meantemp, red_fagus_dataset$Avg_VPD)
cor.test(red_fagus_dataset$Avg_precip, red_fagus_dataset$Avg_VPD)
cor.test(red_fagus_dataset$Avg_meantemp, red_fagus_dataset$Avg_precip)
#checking correlation between climatic variables across space and time 
cor.test(red_fagus_dataset$AmongTemp, red_fagus_dataset$AmongVPD)
cor.test(red_fagus_dataset$WithinTemp, red_fagus_dataset$WithinVPD)
cor.test(red_fagus_dataset$AmongTemp, red_fagus_dataset$AmongPrecip)
cor.test(red_fagus_dataset$WithinTemp, red_fagus_dataset$WithinPrecip)
cor.test(red_fagus_dataset$AmongPrecip, red_fagus_dataset$AmongVPD)
cor.test(red_fagus_dataset$WithinPrecip, red_fagus_dataset$WithinVPD)

#null model with no climate drivers
#including fixed effect for latitude, spline terms including random effect for intercept and slope for TreeID
#and random Year and SiteID intercept terms
#log + 1 ring width to normalise residuals and account for impossibility of negative values and presence of 0s
null_model <- glmmTMB(log1p(Ring.width)~Latitude+(1|Year)+(1|SiteID)+
                                             ns(Centred_year, df=3)+
                                             (1+ns(Centred_year, df=3)|TreeID)+
                                             (1|Centred_year:SiteID),
                                           data=red_fagus_dataset)

#dropping spatial quadratics to check assumption of optimum climate value
#including fixed effect for latitude, spline terms including random effect for intercept and slope for TreeID
#and random Year and SiteID intercept terms
#including linear terms for spatial variation (Among[]Dev) and temporal variation (Within[])
#including quadratic terms for temporal variation (Within[]Quad4)
#including interaction between linear temporal and spatial variation
#log + 1 ring width to normalise residuals and account for impossibility of negative values and presence of 0s
no_quads_spat <- glmmTMB(log1p(Ring.width)~AmongTempDev+WithinTemp+WithinTempQuad4+
                      AmongVPDDev+WithinVPD+WithinVPDQuad4+
                      AmongPrecipDev+WithinPrecip+WithinPrecipQuad4+
                                             (WithinTemp*AmongTempDev)+
                        (WithinVPD*AmongVPDDev)+
                        (WithinPrecip*AmongPrecipDev)+
                                             Latitude+(1|Year)+(1|SiteID)+
                                             ns(Centred_year, df=3)+
                                             (1+ns(Centred_year, df=3)|TreeID)+
                                             (1|Centred_year:SiteID),
                                           data=red_fagus_dataset)
#dropping temperature quadratic (spatial)
#including fixed effect for latitude, spline terms including random effect for intercept and slope for TreeID
#and random Year and SiteID intercept terms
#including linear terms for spatial variation (Among[]Dev) and temporal variation (Within[])
#including quadratic terms for temporal variation (Within[]Quad4)
#including interaction between linear temporal and spatial variation
#including quadratic terms for spatial variation (Among[]DevQuad) except for temperature
#log + 1 ring width to normalise residuals and account for impossibility of negative values and presence of 0s
no_quadsTemp_spat <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
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
                             data=red_fagus_dataset)
#dropping VPD quadratic (spatial)
#including fixed effect for latitude, spline terms including random effect for intercept and slope for TreeID
#and random Year and SiteID intercept terms
#including linear terms for spatial variation (Among[]Dev) and temporal variation (Within[])
#including quadratic terms for temporal variation (Within[]Quad4)
#including interaction between linear temporal and spatial variation
#including quadratic terms for spatial variation (Among[]DevQuad) except for VPD
#log + 1 ring width to normalise residuals and account for impossibility of negative values and presence of 0s
no_quadsVPD_spat <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
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
                             data=red_fagus_dataset)
#including fixed effect for latitude, spline terms including random effect for intercept and slope for TreeID
#and random Year and SiteID intercept terms
#including linear terms for spatial variation (Among[]Dev) and temporal variation (Within[])
#including quadratic terms for temporal variation (Within[]Quad4)
#including interaction between linear temporal and spatial variation
#including quadratic terms for spatial variation (Among[]DevQuad) except for precipitation
#log + 1 ring width to normalise residuals and account for impossibility of negative values and presence of 0s
no_quadsPrecip_spat <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
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
                             data=red_fagus_dataset)


#including fixed effect for latitude, spline terms including random effect for intercept and slope for TreeID
#and random Year and SiteID intercept terms
#including linear terms for spatial variation (Among[]Dev) and temporal variation (Within[])
#including quadratic terms for spatial variation (Among[]DevQuad)
#including interaction between linear temporal and spatial variation
#log + 1 ring width to normalise residuals and account for impossibility of negative values and presence of 0s
no_quads_temp <- glmmTMB(log1p(Ring.width)~AmongTempDev+WithinTemp+AmongTempDevQuad+
                      AmongVPDDev+WithinVPD+AmongVPDDevQuad+
                      AmongPrecipDev+WithinPrecip+AmongPrecipDevQuad+
                      (WithinTemp*AmongTempDev)+
                        (WithinVPD*AmongVPDDev)+
                        (WithinPrecip*AmongPrecipDev)+
                      Latitude+(1|Year)+(1|SiteID)+
                      ns(Centred_year, df=3)+
                      (1+ns(Centred_year, df=3)|TreeID)+
                      (1|Centred_year:SiteID),
                    data=red_fagus_dataset)
#including fixed effect for latitude, spline terms including random effect for intercept and slope for TreeID
#and random Year and SiteID intercept terms
#including linear terms for spatial variation (Among[]Dev) and temporal variation (Within[])
#including quadratic terms for spatial variation (Among[]DevQuad)
#including interaction between linear temporal and spatial variation
#including quadratic terms for temporal variation (Among[]DevQuad) except for temperature
#log + 1 ring width to normalise residuals and account for impossibility of negative values and presence of 0s
no_quadsTemp_temp <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
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
                        data=red_fagus_dataset)
#including fixed effect for latitude, spline terms including random effect for intercept and slope for TreeID
#and random Year and SiteID intercept terms
#including linear terms for spatial variation (Among[]Dev) and temporal variation (Within[])
#including quadratic terms for spatial variation (Among[]DevQuad)
#including interaction between linear temporal and spatial variation
#including quadratic terms for temporal variation (Among[]DevQuad) except for VPD
#log + 1 ring width to normalise residuals and account for impossibility of negative values and presence of 0s
no_quadsVPD_temp <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
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
                       data=red_fagus_dataset)
#including fixed effect for latitude, spline terms including random effect for intercept and slope for TreeID
#and random Year and SiteID intercept terms
#including linear terms for spatial variation (Among[]Dev) and temporal variation (Within[])
#including quadratic terms for spatial variation (Among[]DevQuad)
#including interaction between linear temporal and spatial variation
#including quadratic terms for temporal variation (Among[]DevQuad) except for precipitation
#log + 1 ring width to normalise residuals and account for impossibility of negative values and presence of 0s
no_quadsPrecip_temp <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+AmongVPDDev+AmongTempDev+
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
                          data=red_fagus_dataset)

#raw climate model to check splitting temporal and spatial adds value
#and random Year and SiteID intercept terms
#including linear terms for average climate variables (averaged across April-August)
#including quadratic terms for average climate variables (averaged across April-August)
#log + 1 ring width to normalise residuals and account for impossibility of negative values and presence of 0s
global_model <- glmmTMB(log1p(Ring.width)~Avg_precip+Avg_meantemp+Avg_VPD+
                          Avg_precipQuad+Avg_meantempQuad+Avg_VPDQuad+
                          Latitude+
                          (1|Year)+(1|SiteID)+
                          ns(Centred_year, df=3)+
                          (1+ns(Centred_year, df=3)|TreeID)+
                          (1|Centred_year:SiteID),
                        data=red_fagus_dataset)
summary(global_model)

#plotting the output of this raw climate model to infer climate relationships with tree ring width 
#as this was best performing model for FS and BP 

#creating the fixed skeleton dataframes for the prediction
#fixed continuous variables at median, year at 2000, tree ID at first tree ID in dataset, site ID at first site ID in dataset
med_yr <- 2000
med_year <- median(red_fagus_dataset$Centred_year)
med_lat <- median(red_fagus_dataset$Latitude)
tree_id <- red_fagus_dataset$SiteID[1]
site_id <- red_fagus_dataset$TreeID[1]
med_temp <- median(red_fagus_dataset$Avg_meantemp)
med_VPD <- median(red_fagus_dataset$Avg_VPD)
med_precip <- median(red_fagus_dataset$Avg_precip)
med_temp_quad <- med_temp^2
med_VPD_quad <- med_VPD^2
med_precip_quad <- med_precip^2
pred_temp <- seq(min(red_fagus_dataset$Avg_meantemp), max(red_fagus_dataset$Avg_meantemp), 2) #allowing temperature to vary for prediction to isolate temperature effect
pred_VPD <- seq(min(red_fagus_dataset$Avg_VPD), max(red_fagus_dataset$Avg_VPD), 0.1) #allowing VPD to vary for prediction to isolate temperature effect
pred_precip <- seq(min(red_fagus_dataset$Avg_precip), max(red_fagus_dataset$Avg_precip), 0.5) #allowing precipitation to vary for prediction to isolate temperature effect
pred_temp_quad <- pred_temp^2
pred_VPD_quad <- pred_VPD^2
pred_precip_quad <- pred_precip^2
#putting above vectors into a dataframe where temperature is the only thing that varies
global_model_dataset_temp_fagus <- data.frame(Year=rep(med_yr, length(pred_temp)),
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
#putting above vectors into a dataframe where VPD is the only thing that varies
global_model_dataset_VPD_fagus <- data.frame(Year=rep(med_yr, length(pred_VPD)),
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
#putting above vectors into a dataframe where precipitation is the only thing that varies
global_model_dataset_precip_fagus <- data.frame(Year=rep(med_yr, length(pred_precip)),
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
#predicting estimates for tree ring width and SE based on only temperature varying to isolate relationship with temperature
global_prediction_temp_fagus <- predict(global_model, newdata = global_model_dataset_temp_fagus, re.form=NULL, se.fit=TRUE)
#predicting estimates for tree ring width and SE based on only precipitation varying to isolate relationship with precipitation
global_prediction_precip_fagus <- predict(global_model, newdata = global_model_dataset_precip_fagus, re.form=NULL, se.fit=TRUE)
#predicting estimates for tree ring width and SE based on only VPD varying to isolate relationship with VPD
global_prediction_VPD_fagus <- predict(global_model, newdata = global_model_dataset_VPD_fagus, re.form=NULL, se.fit=TRUE)
#putting predictions and se into the prediction dataframes created above
global_model_dataset_temp_fagus$pred <- global_prediction_temp_fagus$fit
global_model_dataset_precip_fagus$pred <- global_prediction_precip_fagus$fit
global_model_dataset_VPD_fagus$pred <- global_prediction_VPD_fagus$fit
global_model_dataset_temp_fagus$SE <- global_prediction_temp_fagus$se.fit
global_model_dataset_precip_fagus$SE <- global_prediction_precip_fagus$se.fit
global_model_dataset_VPD_fagus$SE <- global_prediction_VPD_fagus$se.fit
#calculating 95% confidence intervals
global_model_dataset_temp_fagus$LCI <- global_model_dataset_temp_fagus$pred-1.96*global_model_dataset_temp_fagus$SE
global_model_dataset_temp_fagus$HCI <- global_model_dataset_temp_fagus$pred+1.96*global_model_dataset_temp_fagus$SE
global_model_dataset_VPD_fagus$LCI <- global_model_dataset_VPD_fagus$pred-1.96*global_model_dataset_VPD_fagus$SE
global_model_dataset_VPD_fagus$HCI <- global_model_dataset_VPD_fagus$pred+1.96*global_model_dataset_VPD_fagus$SE
global_model_dataset_precip_fagus$LCI <- global_model_dataset_precip_fagus$pred-1.96*global_model_dataset_precip_fagus$SE
global_model_dataset_precip_fagus$HCI <- global_model_dataset_precip_fagus$pred+1.96*global_model_dataset_precip_fagus$SE

#making figures demonstrating the predicted climate relationships with tree ring width 
#for precipitation
fagus_raw_precip <- ggplot(data=global_model_dataset_precip_fagus, aes(x=Avg_precip, y=pred))+
  geom_line(colour="mediumpurple3")+
  geom_line(data=global_model_dataset_precip_fagus, aes(x=Avg_precip, y=HCI), colour = "pink", linetype = "dashed")+
  geom_line(data=global_model_dataset_precip_fagus, aes(x=Avg_precip, y=LCI), colour = "pink", linetype = "dashed")+
  xlab("Average daily precipitation (mm)")+
  ylab("Ring width (log+1)")+
  theme_cowplot()+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))+
  ylim(3, 7)+
  xlim(1, 8)
#for temperature
fagus_raw_temp <- ggplot(data=global_model_dataset_temp_fagus, aes(x=Avg_meantemp, y=pred))+
  geom_line(linetype="dashed", col="mediumpurple3")+
  geom_line(data=global_model_dataset_temp_fagus, aes(x=Avg_meantemp, y=HCI), colour = "pink", linetype = "dashed")+
  geom_line(data=global_model_dataset_temp_fagus, aes(x=Avg_meantemp, y=LCI), colour = "pink", linetype = "dashed")+
  xlab("Average daily mean temperature (°C)")+
  ylab("Ring width (log+1)")+
  theme_cowplot()+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))+
  ylim(3, 7)+
  xlim(9, 21)
#for VPD
fagus_raw_VPD <- ggplot(data=global_model_dataset_VPD_fagus, aes(x=Avg_VPD, y=pred))+
  geom_line(colour="mediumpurple3", linetype="dashed")+
  geom_line(data=global_model_dataset_VPD_fagus, aes(x=Avg_VPD, y=HCI), colour = "pink", linetype = "dashed")+
  geom_line(data=global_model_dataset_VPD_fagus, aes(x=Avg_VPD, y=LCI), colour = "pink", linetype = "dashed")+
  xlab("Average daily VPD (kPa)")+
  ylab("Ring width (log+1)")+
  ggtitle("Fagus sylvatica")+
  theme_cowplot()+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))+
  ylim(3, 7)+
  xlim(0.2, 1)

#creating multipanel plot; columns: FS, QP, BP, rows: VPD, temp, precip
plot_grid(fagus_raw_VPD, querc_raw_VPD, bet_raw_VPD,
          fagus_raw_temp,  querc_raw_temp, bet_raw_temp,
          fagus_raw_precip, querc_raw_precip, bet_raw_precip)

#dropping quadratics from the raw climate model to check that adding them improves fit supportive of quadratic relationship
global_model_no_quads <- glmmTMB(log1p(Ring.width)~Avg_precip+Avg_meantemp+Avg_VPD+
                                   Latitude+
                                   (1|Year)+(1|SiteID)+
                                   ns(Centred_year, df=3)+
                                   (1+ns(Centred_year, df=3)|TreeID)+
                                   (1|Centred_year:SiteID),
                                 data=red_fagus_dataset)

##spatial-temporal model containing all climate predictors
#including fixed effect for latitude, spline terms including random effect for intercept and slope for TreeID
#and random Year and SiteID intercept terms
#including linear terms for spatial variation (Among[]Dev) and temporal variation (Within[])
#including quadratic terms for spatial variation (Among[]DevQuad)
#including interaction between linear temporal and spatial variation
#including quadratic terms for temporal variation (Within[]Quad4) except for temperature
#including quadratic terms for spatial variation (Among[]DevQuad) except for temperature
#####
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

#the above, but including only temperature as a climatic predictor
temperature_quad_model_lat_year <- glmmTMB(log1p(Ring.width)~AmongTempDev+WithinTemp+
                                             AmongTempDevQuad+WithinTempQuad4+
                                             (WithinTemp*AmongTempDev)+
                                             Latitude+(1|Year)+(1|SiteID)+
                                             ns(Centred_year, df=3)+
                                             (1+ns(Centred_year, df=3)|TreeID)+
                                             (1|Centred_year:SiteID),
                                           data=red_fagus_dataset)
#the above, but including only VPD as a climatic predictor
VPD_quad_model_lat_year <- glmmTMB(log1p(Ring.width)~AmongVPDDev+WithinVPD+
                                     AmongVPDDevQuad+WithinVPDQuad4+
                                     (AmongVPDDev*WithinVPD)+
                                     Latitude+(1|Year)+(1|SiteID)+
                                     ns(Centred_year, df=3)+
                                     (1+ns(Centred_year, df=3)|TreeID)+
                                     (1|Centred_year:SiteID),
                                   data=red_fagus_dataset)
#the above, but including only precipitation as a climatic predictor
precip_quad_model_lat_year <- glmmTMB(log1p(Ring.width)~AmongPrecipDev+WithinPrecip+
                                        AmongPrecipDevQuad+WithinPrecipQuad4+
                                        (AmongPrecipDev*WithinPrecip)+
                                        Latitude+(1|Year)+(1|SiteID)+
                                        ns(Centred_year, df=3)+
                                        (1+ns(Centred_year, df=3)|TreeID)+
                                        (1|Centred_year:SiteID),
                                      data=red_fagus_dataset)

#the above but with different combinations of climatic variables
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
AICdf_fagus <- matrix(nrow=18, ncol=6)
colnames(AICdf_fagus) <- c('Model', 'Number of Parameters (Fixed)', 'AIC', 'DeltaAIC', 'Weight', 'Species')

summary(null_model)
library(insight)
get_parameters(null_model)
AICdf_fagus[1,1] <- c('Null model')#model name
AICdf_fagus[1,2] <- c('6')#number of fixed parameters
AICdf_fagus[1,3] <- 26134.6 #taken from that calculated by glmmTMB in summary
#same structure used below

summary(no_quads_temp)
get_parameters(no_quads_temp)
AICdf_fagus[2,1] <- c('All climate variables, excluding temporal quadratics')
AICdf_fagus[2,2] <- c('18')
AICdf_fagus[2,3] <- 26110.7

summary(no_quads_spat)
get_parameters(no_quads_spat)
AICdf_fagus[3,1] <- c('All climate variables, excluding spatial quadratics')
AICdf_fagus[3,2] <- c('18')
AICdf_fagus[3,3] <-  26107.0

summary(precip_quad_model_lat_year)
get_parameters(precip_quad_model_lat_year)
AICdf_fagus[4,1] <- c('Precipitation only')
AICdf_fagus[4,2] <- c('11')
AICdf_fagus[4,3] <-  26121.2 

summary(temperature_quad_model_lat_year)
AICdf_fagus[5,1] <- c('Mean temperature only')
AICdf_fagus[5,2] <- c('11')
AICdf_fagus[5,3] <- 26130.3

summary(VPD_quad_model_lat_year)
AICdf_fagus[6,1] <- c('VPD only')
AICdf_fagus[6,2] <- c('11')
AICdf_fagus[6,3] <- 26115.8

summary(all_climate_model)
get_parameters(all_climate_model)
AICdf_fagus[7,1] <- c('All climate variables')
AICdf_fagus[7,2] <- c('21')
AICdf_fagus[7,3] <- 26111.9

summary(VPD_temp_climate_model)
get_parameters(VPD_temp_climate_model)
AICdf_fagus[8,1] <- c('VPD and mean temperature')
AICdf_fagus[8,2] <- c('16')
AICdf_fagus[8,3] <- 26114.3

summary(VPD_precip_climate_model)
AICdf_fagus[9,1] <- c('VPD and precipitation')
AICdf_fagus[9,2] <- c('16')
AICdf_fagus[9,3] <- 26115.4

summary(precip_temp_climate_model)
AICdf_fagus[10,1] <- c('Precipitation and mean temperature')
AICdf_fagus[10,2] <- c('16')
AICdf_fagus[10,3] <-  26121.9

summary(no_quadsTemp_temp)
get_parameters(no_quadsTemp_temp)
AICdf_fagus[11,1] <- c('All climate variables, excluding temporal temperature quadratic')
AICdf_fagus[11,2] <- c('20')
AICdf_fagus[11,3] <-26110.1

summary(no_quadsTemp_spat)
AICdf_fagus[12,1] <- c('All climate variables, excluding spatial temperature quadratic')
AICdf_fagus[12,2] <- c('20')
AICdf_fagus[12,3] <-26109.9

summary(no_quadsVPD_temp)
AICdf_fagus[13,1] <- c('All climate variables, excluding temporal VPD quadratic')
AICdf_fagus[13,2] <- c('20')
AICdf_fagus[13,3] <-26111.8 

summary(no_quadsVPD_spat)
AICdf_fagus[14,1] <- c('All climate variables, excluding spatial VPD quadratic')
AICdf_fagus[14,2] <- c('20')
AICdf_fagus[14,3] <-26110.2 

summary(no_quadsPrecip_temp)
AICdf_fagus[15,1] <- c('All climate variables, excluding temporal precipitation quadratic')
AICdf_fagus[15,2] <- c('20')
AICdf_fagus[15,3] <- 26112.6

summary(no_quadsPrecip_spat)
AICdf_fagus[16,1] <- c('All climate variables, excluding spatial precipitation quadratic')
AICdf_fagus[16,2] <- c('20')
AICdf_fagus[16,3] <-26110.5

summary(global_model)
get_parameters(global_model)
AICdf_fagus[17,1] <- c('Raw climate model')
AICdf_fagus[17,2] <- c('12')
AICdf_fagus[17,3] <-26105.2

summary(global_model_no_quads)
get_parameters(global_model_no_quads)
AICdf_fagus[18,1] <- c('Raw climate model, excluding quadratics')
AICdf_fagus[18,2] <- c('9')
AICdf_fagus[18,3] <- 26108.8 

#calculating delta AIC and weights
library(qpcR)
weights_fagus <- akaike.weights(as.numeric(AICdf_fagus[,3]))
AICdf_fagus[,4] <- weights_fagus$deltaAIC
AICdf_fagus[,5] <- weights_fagus$weights

AICdf_fagus[,6] <- "Fagus sylvatica"#adding species name for dataframe to create figure
write.csv(AICdf_fagus, "Figures/AIC_scores.csv")

#creating figure
library(ggplot2)
library(cowplot)
#nicer colour scheme
library(viridis)

#going from matrix to df
AICdf_fagus <- as.data.frame(AICdf_fagus)
#making delta AIC numeric
AICdf_fagus$DeltaAIC <- as.numeric(AICdf_fagus$DeltaAIC)
#rounding delta AIC to reasonable dp
AICdf_fagus$DeltaAIC_lab <- format(round(AICdf_fagus$DeltaAIC, 2), nsmall = 1)
#rearranging order of model
AICdf_fagus$Model <- factor(AICdf_fagus$Model, levels=c("Null model", "Mean temperature only",
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
#creating figure showing the delta AIC scores of different models
AIC_fagus_fig <- ggplot(data=AICdf_fagus, aes(x=Model, y=DeltaAIC, fill=Model))+
  geom_bar(position="dodge", stat="identity")+
  theme_cowplot()+
  theme(axis.text=element_text(size=10), legend.text = element_text(size=10))+
  geom_text(aes(label=DeltaAIC_lab), vjust=-0.5, size = 2.5, position = position_dodge(0.9))+
  ylab("Delta AIC")+
  ggtitle("Fagus sylvatica")+
  scale_fill_viridis(discrete=TRUE)+
  theme(legend.position="none")+
  scale_x_discrete(labels= c(1:18))+#adding numbers to x axis to make clearer which model is which
  theme(title=element_text(size=12),
        axis.title.x=element_blank())+
  theme(plot.title = element_text(face = "italic"))+
  ylim(0,130)

##plotting 
library(dplyr)

###among quad fagus
#####
#skeleton dataset for prediction of VPD
#fixed continuous variables at median, year at 2000, tree ID at first tree ID in dataset, site ID at first site ID in dataset
#apart from the among deviation and among quad values so the only thing that the only thing that varies 
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
#using the all_climate_model 
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

#calculating 95% confidence intervals of among slope
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
  
  MinWithinTemp <- min(red_fagus_dataset$WithinTemp)
  MaxWithinTemp <- max(red_fagus_dataset$WithinTemp)
  pred_withinTemp <- seq(MinWithinTemp, MaxWithinTemp, 0.5)#sequence of values for temporal effect
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
  #calculating deviations away from  global average for each within deviation
  newdatasubset$ActualAmongTemp <- newdatasubset$AvgAmongTemp+newdatasubset$AmongTempDev
  newdatasubset$ActualTemp <- newdatasubset$ActualAmongTemp+newdatasubset$WithinTemp
  newdatasubset$WithinGlobDev <- newdatasubset$ActualTemp-newdatasubset$AvgAmongTemp
  
  temp_pred_subset <-predict(all_climate_model_fagus, newdata = newdatasubset, re.form=NULL, se.fit=TRUE)
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
  theme(plot.title = element_text(face = "italic"))+
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

#creating figure with a curve for among/spatial, and four curves for within/temporal
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
  ggtitle("Fagus sylvatica")+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12), 
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))+
  ylim(2,6)+
  xlim(-0.5, 1)#putting on the same axis

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
  theme(axis.title.y=element_text(size=12))+
  ylim(4,6)+
  xlim(-3, 6)

fagus_Precip

#grids
#multipanel plot for spatial temporal comparison graphs 
library(cowplot)

plot_grid(fagus_VPD, quercus_VPD, betula_VPD, 
          fagus_temp, quercus_temp, betula_temp, 
          fagus_Precip, quercus_Precip, betula_Precip)

#model outputs
summary(all_climate_model)
summary(all_climate_model_quercus)
summary(all_climate_model_fagus)

