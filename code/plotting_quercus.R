#climate plots
mean_centred_quercus_dataset <- read.csv("Data/e_obs/mean_centred_data_quercus.csv")
library(dplyr)
red_quercus_dataset <- mean_centred_quercus_dataset %>%
  filter(SiteID != "ITQP09") %>%
  filter(SiteID != "ITQP10")

###among quad quercus
#####
#skeleton dataset for prediction of VPD
#keeping everything fixed apart from the among and among quad values so only thing that 
#varies is the average temperature, to show the global/spatial relationship
med_year <- median(red_quercus_dataset$Centred_year)
med_yr <- 2000
med_withinVPD <- median(red_quercus_dataset$WithinVPD)
med_withinVPDQuad <- med_withinVPD^2
MinAmongVPD <- min(red_quercus_dataset$AmongVPD)
MaxAmongVPD <- max(red_quercus_dataset$AmongVPD)
pred_amongVPD <- seq(MinAmongVPD, MaxAmongVPD, 0.025)
pred_amongVPDQuad <- pred_amongVPD^2
tree_id <- as.character((red_quercus_dataset$TreeID[1])) 
site_id <- as.character((red_quercus_dataset$SiteID[1]))
med_lat <- red_quercus_dataset$Latitude[1]
med_amongTemp <- median(red_quercus_dataset$AmongTemp)
med_amongTempQuad <- med_amongTemp^2
med_amongPrecip <- median(red_quercus_dataset$AmongPrecip)
med_amongPrecipQuad <- med_amongPrecip^2
med_withinTemp <- median(red_quercus_dataset$WithinTemp)
med_withinTempQuad <- med_withinTemp^2
med_withinPrecip <- median(red_quercus_dataset$WithinPrecip)
med_withinPrecipQuad <- med_withinPrecip^2

#creating the dataframes for the prediction
newdataAmongVPD <- data.frame(Year=rep(med_yr, length(pred_amongVPD)),
                              Centred_year=rep(med_year, length(pred_amongVPD)),
                              Latitude=rep(med_lat, length(pred_amongVPD)),
                              WithinVPD=rep(med_withinVPD, length(pred_amongVPD)),
                              WithinVPDQuad4=rep(med_withinVPDQuad, length(pred_amongVPD)),
                              TreeID=tree_id,
                              Year=rep(med_year, length(pred_amongVPD)),
                              SiteID=site_id,
                              AmongVPD=pred_amongVPD,
                              AmongVPDQuad = pred_amongVPDQuad,
                              AmongPrecip=rep(med_amongPrecip, length(pred_amongVPD)),
                              AmongPrecipQuad=rep(med_amongPrecipQuad, length(pred_amongVPD)),
                              WithinPrecip=rep(med_withinPrecip, length(pred_amongVPD)),
                              WithinPrecipQuad4=rep(med_withinPrecipQuad, length(pred_amongVPD)),
                              AmongTemp=rep(med_amongTemp, length(pred_amongVPD)),
                              AmongTempQuad=rep(med_amongTempQuad, length(pred_amongVPD)),
                              WithinTemp=rep(med_withinTemp, length(pred_amongVPD)),
                              WithinTempQuad4=rep(med_withinTempQuad, length(pred_amongVPD)))

#for prediction of temperature
med_year <- median(red_quercus_dataset$Centred_year)
med_yr <- 2000
med_withinTemp <- median(red_quercus_dataset$WithinTemp)
med_withinTempQuad <- med_withinTemp^2
MinAmongTemp <- min(red_quercus_dataset$AmongTemp)
MaxAmongTemp <- max(red_quercus_dataset$AmongTemp)
pred_amongTemp <- seq(MinAmongTemp, MaxAmongTemp, 1)
pred_amongTempQuad <- pred_amongTemp^2
tree_id <- as.character((red_quercus_dataset$TreeID[1])) 
site_id <- as.character((red_quercus_dataset$SiteID[1]))
med_lat <- red_quercus_dataset$Latitude[1]
med_amongVPD <- median(red_quercus_dataset$AmongVPD)
med_amongVPDQuad <- med_amongVPD^2
med_amongPrecip <- median(red_quercus_dataset$AmongPrecip)
med_amongPrecipQuad <- med_amongPrecip^2
med_withinVPD <- median(red_quercus_dataset$WithinVPD)
med_withinVPDQuad <- med_withinVPD^2
med_withinPrecip <- median(red_quercus_dataset$WithinPrecip)
med_withinPrecipQuad <- med_withinPrecip^2

newdataAmongTemp <- data.frame(Year=rep(med_yr, length(pred_amongTemp)),
                               Centred_year=rep(med_year, length(pred_amongTemp)),
                               Latitude=rep(med_lat, length(pred_amongTemp)),
                               WithinTemp=rep(med_withinTemp, length(pred_amongTemp)),
                               WithinTempQuad4=rep(med_withinTempQuad, length(pred_amongTemp)),
                               TreeID=rep(tree_id, length(pred_amongTemp)),
                               SiteID=rep(site_id, length(pred_amongTemp)),
                               AmongTemp=pred_amongTemp,
                               AmongTempQuad = pred_amongTempQuad,
                               AmongPrecip=rep(med_amongPrecip, length(pred_amongTemp)),
                               AmongPrecipQuad=rep(med_amongPrecipQuad, length(pred_amongTemp)),
                               WithinPrecip=rep(med_withinPrecip, length(pred_amongTemp)),
                               WithinPrecipQuad4=rep(med_withinPrecipQuad, length(pred_amongTemp)),
                               AmongVPD=rep(med_amongVPD, length(pred_amongTemp)),
                               AmongVPDQuad=rep(med_amongVPDQuad, length(pred_amongTemp)),
                               WithinVPD=rep(med_withinVPD, length(pred_amongTemp)),
                               WithinVPDQuad4=rep(med_withinVPDQuad, length(pred_amongTemp)))

#for prediction for precipitation
med_year <- median(red_quercus_dataset$Centred_year)
med_yr <- 2000
med_withinPrecip <- median(red_quercus_dataset$WithinPrecip)
med_withinPrecipQuad <- med_withinPrecip^2
MinAmongPrecip <- min(red_quercus_dataset$AmongPrecip)
MaxAmongPrecip <- max(red_quercus_dataset$AmongPrecip)
pred_amongPrecip <- seq(MinAmongPrecip, MaxAmongPrecip, 0.25)
pred_amongPrecipQuad <- pred_amongPrecip^2
tree_id <- as.character((red_quercus_dataset$TreeID[1])) 
site_id <- as.character((red_quercus_dataset$SiteID[1]))
med_lat <- red_quercus_dataset$Latitude[1]
med_amongVPD <- median(red_quercus_dataset$AmongVPD)
med_amongVPDQuad <- med_amongVPD^2
med_amongTemp <- median(red_quercus_dataset$AmongTemp)
med_amongTempQuad <- med_amongTemp^2
med_withinVPD <- median(red_quercus_dataset$WithinVPD)
med_withinVPDQuad <- med_withinVPD^2
med_withinTemp <- median(red_quercus_dataset$WithinTemp)
med_withinTempQuad <- med_withinTemp^2


newdataAmongPrecip <- data.frame(Year=rep(med_yr, length(pred_amongPrecip)),
                                 Centred_year=rep(med_year, length(pred_amongPrecip)),
                                 Latitude=rep(med_lat, length(pred_amongPrecip)),
                                 WithinPrecip=rep(med_withinPrecip, length(pred_amongPrecip)),
                                 WithinPrecipQuad4=rep(med_withinPrecipQuad, length(pred_amongPrecip)),
                                 TreeID=tree_id,
                                 SiteID=site_id,
                                 AmongPrecip=pred_amongPrecip,
                                 AmongPrecipQuad = pred_amongPrecipQuad,
                                 AmongVPD=rep(med_amongVPD, length(pred_amongPrecip)),
                                 AmongVPDQuad=rep(med_amongVPDQuad, length(pred_amongPrecip)),
                                 WithinVPD=rep(med_withinVPD, length(pred_amongPrecip)),
                                 WithinVPDQuad4=rep(med_withinVPDQuad, length(pred_amongPrecip)),
                                 AmongTemp=rep(med_amongTemp, length(pred_amongPrecip)),
                                 AmongTempQuad=rep(med_amongTempQuad, length(pred_amongPrecip)),
                                 WithinTemp=rep(med_withinTemp, length(pred_amongPrecip)),
                                 WithinTempQuad4=rep(med_withinTempQuad, length(pred_amongPrecip)))

#predicting expected tree ring widths in the skeleton database 
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
amongmeantemps <- c(12.5, 14, 15.5, 17) 
within_points_df <- list()

for(i in 1:length(amongmeantemps)){
  med_amongTemp <- amongmeantemps[i]
  med_amongTempQuad <- med_amongTemp^2
  
  MinWithinTemp <- min(red_quercus_dataset$WithinTemp)
  MaxWithinTemp <- max(red_quercus_dataset$WithinTemp)
  pred_withinTemp <- seq(MinWithinTemp, MaxWithinTemp, 0.5)
  pred_withinTempquad <- pred_withinTemp^2
  
  med_withinVPD <- median(red_quercus_dataset$WithinVPD)
  med_withinVPDQuad <- med_withinTemp^2
  med_amongVPD <- median(red_quercus_dataset$AmongVPD)
  med_amongVPDQuad <- med_amongVPD^2
  
  med_withinPrecip <- median(red_quercus_dataset$WithinPrecip)
  med_withinPrecipQuad <- med_withinPrecip^2
  med_amongPrecip <- median(red_quercus_dataset$AmongPrecip)
  med_amongPrecipQuad <- med_amongPrecip^2
  
  newdatasubset <- data.frame(Year=rep(med_yr, length(pred_withinTemp)),
                              Centred_year=rep(med_year, length(pred_withinTemp)),
                              Latitude=rep(med_lat, length(pred_withinTemp)),
                              WithinTemp=pred_withinTemp,
                              WithinTempQuad4=pred_withinTempquad,
                              TreeID=tree_id,
                              SiteID=site_id,
                              AmongTemp=rep(med_amongTemp, length(pred_withinTemp)),
                              AmongTempQuad=rep(med_amongTempQuad, length(pred_withinTemp)),
                              AmongPrecip=rep(med_amongPrecip, length(pred_withinTemp)),
                              AmongPrecipQuad=rep(med_amongPrecipQuad, length(pred_withinTemp)),
                              WithinPrecip=rep(med_withinPrecip, length(pred_withinTemp)),
                              WithinPrecipQuad4=rep(med_withinPrecipQuad, length(pred_withinTemp)),
                              AmongVPD=rep(med_amongVPD, length(pred_withinTemp)),
                              AmongVPDQuad=rep(med_amongVPDQuad, length(pred_withinTemp)),
                              WithinVPD=rep(med_withinVPD, length(pred_withinTemp)),
                              WithinVPDQuad4=rep(med_withinVPDQuad, length(pred_withinTemp)))
  
  temp_pred_subset <-predict(all_climate_model_quercus, newdata = newdatasubset, re.form=NULL, se.fit=TRUE)
  temp_pred_subset_vals <- temp_pred_subset$fit
  temp_pred_subset_HCI <- temp_pred_subset_vals+1.96*temp_pred_subset$se.fit
  temp_pred_subset_LCI <- temp_pred_subset_vals-1.96*temp_pred_subset$se.fit
  within_points_df[[i]] <- data.frame(Prediction=temp_pred_subset_vals, 
                                      AmongTemp=newdatasubset$AmongTemp,
                                      ActualTemp=(newdatasubset$AmongTemp+newdatasubset$WithinTemp),
                                      WithinTemp=newdatasubset$WithinTemp,
                                      HCI=temp_pred_subset_HCI,
                                      LCI=temp_pred_subset_LCI)
}  

within_curves1 <- within_points_df[[1]]
within_curves2 <- within_points_df[[2]]
within_curves3 <- within_points_df[[3]]
within_curves4 <- within_points_df[[4]]

library(ggplot2)
library(cowplot)
quercus_temp <- ggplot(data=red_quercus_dataset, aes(x=AmongTemp, y=log1p(Ring.width)))+
  geom_line(data = data.frame(AmongTemp = newdataAmongTemp$AmongTemp, Ring.width = newdataAmongTemp$Prediction), aes(x = AmongTemp, y = Ring.width), colour = "blue")+
  geom_line(data = data.frame(AmongTemp = within_curves1$ActualTemp, Ring.width = within_curves1$Prediction), aes(x = AmongTemp, y = Ring.width), colour = "red")+
  geom_line(data = data.frame(AmongTemp = within_curves2$ActualTemp, Ring.width = within_curves2$Prediction), aes(x = AmongTemp, y = Ring.width), colour = "red")+
  geom_line(data = data.frame(AmongTemp = within_curves3$ActualTemp, Ring.width = within_curves3$Prediction), aes(x = AmongTemp, y = Ring.width), colour = "red")+
  geom_line(data = data.frame(AmongTemp = within_curves4$ActualTemp, Ring.width = within_curves4$Prediction), aes(x = AmongTemp, y = Ring.width), colour = "red")+
  theme_cowplot()+
  xlab("Average daily mean temperature (°C)")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))

#for VPD
#need to change the deviations because can't have above 1 VPD 
amongVPDs <- c(0.3, 0.4, 0.5, 0.6) 
within_points_df_VPD <- list()

for(i in 1:length(amongVPDs)){
  med_year <- median(red_quercus_dataset$Centred_year)
  med_yr <- 2000
  med_amongVPD <- amongVPDs[i]
  med_amongVPDQuad <- med_amongVPD^2
  tree_id <- as.character((red_quercus_dataset$TreeID[1]))
  site_id <- as.character((red_quercus_dataset$SiteID[1]))
  med_lat <- red_quercus_dataset$Latitude[1]
  
  MinWithinVPD <- min(red_quercus_dataset$WithinVPD)
  MaxWithinVPD <- max(red_quercus_dataset$WithinVPD)
  pred_withinVPD <- seq(MinWithinVPD, MaxWithinVPD, 0.125)
  pred_withinVPDQuad <- pred_withinVPD^2
  
  med_withinTemp <- median(red_quercus_dataset$WithinTemp)
  med_withinTempQuad <- med_withinTemp^2
  med_amongTemp <- median(red_quercus_dataset$AmongTemp)
  med_amongTempQuad <- med_amongTemp^2
  
  med_withinPrecip <- median(red_quercus_dataset$WithinPrecip)
  med_withinPrecipQuad <- med_withinPrecip^2
  med_amongPrecip <- median(red_quercus_dataset$AmongPrecip)
  med_amongPrecipQuad <- med_amongPrecip^2
  
  newdatasubset <- data.frame(Year=rep(med_yr, length(pred_withinVPD)),
                              Centred_year=rep(med_year, length(pred_withinVPD)),
                              Latitude=rep(med_lat, length(pred_withinVPD)),
                              WithinVPD=pred_withinVPD,
                              WithinVPDQuad4=pred_withinVPDQuad,
                              AmongVPD=rep(med_amongVPD, length(pred_withinVPD)),
                              AmongVPDQuad=rep(med_amongVPDQuad, length(pred_withinVPD)),
                              TreeID=tree_id,
                              SiteID=site_id,
                              AmongTemp=rep(med_amongTemp, length(pred_withinVPD)),
                              AmongTempQuad=rep(med_amongTempQuad, length(pred_withinVPD)),
                              AmongPrecip=rep(med_amongPrecip, length(pred_withinVPD)),
                              AmongPrecipQuad=rep(med_amongPrecipQuad, length(pred_withinVPD)),
                              WithinPrecip=rep(med_withinPrecip, length(pred_withinVPD)),
                              WithinPrecipQuad4=rep(med_withinPrecipQuad, length(pred_withinVPD)),
                              WithinTemp=rep(med_withinTemp, length(pred_withinVPD)),
                              WithinTempQuad4=rep(med_withinTempQuad, length(pred_withinVPD)))
  
  VPD_pred_subset <-predict(all_climate_model_quercus, newdata = newdatasubset, re.form=NULL, se.fit=TRUE)
  VPD_pred_subset_vals <- VPD_pred_subset$fit
  VPD_pred_subset_HCI <- VPD_pred_subset_vals+1.96*VPD_pred_subset$se.fit
  VPD_pred_subset_LCI <- VPD_pred_subset_vals-1.96*VPD_pred_subset$se.fit
  within_points_df_VPD[[i]] <- data.frame(Prediction=VPD_pred_subset_vals, 
                                          AmongVPD=newdatasubset$AmongVPD,
                                          ActualVPD=(newdatasubset$AmongVPD+newdatasubset$WithinVPD),
                                          WithinVPD=newdatasubset$WithinVPD,
                                          HCI=VPD_pred_subset_HCI,
                                          LCI=VPD_pred_subset_LCI)
}  

within_curves1_VPD <- within_points_df_VPD[[1]]
within_curves2_VPD <- within_points_df_VPD[[2]]
within_curves3_VPD <- within_points_df_VPD[[3]]
within_curves4_VPD <- within_points_df_VPD[[4]]

library(ggplot2)
quercus_VPD <- ggplot(data=red_quercus_dataset, aes(x=AmongVPD, y=log1p(Ring.width)))+
  geom_line(data = data.frame(AmongVPD = newdataAmongVPD$AmongVPD, Ring.width = newdataAmongVPD$Prediction), aes(x = AmongVPD, y = Ring.width), colour = "blue")+
  geom_line(data = data.frame(AmongVPD = within_curves1_VPD$ActualVPD, Ring.width = within_curves1_VPD$Prediction), aes(x = AmongVPD, y = Ring.width), colour = "red")+
  geom_line(data = data.frame(AmongVPD = within_curves2_VPD$ActualVPD, Ring.width = within_curves2_VPD$Prediction), aes(x = AmongVPD, y = Ring.width), colour = "red")+
  geom_line(data = data.frame(AmongVPD = within_curves3_VPD$ActualVPD, Ring.width = within_curves3_VPD$Prediction), aes(x = AmongVPD, y = Ring.width), colour = "red")+
  geom_line(data = data.frame(AmongVPD = within_curves4_VPD$ActualVPD, Ring.width = within_curves4_VPD$Prediction), aes(x = AmongVPD, y = Ring.width), colour = "red")+
  theme_cowplot()+
  xlab("Average daily VPD")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))

#for precip
amongPrecips <- c(1.5, 2, 2.5, 3) 
within_points_df_precip <- list()

for(i in 1:length(amongPrecips)){
  med_year <- median(red_quercus_dataset$Centred_year)
  med_yr <- 2000
  med_amongPrecip <- amongPrecips[i]
  med_amongPrecipQuad <- med_amongPrecip^2
  tree_id <- as.character((red_quercus_dataset$TreeID[1]))
  site_id <- as.character((red_quercus_dataset$SiteID[1]))
  med_lat <- red_quercus_dataset$Latitude[1]
  
  MinWithinPrecip <- min(red_quercus_dataset$WithinPrecip)
  MaxWithinPrecip <- max(red_quercus_dataset$WithinPrecip)
  pred_withinPrecip <- seq(MinWithinPrecip, MaxWithinPrecip, 0.25)
  pred_withinPrecipQuad <- pred_withinPrecip^2
  
  med_withinVPD <- median(red_quercus_dataset$WithinVPD)
  med_withinVPDquad <- med_withinVPD^2
  med_amongVPD <- median(red_quercus_dataset$AmongVPD)
  med_amongVPDQuad <- med_amongVPD^2
  
  med_withinTemp <- median(red_quercus_dataset$WithinTemp)
  med_withinTempQuad <- med_withinTemp^2
  med_amongTemp <- median(red_quercus_dataset$AmongTemp)
  med_amongTempQuad <- med_amongTemp^2
  
  newdatasubset <- data.frame(Year=rep(med_yr, length(pred_withinPrecip)),
                              Centred_year=rep(med_year, length(pred_withinPrecip)),
                              Latitude=rep(med_lat, length(pred_withinPrecip)),
                              WithinPrecip=pred_withinPrecip,
                              WithinPrecipQuad4=pred_withinPrecipQuad,
                              AmongPrecip=rep(med_amongPrecip, length(pred_withinPrecip)),
                              AmongPrecipQuad=rep(med_amongPrecipQuad, length(pred_withinPrecip)),
                              TreeID=tree_id,
                              SiteID=site_id,
                              AmongTemp=rep(med_amongTemp, length(pred_withinPrecip)),
                              AmongTempQuad=rep(med_amongTempQuad, length(pred_withinPrecip)),
                              AmongVPD=rep(med_amongVPD, length(pred_withinPrecip)),
                              AmongVPDQuad=rep(med_amongVPDQuad, length(pred_withinPrecip)),
                              WithinVPD=rep(med_withinVPD, length(pred_withinPrecip)),
                              WithinVPDQuad4=rep(med_withinVPDQuad, length(pred_withinPrecip)),
                              WithinTemp=rep(med_withinTemp, length(pred_withinPrecip)),
                              WithinTempQuad4=rep(med_withinTempQuad, length(pred_withinPrecip)))
  
  Precip_pred_subset <-predict(all_climate_model_quercus, newdata = newdatasubset, re.form=NULL, se.fit=TRUE)
  Precip_pred_subset_vals <- Precip_pred_subset$fit
  Precip_pred_subset_HCI <- Precip_pred_subset_vals+1.96*Precip_pred_subset$se.fit
  Precip_pred_subset_LCI <- Precip_pred_subset_vals-1.96*Precip_pred_subset$se.fit
  within_points_df_precip[[i]] <- data.frame(Prediction=Precip_pred_subset_vals, 
                                             AmongPrecip=newdatasubset$AmongPrecip,
                                             ActualPrecip=(newdatasubset$AmongPrecip+newdatasubset$WithinPrecip),
                                             WithinPrecip=newdatasubset$WithinPrecip,
                                             HCI=Precip_pred_subset_HCI,
                                             LCI=Precip_pred_subset_LCI)
}  

within_curves1_precip <- within_points_df_precip[[1]]
within_curves1_precip <- within_curves1_precip %>% 
  filter(ActualPrecip > 0.01)
within_curves2_precip <- within_points_df_precip[[2]]
within_curves2_precip <- within_curves2_precip %>% 
  filter(ActualPrecip > 0.01)
within_curves3_precip <- within_points_df_precip[[3]]
within_curves4_precip <- within_points_df_precip[[4]]

library(ggplot2)
quercus_precip <- ggplot(data=red_quercus_dataset, aes(x=AmongPrecip, y=log1p(Ring.width)))+
  geom_line(data = data.frame(AmongPrecip = newdataAmongPrecip$AmongPrecip, Ring.width = newdataAmongPrecip$Prediction), aes(x = AmongPrecip, y = Ring.width), colour = "blue")+
  geom_line(data = data.frame(AmongPrecip = within_curves1_precip$ActualPrecip, Ring.width = within_curves1_precip$Prediction), aes(x = AmongPrecip, y = Ring.width), colour = "red")+
  geom_line(data = data.frame(AmongPrecip = within_curves2_precip$ActualPrecip, Ring.width = within_curves2_precip$Prediction), aes(x = AmongPrecip, y = Ring.width), colour = "red")+
  geom_line(data = data.frame(AmongPrecip = within_curves3_precip$ActualPrecip, Ring.width = within_curves3_precip$Prediction), aes(x = AmongPrecip, y = Ring.width), colour = "red")+
  geom_line(data = data.frame(AmongPrecip = within_curves4_precip$ActualPrecip, Ring.width = within_curves4_precip$Prediction), aes(x = AmongPrecip, y = Ring.width), colour = "red")+
  theme_cowplot()+
  xlab("Average daily precipitation (mm)")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))

