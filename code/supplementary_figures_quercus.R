#plotting for supplementary 
library(cowplot)
library(tidyverse)

#range of climate across countries
#plotting span of climate across the different countries sampled for oak
quercus_temp_data <- ggplot(red_quercus_dataset, aes(x=Country, y=Avg_meantemp, fill=Country))+
  geom_boxplot()+
  theme_cowplot()+
  ylab('Average daily mean temperature (°C)')+
  xlab('Country')+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  theme(axis.text=element_text(size=8))+
  theme(legend.position="none")

quercus_VPD_data <- ggplot(red_quercus_dataset, aes(x=Country, y=Avg_VPD, fill=Country))+
  geom_boxplot()+
  theme_cowplot()+
  ylab('Average daily VPD')+
  xlab('Country')+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  theme(axis.text=element_text(size=8))+
  theme(legend.position="none")

quercus_precip_data <- ggplot(red_quercus_dataset, aes(x=Country, y=Avg_precip, fill=Country))+
  geom_boxplot()+
  theme_cowplot()+
  ylab('Average daily precipitation (mm)')+
  xlab('Country')+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  theme(axis.text=element_text(size=8))+
  theme(legend.position="none")


#repeating for beech
fagus_temp_data <- ggplot(red_fagus_dataset, aes(x=Country, y=Avg_meantemp, fill=Country))+
  geom_boxplot()+
  theme_cowplot()+
  ylab('Average daily mean temperature (°C)')+
  xlab('Country')+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  theme(axis.text=element_text(size=8))+
  theme(legend.position="none")

fagus_VPD_data <- ggplot(red_fagus_dataset, aes(x=Country, y=Avg_VPD, fill=Country))+
  geom_boxplot()+
  theme_cowplot()+
  ylab('Average daily VPD')+
  xlab('Country')+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  theme(axis.text=element_text(size=8))+
  theme(legend.position="none")

fagus_precip_data <- ggplot(red_fagus_dataset, aes(x=Country, y=Avg_precip, fill=Country))+
  geom_boxplot()+
  theme_cowplot()+
  ylab('Average daily precipitation (mm)')+
  xlab('Country')+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  theme(axis.text=element_text(size=8))+
  theme(legend.position="none")

#creating multipanel plots for each species showing the span of the climate data 
plot_grid(quercus_temp_data, quercus_VPD_data, quercus_precip_data)
plot_grid(fagus_temp_data, fagus_VPD_data, fagus_precip_data)

#error around among curve for quercus
quercus_VPD_error <- ggplot(red_quercus_dataset, aes(x=AmongVPD, y=log1p(Ring.width)))+
  geom_point()+
  geom_line(data = data.frame(AmongVPD = newdataAmongVPD$AmongVPD, Ring.width = newdataAmongVPD$Prediction), aes(x = AmongVPD, y = Ring.width), colour = "blue")+
  geom_line(data = data.frame(AmongVPD = newdataAmongVPD$AmongVPD, Ring.width = newdataAmongVPD$HCI), aes(x = AmongVPD, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  geom_line(data = data.frame(AmongVPD = newdataAmongVPD$AmongVPD, Ring.width = newdataAmongVPD$LCI), aes(x = AmongVPD, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  theme_cowplot()+
  xlab("Average daily VPD")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))

quercus_temp_error <- ggplot(red_quercus_dataset, aes(x=AmongTemp, y=log1p(Ring.width)))+
  geom_point()+
  geom_line(data = data.frame(AmongTemp = newdataAmongTemp$AmongTemp, Ring.width = newdataAmongTemp$Prediction), aes(x = AmongTemp, y = Ring.width), colour = "blue")+
  geom_line(data = data.frame(AmongTemp = newdataAmongTemp$AmongTemp, Ring.width = newdataAmongTemp$HCI), aes(x = AmongTemp, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  geom_line(data = data.frame(AmongTemp = newdataAmongTemp$AmongTemp, Ring.width = newdataAmongTemp$LCI), aes(x = AmongTemp, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  theme_cowplot()+
  xlab("Average daily mean temperature (°C)")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))

quercus_precip_error <- ggplot(red_quercus_dataset, aes(x=AmongPrecip, y=log1p(Ring.width)))+
  geom_point()+
  geom_line(data = data.frame(AmongPrecip = newdataAmongPrecip$AmongPrecip, Ring.width = newdataAmongPrecip$Prediction), aes(x = AmongPrecip, y = Ring.width), colour = "blue")+
  geom_line(data = data.frame(AmongPrecip = newdataAmongPrecip$AmongPrecip, Ring.width = newdataAmongPrecip$HCI), aes(x = AmongPrecip, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  geom_line(data = data.frame(AmongPrecip = newdataAmongPrecip$AmongPrecip, Ring.width = newdataAmongPrecip$LCI), aes(x = AmongPrecip, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  theme_cowplot()+
  xlab("Average daily precipitation (mm)")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))
         
#creating multipanel plot for error around curve 
plot_grid(quercus_precip_error, quercus_temp_error, quercus_VPD_error)

#error around among curve for fagus
fagus_VPD_error <- ggplot(red_fagus_dataset, aes(x=AmongVPD, y=log1p(Ring.width)))+
  geom_point()+
  geom_line(data = data.frame(AmongVPD = newdataAmongVPD$AmongVPD, Ring.width = newdataAmongVPD$Prediction), aes(x = AmongVPD, y = Ring.width), colour = "blue")+
  geom_line(data = data.frame(AmongVPD = newdataAmongVPD$AmongVPD, Ring.width = newdataAmongVPD$HCI), aes(x = AmongVPD, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  geom_line(data = data.frame(AmongVPD = newdataAmongVPD$AmongVPD, Ring.width = newdataAmongVPD$LCI), aes(x = AmongVPD, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  theme_cowplot()+
  xlab("Average daily VPD")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))

fagus_temp_error <- ggplot(red_fagus_dataset, aes(x=AmongTemp, y=log1p(Ring.width)))+
  geom_point()+
  geom_line(data = data.frame(AmongTemp = newdataAmongTemp$AmongTemp, Ring.width = newdataAmongTemp$Prediction), aes(x = AmongTemp, y = Ring.width), colour = "blue")+
  geom_line(data = data.frame(AmongTemp = newdataAmongTemp$AmongTemp, Ring.width = newdataAmongTemp$HCI), aes(x = AmongTemp, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  geom_line(data = data.frame(AmongTemp = newdataAmongTemp$AmongTemp, Ring.width = newdataAmongTemp$LCI), aes(x = AmongTemp, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  theme_cowplot()+
  xlab("Average daily mean temperature (°C)")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))

fagus_precip_error <- ggplot(red_fagus_dataset, aes(x=AmongPrecip, y=log1p(Ring.width)))+
  geom_point()+
  geom_line(data = data.frame(AmongPrecip = newdataAmongPrecip$AmongPrecip, Ring.width = newdataAmongPrecip$Prediction), aes(x = AmongPrecip, y = Ring.width), colour = "blue")+
  geom_line(data = data.frame(AmongPrecip = newdataAmongPrecip$AmongPrecip, Ring.width = newdataAmongPrecip$HCI), aes(x = AmongPrecip, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  geom_line(data = data.frame(AmongPrecip = newdataAmongPrecip$AmongPrecip, Ring.width = newdataAmongPrecip$LCI), aes(x = AmongPrecip, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  theme_cowplot()+
  xlab("Average daily precipitation (mm)")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))

#creating multipanel plot for error curves
plot_grid(fagus_precip_error, fagus_temp_error, fagus_VPD_error)


