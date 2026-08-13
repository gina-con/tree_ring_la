#red_fagus_dataset/mean_centred_betulat_dataset/red_quercus_dataset loaded into environment from amongdev_fagus.R/amongdev_betula.R/amongdev_quercus.R
#plotting for supplementary 
#QP = Q. petraea; FS = F. sylvatica; BP = B. pendula
#all plots are plotted using theme_cowplot using the base R colour palette in for consistency in apperance
library(cowplot)
library(tidyverse)

#temperature trend for each country across time for QP, using geom_smooth function which applies a spline using
#a generalised additive model.
quercus_temp_trend <- ggplot(red_quercus_dataset, aes(x=Year, y=Avg_meantemp, colour=Country))+
  geom_smooth()+
  theme_cowplot()+
  ylab("Average mean temperature (°C)")+
  ggtitle("Quercus petraea")+
  theme(legend.title=element_blank(),
        axis.title=element_text(size=10), 
        axis.text=element_text(size=8),
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))
#precipitation trend for each country across time for QP
quercus_precip_trend <- ggplot(red_quercus_dataset, aes(x=Year, y=Avg_precip, colour=Country))+
  geom_smooth()+
  theme_cowplot()+
  ylab("Average precipitation (mm)")+
  theme(legend.title=element_blank(),
        axis.title=element_text(size=10), 
        axis.text=element_text(size=8),
        title=element_text(size=12))
#VPD trend for each country across time for QP
quercus_VPD_trend <- ggplot(red_quercus_dataset, aes(x=Year, y=Avg_VPD, colour=Country))+
  geom_smooth()+
  theme_cowplot()+
  ylab("Average VPD (kPa)")+
  theme(legend.title=element_blank(),
        axis.title=element_text(size=10), 
        axis.text=element_text(size=8),
        title=element_text(size=12))

#the above code, duplicated for FS
#temperature
fagus_temp_trend <- ggplot(red_fagus_dataset, aes(x=Year, y=Avg_meantemp, colour=Country))+
  geom_smooth()+
  theme_cowplot()+
  ylab("Average mean temperature (°C)")+
  ggtitle("Fagus sylvatica")+
  theme(legend.title=element_blank(),
        axis.title=element_text(size=10), 
        axis.text=element_text(size=8),
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))
#precipitation
fagus_precip_trend <- ggplot(red_fagus_dataset, aes(x=Year, y=Avg_precip, colour=Country))+
  geom_smooth()+
  theme_cowplot()+
  ylab("Average precipitation (mm)")+
  theme(legend.title=element_blank(),
        axis.title=element_text(size=10), 
        axis.text=element_text(size=8),
        title=element_text(size=12))
#VPD
fagus_VPD_trend <- ggplot(red_fagus_dataset, aes(x=Year, y=Avg_VPD, colour=Country))+
  geom_smooth()+
  theme_cowplot()+
  ylab("Average VPD (kPa)")+
  theme(legend.title=element_blank(),
        axis.title=element_text(size=10), 
        axis.text=element_text(size=8),
        title=element_text(size=12))

#the above code, duplicated for BP
#temperature
betula_temp_trend <- ggplot(mean_centred_betula_dataset, aes(x=Year, y=Avg_meantemp, colour=Country))+
  geom_smooth()+
  theme_cowplot()+
  ylab("Average mean temperature (°C)")+
  ggtitle("Betula pendula")+
  theme(legend.title=element_blank(),
        axis.title=element_text(size=10), 
        axis.text=element_text(size=8),
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))
#precipitation
betula_precip_trend <- ggplot(mean_centred_betula_dataset, aes(x=Year, y=Avg_precip, colour=Country))+
  geom_smooth()+
  theme_cowplot()+
  ylab("Average precipitation (mm)")+
  theme(legend.title=element_blank(),
        axis.title=element_text(size=10), 
        axis.text=element_text(size=8),
        title=element_text(size=12))
#VPD
betula_VPD_trend <- ggplot(mean_centred_betula_dataset, aes(x=Year, y=Avg_VPD, colour=Country))+
  geom_smooth()+
  theme_cowplot()+
  ylab("Average VPD (kPa)")+
  theme(legend.title=element_blank(),
        axis.title=element_text(size=10), 
        axis.text=element_text(size=8),
        title=element_text(size=12))

#creating a multipanel figure for the above plots, columns: FS, QP, BP; rows: VPD, temp, precip
plot_grid(fagus_VPD_trend, quercus_VPD_trend, betula_VPD_trend,
          fagus_temp_trend, quercus_temp_trend, betula_temp_trend,
          fagus_precip_trend, quercus_precip_trend, betula_precip_trend)

#overall trend across sites across time for each species
#using geom_smooth which uses GAM fitted with spline 
#QP temp
quercus_temp_ov_trend <- ggplot(red_quercus_dataset, aes(x=Year, y=Avg_meantemp))+
  geom_smooth()+
  theme_cowplot()+
  ylab("Average mean temperature (°C)")+
  theme(title=element_text(size=12))
#QP precip
quercus_precip_ov_trend <- ggplot(red_quercus_dataset, aes(x=Year, y=Avg_precip))+
  geom_smooth()+
  theme_cowplot()+
  ylab("Average precipitation (mm)")+
  theme(legend.title=element_blank(),
        title=element_text(size=12))
#QP VPD
quercus_VPD_ov_trend <- ggplot(red_quercus_dataset, aes(x=Year, y=Avg_VPD))+
  geom_smooth()+
  theme_cowplot()+
  ylab("Average VPD (kPa)")+
  ggtitle("Quercus petraea")+
  theme(plot.title = element_text(face = "italic"))+
  theme(legend.title=element_blank(),
        title=element_text(size=12))

#FS temp
fagus_temp_ov_trend <- ggplot(red_fagus_dataset, aes(x=Year, y=Avg_meantemp))+
  geom_smooth()+
  theme_cowplot()+
  ylab("Average mean temperature (°C)")+
  theme(legend.title=element_blank(),
        title=element_text(size=12))
#FS precip
fagus_precip_ov_trend <- ggplot(red_fagus_dataset, aes(x=Year, y=Avg_precip))+
  geom_smooth()+
  theme_cowplot()+
  ylab("Average precipitation (mm)")+
  theme(legend.title=element_blank(),
        title=element_text(size=12))
#FS VPD
fagus_VPD_ov_trend <- ggplot(red_fagus_dataset, aes(x=Year, y=Avg_VPD))+
  geom_smooth()+
  theme_cowplot()+
  ylab("Average VPD (kPa)")+
  ggtitle("Fagus sylvatica")+
  theme(legend.title=element_blank(),
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))

#BP temp
betula_temp_ov_trend <- ggplot(mean_centred_betula_dataset, aes(x=Year, y=Avg_meantemp))+
  geom_smooth()+
  theme_cowplot()+
  ylab("Average mean temperature (°C)")+
  theme(legend.title=element_blank(),
        title=element_text(size=12))
#BP precip
betula_precip_ov_trend <- ggplot(mean_centred_betula_dataset, aes(x=Year, y=Avg_precip))+
  geom_smooth()+
  theme_cowplot()+
  ylab("Average precipitation (mm)")+
  theme(legend.title=element_blank(),
        title=element_text(size=12))
#BP VPD 
betula_VPD_ov_trend <- ggplot(mean_centred_betula_dataset, aes(x=Year, y=Avg_VPD))+
  geom_smooth()+
  theme_cowplot()+
  ylab("Average VPD (kPa)")+
  ggtitle("Betula pendula")+
  theme(title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))

plot_grid(fagus_VPD_ov_trend, quercus_VPD_ov_trend, betula_VPD_ov_trend,
          fagus_temp_ov_trend, quercus_temp_ov_trend, betula_temp_ov_trend,
          fagus_precip_ov_trend, quercus_precip_ov_trend, betula_precip_ov_trend)

#plotting the range of climate across countries sites included for each species, using boxplot to represent
#median, 25% and 75% quantile data falls within and outlier values
#plotting span of temp across the different countries sampled for QP
quercus_temp_data <- ggplot(red_quercus_dataset, aes(x=Country, y=Avg_meantemp, fill=Country))+
  geom_boxplot()+
  theme_cowplot()+
  ylab('Average daily mean temperature (°C)')+
  xlab('Country')+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  theme(axis.text=element_text(size=8))+
  theme(legend.position="none")

#QP VPD boxplot
quercus_VPD_data <- ggplot(red_quercus_dataset, aes(x=Country, y=Avg_VPD, fill=Country))+
  geom_boxplot()+
  theme_cowplot()+
  ylab('Average daily VPD (kPa)')+
  xlab('Country')+
  ggtitle('Quercus petraea')+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12),
        title=element_text(size=12))+
  theme(axis.text=element_text(size=8))+
  theme(legend.position="none")+
  theme(plot.title = element_text(face = "italic"))

#QP precip boxplot
quercus_precip_data <- ggplot(red_quercus_dataset, aes(x=Country, y=Avg_precip, fill=Country))+
  geom_boxplot()+
  theme_cowplot()+
  ylab('Average daily precipitation (mm)')+
  xlab('Country')+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  theme(axis.text=element_text(size=8))+
  theme(legend.position="none")


#repeating for FS
#FS temp boxplot
fagus_temp_data <- ggplot(red_fagus_dataset, aes(x=Country, y=Avg_meantemp, fill=Country))+
  geom_boxplot()+
  theme_cowplot()+
  ylab('Average daily mean temperature (°C)')+
  xlab('Country')+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  theme(axis.text=element_text(size=8))+
  theme(legend.position="none")

#FS VPD boxplot
fagus_VPD_data <- ggplot(red_fagus_dataset, aes(x=Country, y=Avg_VPD, fill=Country))+
  geom_boxplot()+
  theme_cowplot()+
  ylab('Average daily VPD')+
  xlab('Country')+
  ggtitle("Fagus sylvatica")+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12), 
        title=element_text(size=12))+
  theme(axis.text=element_text(size=8))+
  theme(plot.title = element_text(face = "italic"))+
  theme(legend.position="none")

#FS precip boxplot
fagus_precip_data <- ggplot(red_fagus_dataset, aes(x=Country, y=Avg_precip, fill=Country))+
  geom_boxplot()+
  theme_cowplot()+
  ylab('Average daily precipitation (mm)')+
  xlab('Country')+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  theme(axis.text=element_text(size=8))+
  theme(legend.position="none")

#repeating for BP
#range of climate across countries
#BP temp boxplot
betula_temp_data <- ggplot(mean_centred_betula_dataset, aes(x=Country, y=Avg_meantemp, fill=Country))+
  geom_boxplot()+
  theme_cowplot()+
  ylab('Average daily mean temperature (°C)')+
  xlab('Country')+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  theme(axis.text=element_text(size=8))+
  theme(legend.position="none")

#BP VPD boxplot
betula_VPD_data <- ggplot(mean_centred_betula_dataset, aes(x=Country, y=Avg_VPD, fill=Country))+
  geom_boxplot()+
  theme_cowplot()+
  ylab('Average daily VPD')+
  xlab('Country')+
  ggtitle("Betula pendula")+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        title=element_text(size=12))+
  theme(axis.text=element_text(size=8))+
  theme(legend.position="none")+
  theme(plot.title = element_text(face = "italic"))

#BP precip boxplot
betula_precip_data <- ggplot(mean_centred_betula_dataset, aes(x=Country, y=Avg_precip, fill=Country))+
  geom_boxplot()+
  theme_cowplot()+
  ylab('Average daily precipitation (mm)')+
  xlab('Country')+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  theme(axis.text=element_text(size=8))+
  theme(legend.position="none")

#creating a multipanel figure for the above plots, columns: FS, QP, BP; rows: VPD, temp, precip
plot_grid(fagus_VPD_data, quercus_VPD_data, betula_VPD_data,
          fagus_temp_data, quercus_temp_data, betula_temp_data,
          fagus_precip_data, quercus_precip_data, betula_precip_data)

#plotting graphs demonstrating 95% confidence intervals (CI) around the spatial response curves
#code to be run after generating newdataAmongVPD/newdataAmongTemp/newdataAmongVPD datasets from
#amongdev_fagus.R/amongdev_betula.R/amongdev_quercus.R code

#FS VPD spatial CI 
fagus_VPD_error <- ggplot(red_fagus_dataset, aes(x=AmongVPDDev, y=log1p(Ring.width)))+
  geom_point()+
  geom_line(data = data.frame(AmongVPDDev = newdataAmongVPD$AmongVPDDev, Ring.width = newdataAmongVPD$Prediction), aes(x = AmongVPDDev, y = Ring.width), colour = "blue")+
  geom_line(data = data.frame(AmongVPDDev = newdataAmongVPD$AmongVPDDev, Ring.width = newdataAmongVPD$HCI), aes(x = AmongVPDDev, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  geom_line(data = data.frame(AmongVPDDev = newdataAmongVPD$AmongVPDDev, Ring.width = newdataAmongVPD$LCI), aes(x = AmongVPDDev, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  theme_cowplot()+
  xlab("Deviation from global average VPD (kPa)")+
  ylab("Ring width (log+1)")+
  ggtitle("Fagus sylvatica")+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))

#FS temp spatial CI
fagus_temp_error <- ggplot(red_fagus_dataset, aes(x=AmongTempDev, y=log1p(Ring.width)))+
  geom_point()+
  geom_line(data = data.frame(AmongTempDev = newdataAmongTemp$AmongTempDev, Ring.width = newdataAmongTemp$Prediction), aes(x = AmongTempDev, y = Ring.width), colour = "blue")+
  geom_line(data = data.frame(AmongTempDev = newdataAmongTemp$AmongTempDev, Ring.width = newdataAmongTemp$HCI), aes(x = AmongTempDev, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  geom_line(data = data.frame(AmongTempDev = newdataAmongTemp$AmongTempDev, Ring.width = newdataAmongTemp$LCI), aes(x = AmongTempDev, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  theme_cowplot()+
  xlab("Deviation from global average temperature(°C)")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12))

#FS precip spatial CI
fagus_precip_error <- ggplot(red_fagus_dataset, aes(x=AmongPrecipDev, y=log1p(Ring.width)))+
  geom_point()+
  geom_line(data = data.frame(AmongPrecipDev = newdataAmongPrecip$AmongPrecipDev, Ring.width = newdataAmongPrecip$Prediction), aes(x = AmongPrecipDev, y = Ring.width), colour = "blue")+
  geom_line(data = data.frame(AmongPrecipDev = newdataAmongPrecip$AmongPrecipDev, Ring.width = newdataAmongPrecip$HCI), aes(x = AmongPrecipDev, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  geom_line(data = data.frame(AmongPrecipDev = newdataAmongPrecip$AmongPrecipDev, Ring.width = newdataAmongPrecip$LCI), aes(x = AmongPrecipDev, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  theme_cowplot()+
  xlab("Deviation from global average precipitation (mm)")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))

#QS VPD spatial CI
quercus_VPD_error <- ggplot(red_quercus_dataset, aes(x=AmongVPDDev, y=log1p(Ring.width)))+
  geom_point()+
  geom_line(data = data.frame(AmongVPDDev = newdataAmongVPD$AmongVPDDev, Ring.width = newdataAmongVPD$Prediction), aes(x = AmongVPDDev, y = Ring.width), colour = "blue")+
  geom_line(data = data.frame(AmongVPDDev = newdataAmongVPD$AmongVPDDev, Ring.width = newdataAmongVPD$HCI), aes(x = AmongVPDDev, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  geom_line(data = data.frame(AmongVPDDev = newdataAmongVPD$AmongVPDDev, Ring.width = newdataAmongVPD$LCI), aes(x = AmongVPDDev, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  theme_cowplot()+
  xlab("Deviation from global average VPD (kPa)")+
  ylab("Ring width (log+1)")+
  ggtitle("Quercus petraea")+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))
  
#QS spatial temp CI
quercus_temp_error <- ggplot(red_quercus_dataset, aes(x=AmongTempDev, y=log1p(Ring.width)))+
  geom_point()+
  geom_line(data = data.frame(AmongTempDev = newdataAmongTemp$AmongTempDev, Ring.width = newdataAmongTemp$Prediction), aes(x = AmongTempDev, y = Ring.width), colour = "blue")+
  geom_line(data = data.frame(AmongTempDev = newdataAmongTemp$AmongTempDev, Ring.width = newdataAmongTemp$HCI), aes(x = AmongTempDev, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  geom_line(data = data.frame(AmongTempDev = newdataAmongTemp$AmongTempDev, Ring.width = newdataAmongTemp$LCI), aes(x = AmongTempDev, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  theme_cowplot()+
  xlab("Deviation from global average temperature(°C)")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12))
#QS spatial precip error
quercus_precip_error <- ggplot(red_quercus_dataset, aes(x=AmongPrecipDev, y=log1p(Ring.width)))+
  geom_point()+
  geom_line(data = data.frame(AmongPrecip = newdataAmongPrecip$AmongPrecipDev, Ring.width = newdataAmongPrecip$Prediction), aes(x = AmongPrecip, y = Ring.width), colour = "blue")+
  geom_line(data = data.frame(AmongPrecip = newdataAmongPrecip$AmongPrecipDev, Ring.width = newdataAmongPrecip$HCI), aes(x = AmongPrecip, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  geom_line(data = data.frame(AmongPrecip = newdataAmongPrecip$AmongPrecipDev, Ring.width = newdataAmongPrecip$LCI), aes(x = AmongPrecip, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  theme_cowplot()+
  xlab("Deviation from global average precipitation (mm)")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))

#BP VPD spatial CI
betula_VPD_error <- ggplot(mean_centred_betula_dataset, aes(x=AmongVPDDev, y=log1p(Ring.width)))+
  geom_point()+
  geom_line(data = data.frame(AmongVPDDev = newdataAmongVPD$AmongVPDDev, Ring.width = newdataAmongVPD$Prediction), aes(x = AmongVPDDev, y = Ring.width), colour = "blue")+
  geom_line(data = data.frame(AmongVPDDev = newdataAmongVPD$AmongVPDDev, Ring.width = newdataAmongVPD$HCI), aes(x = AmongVPDDev, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  geom_line(data = data.frame(AmongVPDDev = newdataAmongVPD$AmongVPDDev, Ring.width = newdataAmongVPD$LCI), aes(x = AmongVPDDev, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  theme_cowplot()+
  xlab("Deviation from global average VPD (kPa)")+
  ylab("Ring width (log+1)")+
  ggtitle("Betula pendula")+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))
  
#BP temp spatial CI
betula_temp_error <- ggplot(mean_centred_betula_dataset, aes(x=AmongTempDev, y=log1p(Ring.width)))+
  geom_point()+
  geom_line(data = data.frame(AmongTempDev = newdataAmongTemp$AmongTempDev, Ring.width = newdataAmongTemp$Prediction), aes(x = AmongTempDev, y = Ring.width), colour = "blue")+
  geom_line(data = data.frame(AmongTempDev = newdataAmongTemp$AmongTempDev, Ring.width = newdataAmongTemp$HCI), aes(x = AmongTempDev, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  geom_line(data = data.frame(AmongTempDev = newdataAmongTemp$AmongTempDev, Ring.width = newdataAmongTemp$LCI), aes(x = AmongTempDev, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  theme_cowplot()+
  xlab("Deviation from global average temperature(°C)")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))
#BP precip spatial CI
betula_precip_error <- ggplot(mean_centred_betula_dataset, aes(x=AmongPrecipDev, y=log1p(Ring.width)))+
  geom_point()+
  geom_line(data = data.frame(AmongPrecip = newdataAmongPrecip$AmongPrecipDev, Ring.width = newdataAmongPrecip$Prediction), aes(x = AmongPrecip, y = Ring.width), colour = "blue")+
  geom_line(data = data.frame(AmongPrecip = newdataAmongPrecip$AmongPrecipDev, Ring.width = newdataAmongPrecip$HCI), aes(x = AmongPrecip, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  geom_line(data = data.frame(AmongPrecip = newdataAmongPrecip$AmongPrecipDev, Ring.width = newdataAmongPrecip$LCI), aes(x = AmongPrecip, y = Ring.width), colour = "darkgrey", linetype='dashed')+
  theme_cowplot()+
  xlab("Deviation from global average precipitation (mm)")+
  ylab("Ring width (log+1)")+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))

#creating a multipanel figure for the above plots, columns: FS, QP, BP; rows: VPD, temp, precip
plot_grid(fagus_VPD_error, quercus_VPD_error, betula_VPD_error,
          fagus_temp_error, quercus_temp_error, betula_temp_error,
          fagus_precip_error, quercus_precip_error, betula_precip_error)



#plot for schematic to demonstrate how spatial and temporal variation was calculated
#plots showing observations for mean temperature across years for three different sites
#with a line demonstrating the average mean temperature for each site
red_fagus_dataset_site_1 <- red_fagus_dataset %>% 
  filter(SiteID %in% 'ATFS13')
ggplot(data=red_fagus_dataset_site_1, aes(x=Year, y=Avg_meantemp))+
  geom_point()+
  geom_hline(yintercept = mean(red_fagus_dataset_site_1$Avg_meantemp), color="blue")+
  theme_cowplot()+
  ylab("Average daily mean temperature (°C)")+
  ggtitle(red_fagus_dataset_site_1$SiteID)

red_fagus_dataset_site_2 <- red_fagus_dataset %>% 
  filter(SiteID %in% 'CHFS11')
ggplot(data=red_fagus_dataset_site_2, aes(x=Year, y=Avg_meantemp))+
  geom_point()+
  geom_hline(yintercept = mean(red_fagus_dataset_site_2$Avg_meantemp), color="blue")+
  theme_cowplot()+
  ylab("Average daily mean temperature (°C)")+
  ggtitle(red_fagus_dataset_site_2$SiteID)

red_fagus_dataset_site_3 <- red_fagus_dataset %>% 
  filter(SiteID %in% 'FRFS03')
ggplot(data=red_fagus_dataset_site_3, aes(x=Year, y=Avg_meantemp))+
  geom_point()+
  geom_hline(yintercept = mean(red_fagus_dataset_site_3$Avg_meantemp), color="blue")+
  theme_cowplot()+
  ylab("Average daily mean temperature (°C)")+
  ggtitle(red_fagus_dataset_site_3$SiteID)
