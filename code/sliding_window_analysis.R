#loading the data in in long format as previously extracted in data_loading.R file
#for fagus sylvatica analysis
maxtemp_fagus_sites <- read.csv("Data/e_obs/MeanTempsitedata.csv", header=TRUE, sep=",", check.names=FALSE)
meantemp_fagus_sites <- read.csv("Data/e_obs/meantempsitedata.csv", header=TRUE, sep=",", check.names=FALSE)
relhum_fagus_sites <- read.csv("Data/e_obs/relhumsitedata.csv", header=TRUE, sep=",", check.names=FALSE)
precip_fagus_sites <- read.csv("Data/e_obs/precip.csv", header=TRUE, sep=",", check.names=FALSE)

#checking to see what is in the files 
head(maxtemp_fagus_sites)
head(meantemp_fagus_sites)
head(relhum_fagus_sites)
head(precip_fagus_sites)

#Putting data into the correct format
#renaming the column with site IDs in to SiteID 

colnames(maxtemp_fagus_sites)[1] = 'SiteID'
colnames(meantemp_fagus_sites)[1] = 'SiteID'
colnames(relhum_fagus_sites)[1] = 'SiteID'
colnames(precip_fagus_sites)[1] = 'SiteID'

#putting into long format to seperate year, month and day - can then filter only relevant months and paste columns together
#maxtemp
library(tidyr)
maxtemp_fagus_sites_long <- maxtemp_fagus_sites %>%
  pivot_longer(cols=X1950.01.01:X2024.12.31,
               names_to = "Date",
               values_to = "maxtemp")%>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "\\.")

#removing X from year 
library(stringr)
library(dplyr)
maxtemp_fagus_sites_long <- maxtemp_fagus_sites_long %>%
  mutate_at("Year", str_replace, "X", "")

#filtering down to just April-August (for present year only analysis)
months <- c("04", "05", "06", "07", "08")
maxtemp_fagus_summer <- data.frame()

for(i in 1:length(months)){
  subset <- maxtemp_fagus_sites_long %>% 
    filter(Month %in% months[i])
  maxtemp_fagus_summer <- rbind(maxtemp_fagus_summer, subset)
}

#pasting together site and year, and month and day for column name when converting back to wide format
maxtemp_fagus_summer <- within(maxtemp_fagus_summer, Site_year <- paste(SiteID, Year,sep='_'))
maxtemp_fagus_summer <- within(maxtemp_fagus_summer, Day_month <- paste(Day, Month,sep='_')) ##need this to be just the ordinal day not month and day
maxtemp_fagus_summer <- maxtemp_fagus_summer[,c(6,7,5)]

#putting back into wide format for sliding window analysis
maxtemp_fagus_sites_wide<- maxtemp_fagus_summer %>%
  pivot_wider(id_cols="Site_year",
               names_from = "Day_month",
               values_from = "maxtemp")

#renaming the columns so that they correspond to the ordinal day
colnames(maxtemp_fagus_sites_wide) <- c(90:243)
names(maxtemp_fagus_sites_wide)[names(maxtemp_fagus_sites_wide) == '90'] <- 'Site_year'

##
#meantemp
meantemp_fagus_sites_long <- meantemp_fagus_sites %>%
  pivot_longer(cols=X1950.01.01:X2024.12.31,
               names_to = "Date",
               values_to = "MeanTemp")%>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "\\.")

#removing X from year 
meantemp_fagus_sites_long <- meantemp_fagus_sites_long %>%
  mutate_at("Year", str_replace, "X", "")

#filtering down to just April-August (for present year only analysis)
months <- c("04", "05", "06", "07", "08")
meantemp_fagus_summer <- data.frame()

for(i in 1:length(months)){
  subset <- meantemp_fagus_sites_long %>% 
    filter(Month %in% months[i])
  meantemp_fagus_summer <- rbind(meantemp_fagus_summer, subset)
}

#pasting together site and year, and month and day for column name when converting back to wide format
meantemp_fagus_summer <- within(meantemp_fagus_summer, Site_year <- paste(SiteID, Year,sep='_'))
meantemp_fagus_summer <- within(meantemp_fagus_summer, Day_month <- paste(Day, Month,sep='_')) ##need this to be just the ordinal day not month and day
meantemp_fagus_summer <- meantemp_fagus_summer[,c(6,7,5)]

#putting back into wide format for sliding window analysis
meantemp_fagus_sites_wide<- meantemp_fagus_summer %>%
  pivot_wider(id_cols="Site_year",
              names_from = "Day_month",
              values_from = "MeanTemp")

#renaming the columns so that they correspond to the ordinal day
colnames(meantemp_fagus_sites_wide) <- c(90:243)
names(meantemp_fagus_sites_wide)[names(meantemp_fagus_sites_wide) == '90'] <- 'Site_year'

##
#precip
precip_fagus_sites_long <- precip_fagus_sites %>%
  pivot_longer(cols=X1950.01.01:X2024.12.31,
               names_to = "Date",
               values_to = "Precip")%>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "\\.")

#removing X from year 
precip_fagus_sites_long <- precip_fagus_sites_long %>%
  mutate_at("Year", str_replace, "X", "")

#filtering down to just April-August (for present year only analysis)
months <- c("04", "05", "06", "07", "08")
precip_fagus_summer <- data.frame()

for(i in 1:length(months)){
  subset <- precip_fagus_sites_long %>% 
    filter(Month %in% months[i])
  precip_fagus_summer <- rbind(precip_fagus_summer, subset)
}

#pasting together site and year, and month and day for column name when converting back to wide format
precip_fagus_summer <- within(precip_fagus_summer, Site_year <- paste(SiteID, Year,sep='_'))
precip_fagus_summer <- within(precip_fagus_summer, Day_month <- paste(Day, Month,sep='_')) ##need this to be just the ordinal day not month and day
precip_fagus_summer <- precip_fagus_summer[,c(6,7,5)]

#putting back into wide format for sliding window analysis
precip_fagus_sites_wide<- precip_fagus_summer %>%
  pivot_wider(id_cols="Site_year",
              names_from = "Day_month",
              values_from = "Precip")

#renaming the columns so that they correspond to the ordinal day
colnames(precip_fagus_sites_wide) <- c(90:243)
names(precip_fagus_sites_wide)[names(precip_fagus_sites_wide) == '90'] <- 'Site_year'

##
#relhum 
relhum_fagus_sites_long <- relhum_fagus_sites %>%
  pivot_longer(cols=X1950.01.01:X2024.12.31,
               names_to = "Date",
               values_to = "RelHum")%>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "\\.")

#removing X from year 
relhum_fagus_sites_long <- relhum_fagus_sites_long %>%
  mutate_at("Year", str_replace, "X", "")

#filtering down to just April-August (for present year only analysis)
months <- c("04", "05", "06", "07", "08")
relhum_fagus_summer <- data.frame()

for(i in 1:length(months)){
  subset <- relhum_fagus_sites_long %>% 
    filter(Month %in% months[i])
  relhum_fagus_summer <- rbind(relhum_fagus_summer, subset)
}

##
#calculating VPD of the air for each day 
#actual VPD = (RH*VPsat)/100
#VPsat = 0.6108*exp(17.27*T/T+237.3)
#need to deal with VPD NAs 
VPD_dataset <- relhum_fagus_summer
VPD_dataset$MeanTemp <- meantemp_fagus_summer$MeanTemp

VPD_dataset$SVP <- 0.6108*exp((17.27*VPD_dataset$MeanTemp)/(VPD_dataset$MeanTemp+237.3))
VPD_dataset$AVP <- ((VPD_dataset$RelHum/100)*VPD_dataset$SVP)
VPD_dataset$VPD <- VPD_dataset$SVP-VPD_dataset$AVP

#pasting together site and year, and month and day for column name when converting back to wide format
VPD_dataset <- within(VPD_dataset, Site_year <- paste(SiteID, Year,sep='_'))
VPD_dataset <- within(VPD_dataset, Day_month <- paste(Day, Month,sep='_')) ##need this to be just the ordinal day not month and day
VPD_dataset <- VPD_dataset[,c(10,11,9)]

#putting into wide
VPD_fagus_sites_wide<- VPD_dataset %>%
  pivot_wider(id_cols="Site_year",
              names_from = "Day_month",
              values_from = "VPD")

#renaming column names
colnames(VPD_fagus_sites_wide) <- c(90:243)
names(VPD_fagus_sites_wide)[names(VPD_fagus_sites_wide) == '90'] <- 'Site_year'

##
#removing unnecessary duplicates
rm(meantemp_fagus_summer)
rm(meantemp_fagus_sites_long)
rm(meantemp_fagus_sites)


##
##sliding window analysis

##starting with a duration of 20 days
#weekly start dates
growth_start_date <- seq(2,154,7)
#different duration starting from 5 going up to 20
growth_duration <- seq(5, 50, 10)
#calculating different windows for each start date at different durations
growth_windows <- data.frame(start_date=rep(growth_start_date,1, each=length(growth_duration)),
                             growth_duration=rep(growth_duration,length(growth_start_date)))
#creating end dates for each
growth_windows$end <- growth_windows$start_date+growth_windows$growth_duration-1
#removing windows that end after the extent of the data
growth_windows <- growth_windows[-which(growth_windows$end>154),]
#giving the windows an ID 
growth_windows$windowID <- paste0(colnames(VPD_fagus_sites_wide)[growth_windows$start_date],"_",
                                  growth_windows$growth_duration, "days")


# create and empty plot with x axis for the number of days of temp data and y for each window
plot(NA, xlim=c(0,154), ylim=c(1,nrow(growth_windows)), xlab="Column number", ylab="Different windows") 
# Use a loop to plot each window
for(i in 1:nrow(growth_windows)){ 
  points(growth_windows[i,c("start_date","end")], c(i,i), type="l") 
}

#null model for comparison
#need to take the null from previous analysis
#fagus_sylv_dat has no climate data
library(glmmTMB)
library(dplyr)
library(splines)
library(AICcmodavg)

#removing trees not ranked A from the raw data 
#filtering so only trees that have rank of A 
tree_ring_data <- read.csv("data/gentree_data/trw_long_format.csv")
tree_data <- read.csv("data/gentree_data/tree.csv")
site_data <- read.csv("data/gentree_data/site.csv")

#adding elevation and latitude data 
elevdata <- data.frame(site_data$SiteID, site_data$Elevation)
colnames(elevdata) <- c("SiteID", "Elevation")
compl_data <- left_join(tree_ring_data, elevdata, "SiteID")

latdata <- data.frame(site_data$SiteID, site_data$Latitude)
colnames(latdata) <- c("SiteID", "Latitude")
compl_data <- left_join(compl_data, latdata, "SiteID")

#filtering to fagus sylvatica
library(dplyr)
fagus_sylv_dat <- compl_data %>% 
  filter(Species %in% "Fagus sylvatica")

#filtering trees to only A ranked trees
a_rank <- tree_data %>%
  filter(Crossdating %in% "A")

#creating a list of names of trees ranked A 
a_trees <- as.list(unique(a_rank$TreeID))

#selecting only trees ranked A from Fagus data 
fagus_sylv_dat <- fagus_sylv_dat %>% 
  filter(TreeID %in% a_trees)

#removing Spain site due to abnormally low temp readings 
fagus_sylv_dat <- fagus_sylv_dat %>% 
  filter(Country != 'Spain')

#adding centred year 
fagus_sylv_dat$Centred_year <- as.numeric(fagus_sylv_dat$Year) - median(as.numeric(fagus_sylv_dat$Year))

#need to create Site_year column for dataset for left join function in loop
fagus_sylv_dat$Site_year <- apply(fagus_sylv_dat[ , c(1,7)] , 1 , paste , collapse = "_" )

#running null model
null_model_fagus <- glmmTMB(log1p(Ring.width)~Latitude+(1|Year)+(1|SiteID)+
                        ns(Centred_year, df=3)+
                        (1+ns(Centred_year, df=3)|TreeID)+
                        (1|Centred_year:SiteID),
                      data=fagus_sylv_dat)

#empty data frame for results 
slidingwindow <- data.frame(matrix(NA, ncol=6, nrow=nrow(growth_windows)))
colnames(slidingwindow) <- c("windowID", "start_date", "end", "deltaAICc", "MeanTemp_coef", "MeanTemp_SE")

for(i in 1:nrow(growth_windows)){
  climate_data <- data.frame(Site_year = maxtemp_fagus_sites_wide$Site_year,
                             window_maxtemp = rowMeans(maxtemp_fagus_sites_wide[,growth_windows$start_date[i]:growth_windows$end[i]]),
                             window_meantemp = rowMeans(meantemp_fagus_sites_wide[,growth_windows$start_date[i]:growth_windows$end[i]]),
                             window_precip = rowMeans(precip_fagus_sites_wide[,growth_windows$start_date[i]:growth_windows$end[i]]),
                             window_VPD = rowMeans(VPD_fagus_sites_wide[,growth_windows$start_date[i]:growth_windows$end[i]], na.rm=T))
  #need to join temperature and tree data here
  climate_window_amongwithin <- left_join(fagus_sylv_dat, climate_data, by="Site_year")
  #remove years where there is no data available (pre-1950)
  climate_window_amongwithin_excl <- climate_window_amongwithin %>%
    filter(!is.na(window_maxtemp))%>%
    filter(!is.na(window_meantemp))%>%
    filter(!is.na(window_precip))%>%
    filter(!is.na(window_VPD))
  
  #calculating among/within effects 
  fagus_populations <- unique(fagus_sylv_dat$SiteID)
  
  #creating df to put among within effects in 
  window_df <- data.frame(matrix(NA, ncol=32, nrow=0))
  colnames(window_df) <- c("SiteID", "TreeID", "Country",         
                                   "Species","Site", "Tree",              
                                   "Year", "Ring.width", "Elevation",     
                                   "Latitude", "Centred_year", "Site_year",      
                                   "window_maxtemp", "AmongMaxTemp", "WithinMaxTemp",    
                                   "WithinMaxTempQuad4", "AmongMaxTempQuad", 
                                   "window_meantemp", "AmongMeanTemp", "WithinMeanTemp",    
                                   "WithinMeanTempQuad4", "AmongMeanTempQuad",
                                   "window_precip", "AmongPrecip", "WithinPrecip",    
                                   "WithinPrecipQuad4", "AmongPrecipQuad",
                                   "window_VPD", "AmongVPD", "WithinVPD",    
                                   "WithinVPDQuad4", "AmongVPDQuad")

  #mean centering to calculate within and among/between site effects
  #among represents the average temperature at each site 
  #within represents the deviation each year from the each site's average
  for (n in 1:length(fagus_populations)){
    subset <- climate_window_amongwithin_excl %>% 
      filter(SiteID == fagus_populations[n])
    
    #max temp
    subset$AmongMaxTemp <- mean(subset$window_maxtemp)#among is the mean of each site
    subset$WithinMaxTemp <- subset$window_maxtemp-(mean(subset$window_maxtemp))#within is the deviation of each observation from the mean of each site
    subset$WithinMaxTempQuad4 <- (subset$window_maxtemp-(mean(subset$window_maxtemp)))^2 #quadratic calculated using equation 4 from Fay et al
    #equation 4 assumes that the effect of temperature varies among populations (which would be the case if locally adapted)
    subset$AmongMaxTempQuad <- mean(subset$window_maxtemp)^2
    
    #
    #mean temp
    subset$AmongMeanTemp <- mean(subset$window_meantemp)#among is the mean of each site
    subset$WithinMeanTemp <- subset$window_meantemp-(mean(subset$window_meantemp))#within is the deviation of each observation from the mean of each site
    subset$WithinMeanTempQuad4 <- (subset$window_meantemp-(mean(subset$window_meantemp)))^2 #quadratic calculated using equation 4 from Fay et al
    #equation 4 assumes that the effect of temperature varies among populations (which would be the case if locally adapted)
    subset$AmongMeanTempQuad <- mean(subset$window_meantemp)^2
    
    #
    #Precip
    subset$AmongPrecip <- mean(subset$window_precip)#among is the mean of each site
    subset$WithinPrecip <- subset$window_precip-(mean(subset$window_precip))#within is the deviation of each observation from the mean of each site
    subset$WithinPrecipQuad4 <- (subset$window_precip-(mean(subset$window_precip)))^2 #quadratic calculated using equation 4 from Fay et al
    #equation 4 assumes that the effect of temperature varies among populations (which would be the case if locally adapted)
    subset$AmongPrecipQuad <- mean(subset$window_precip)^2
    
    #
    #VPD
    subset$AmongVPD <- mean(subset$window_VPD)#among is the mean of each site
    subset$WithinVPD <- subset$window_VPD-(mean(subset$window_VPD))#within is the deviation of each observation from the mean of each site
    subset$WithinVPDQuad4 <- (subset$window_VPD-(mean(subset$window_VPD)))^2 #quadratic calculated using equation 4 from Fay et al
    #equation 4 assumes that the effect of temperature varies among populations (which would be the case if locally adapted)
    subset$AmongVPDQuad <- mean(subset$window_VPD)^2
    
    #adding to sliding window dataframe
    window_df <- rbind(window_df, subset)
  }
  
  #calculating among deviation (deviation from global average)
  window_df$AmongMaxTempDev<- window_df$AmongMaxTemp - mean(window_df$window_maxtemp)
  window_df$AmongMeanTempDev<- window_df$AmongMeanTemp - mean(window_df$window_meantemp)
  window_df$AmongPrecipDev<- window_df$AmongPrecip - mean(window_df$window_precip)
  window_df$AmongVPDDev<- window_df$AmongVPD - mean(window_df$window_VPD)
  #calculating quadratic (quadratic deviations from global average)
  window_df$AmongMaxTempDevQuad<- window_df$AmongMaxTempDev^2
  window_df$AmongMeanTempDevQuad<- window_df$AmongMeanTempDev^2
  window_df$AmongPrecipDevQuad<- window_df$AmongPrecipDev^2
  window_df$AmongVPDDevQuad<- window_df$AmongVPDDev^2
  #calculating quadratic of original climate data for global optimum model
  window_df$Avg_MaxTempQuad<- window_df$window_maxtemp^2
  window_df$Avg_MeanTempQuad<- window_df$window_meantemp^2
  window_df$Avg_PrecipQuad<- window_df$window_precip^2
  window_df$Avg_VPDQuad<- window_df$window_VPD^2
  
  #making sure Year is categorical not numerical
  window_df$Year <- as.character(window_df$Year)
  
  #runningm model here 
  mod <- glmmTMB(log1p(Ring.width)~AmongMaxTempDev+AmongMeanTempDev+AmongPrecipDev+AmongVPDDev+
                                        AmongMaxTempDevQuad+AmongMeanTempDevQuad+AmongPrecipDevQuad+AmongVPDDevQuad+
                                        WithinMaxTemp+WithinMeanTemp+WithinPrecip+WithinVPD+
                                        WithinMaxTempQuad4+WithinMeanTempQuad4+WithinPrecipQuad4+WithinVPDQuad4+
                                        (AmongMaxTempDev*WithinMaxTemp)+
                                        (AmongMeanTempDev*WithinMeanTemp)+
                                        (AmongPrecipDev*WithinPrecip)+
                                        (AmongVPDDev*WithinVPD)+
                                        Latitude+
                                        (1|Year)+(1|SiteID)+
                                        ns(Centred_year, df=3)+
                                        (1+ns(Centred_year, df=3)|TreeID)+
                                        (1|Centred_year:SiteID),
                                      data=window_df)
  #storing the AIC, coef etc 
  slidingwindow$windowID[i] <- growth_windows$windowID[i]
  slidingwindow$start_date[i] <- growth_windows$start_date[i]+90 
  slidingwindow$end[i] <- growth_windows$end[i]+90 
  slidingwindow$deltaAICc[i] <- AICc(mod)-AICc(null_model_fagus)
  #max temp coeffs
  slidingwindow$MaxTempamong_coef[i] <- summary(mod)$coefficients$cond[2,1]
  slidingwindow$MaxTempwithin_coef[i] <- summary(mod)$coefficients$cond[4,1]
  slidingwindow$MaxTempamongquad_coef[i] <- summary(mod)$coefficients$cond[3,1]
  slidingwindow$MaxTempwithinquad_coef[i] <- summary(mod)$coefficients$cond[5,1]
  #SE
  slidingwindow$MaxTempamong_SE[i] <- summary(mod)$coefficients$cond[2,2]
  slidingwindow$MaxTempwithin_SE[i] <- summary(mod)$coefficients$cond[4,2]
  slidingwindow$MaxTempamongquad_SE[i] <- summary(mod)$coefficients$cond[3,2]
  slidingwindow$MaxTempwithinquad_SE[i] <- summary(mod)$coefficients$cond[5,2]
  
  #mean temp coeffs
  slidingwindow$meantempamong_coef[i] <- summary(mod)$coefficients$cond[2,1]
  slidingwindow$meantempwithin_coef[i] <- summary(mod)$coefficients$cond[4,1]
  slidingwindow$meantempamongquad_coef[i] <- summary(mod)$coefficients$cond[3,1]
  slidingwindow$meantempwithinquad_coef[i] <- summary(mod)$coefficients$cond[5,1]
  #SE
  slidingwindow$meantempamong_SE[i] <- summary(mod)$coefficients$cond[2,2]
  slidingwindow$meantempwithin_SE[i] <- summary(mod)$coefficients$cond[4,2]
  slidingwindow$meantempamongquad_SE[i] <- summary(mod)$coefficients$cond[3,2]
  slidingwindow$meantempwithinquad_SE[i] <- summary(mod)$coefficients$cond[5,2]
  
  #precip coeffs
  slidingwindow$precipamong_coef[i] <- summary(mod)$coefficients$cond[2,1]
  slidingwindow$precipwithin_coef[i] <- summary(mod)$coefficients$cond[4,1]
  slidingwindow$precipamongquad_coef[i] <- summary(mod)$coefficients$cond[3,1]
  slidingwindow$precipwithinquad_coef[i] <- summary(mod)$coefficients$cond[5,1]
  #SE
  slidingwindow$precipamong_SE[i] <- summary(mod)$coefficients$cond[2,2]
  slidingwindow$precipwithin_SE[i] <- summary(mod)$coefficients$cond[4,2]
  slidingwindow$precipamongquad_SE[i] <- summary(mod)$coefficients$cond[3,2]
  slidingwindow$precipwithinquad_SE[i] <- summary(mod)$coefficients$cond[5,2]
  
  #VPD coeffs
  slidingwindow$VPDamong_coef[i] <- summary(mod)$coefficients$cond[2,1]
  slidingwindow$VPDwithin_coef[i] <- summary(mod)$coefficients$cond[4,1]
  slidingwindow$VPDamongquad_coef[i] <- summary(mod)$coefficients$cond[3,1]
  slidingwindow$VPDwithinquad_coef[i] <- summary(mod)$coefficients$cond[5,1]
  #SE
  slidingwindow$VPDamong_SE[i] <- summary(mod)$coefficients$cond[2,2]
  slidingwindow$VPDwithin_SE[i] <- summary(mod)$coefficients$cond[4,2]
  slidingwindow$VPDamongquad_SE[i] <- summary(mod)$coefficients$cond[3,2]
  slidingwindow$VPDwithinquad_SE[i] <- summary(mod)$coefficients$cond[5,2]
  
  #removing data for next iteration
  rm(climate_data, window_df, mod)
}


#plotting sliding window analysis results
plot(NA, xlim=c(min(slidingwindow$start_date),max(slidingwindow$end)),
     ylim=c(min(slidingwindow$deltaAICc),max(slidingwindow$deltaAICc)), 
     xlab="Ordinal date (91 = 1st April)", ylab="deltaAICc") 

for(i in 1:nrow(slidingwindow)){
  points(c(slidingwindow$start_date[i],slidingwindow$end[i]),
         c(slidingwindow$deltaAICc[i],slidingwindow$deltaAICc[i]),type="l") 
} 

abline(h=(min(slidingwindow$deltaAICc)+2), col="red", lty="dashed")   

#finding the window with the lowest AIC and therefore best performance
wind_row <- which(slidingwindow$deltaAICc==min(slidingwindow$deltaAICc))

wind_ID <- slidingwindow$windowID[wind_row] 

wind_ID

103-58

#taking this window for further analysis
#start date is 147, end date is 192
fagus_window <- data.frame(Site_year = meantemp_fagus_sites_wide$Site_year,
                           meantemp = rowMeans(meantemp_fagus_sites_wide[,58:103]),
                           maxtemp = rowMeans(maxtemp_fagus_sites_wide[,58:103]),
                           precip = rowMeans(precip_fagus_sites_wide[,58:103]),
                           VPD = rowMeans(VPD_fagus_sites_wide[,58:103], na.rm=T))
#need to join temperature and tree data here
fagus_window_amongwithin <- left_join(fagus_sylv_dat, fagus_window, by="Site_year")
#remove years where there is no data available (pre-1950)
fagus_window_amongwithin_excl <- fagus_window_amongwithin %>%
  filter(!is.na(maxtemp))%>%
  filter(!is.na(meantemp))%>%
  filter(!is.na(precip))%>%
  filter(!is.na(VPD))

#calculating among/within effects 
fagus_populations <- unique(fagus_sylv_dat$SiteID)

#creating df to put among within effects in 
fagus_df <- data.frame(matrix(NA, ncol=32, nrow=0))
colnames(fagus_df) <- c("SiteID", "TreeID", "Country",         
                         "Species","Site", "Tree",              
                         "Year", "Ring.width", "Elevation",     
                         "Latitude", "Centred_year", "Site_year",      
                         "window_MeanTemp", "AmongMeanTemp", "WithinMeanTemp",    
                         "WithinMeanTempQuad4", "AmongMeanTempQuad", 
                         "window_maxtemp", "AmongMaxTemp", "WithinMaxTemp",    
                         "WithinMaxTempQuad4", "AmongMaxTempQuad",
                         "window_precip", "AmongPrecip", "WithinPrecip",    
                         "WithinPrecipQuad4", "AmongPrecipQuad",
                         "window_VPD", "AmongVPD", "WithinVPD",    
                         "WithinVPDQuad4", "AmongVPDQuad")

for (n in 1:length(fagus_populations)){
  subset <- fagus_window_amongwithin_excl %>% 
    filter(SiteID == fagus_populations[n])
  
  #max temp
  subset$AmongMaxTemp <- mean(subset$maxtemp)#among is the mean of each site
  subset$WithinMaxTemp <- subset$maxtemp-(mean(subset$maxtemp))#within is the deviation of each observation from the mean of each site
  subset$WithinMaxTempQuad4 <- (subset$maxtemp-(mean(subset$maxtemp)))^2 #quadratic calculated using equation 4 from Fay et al
  #equation 4 assumes that the effect of temperature varies among populations (which would be the case if locally adapted)
  subset$AmongMaxTempQuad <- mean(subset$maxtemp)^2
  
  #
  #mean temp
  subset$AmongMeanTemp <- mean(subset$meantemp)#among is the mean of each site
  subset$WithinMeanTemp <- subset$meantemp-(mean(subset$meantemp))#within is the deviation of each observation from the mean of each site
  subset$WithinMeanTempQuad4 <- (subset$meantemp-(mean(subset$meantemp)))^2 #quadratic calculated using equation 4 from Fay et al
  #equation 4 assumes that the effect of temperature varies among populations (which would be the case if locally adapted)
  subset$AmongMeanTempQuad <- mean(subset$meantemp)^2
  
  #
  #Precip
  subset$AmongPrecip <- mean(subset$precip)#among is the mean of each site
  subset$WithinPrecip <- subset$precip-(mean(subset$precip))#within is the deviation of each observation from the mean of each site
  subset$WithinPrecipQuad4 <- (subset$precip-(mean(subset$precip)))^2 #quadratic calculated using equation 4 from Fay et al
  #equation 4 assumes that the effect of temperature varies among populations (which would be the case if locally adapted)
  subset$AmongPrecipQuad <- mean(subset$precip)^2
  
  #
  #VPD
  subset$AmongVPD <- mean(subset$VPD)#among is the mean of each site
  subset$WithinVPD <- subset$VPD-(mean(subset$VPD))#within is the deviation of each observation from the mean of each site
  subset$WithinVPDQuad4 <- (subset$VPD-(mean(subset$VPD)))^2 #quadratic calculated using equation 4 from Fay et al
  #equation 4 assumes that the effect of temperature varies among populations (which would be the case if locally adapted)
  subset$AmongVPDQuad <- mean(subset$VPD)^2
  
  #adding to sliding window dataframe
  fagus_df <- rbind(fagus_df, subset)
}

#calculating among deviation (deviation from global average)
fagus_df$AmongMaxTempDev<- fagus_df$AmongMaxTemp - mean(fagus_df$maxtemp)
fagus_df$AmongMeanTempDev<- fagus_df$AmongMeanTemp - mean(fagus_df$meantemp)
fagus_df$AmongPrecipDev<- fagus_df$AmongPrecip - mean(fagus_df$precip)
fagus_df$AmongVPDDev<- fagus_df$AmongVPD - mean(fagus_df$VPD)
#calculating quadratic (quadratic deviations from global average)
fagus_df$AmongMaxTempDevQuad<- fagus_df$AmongMaxTempDev^2
fagus_df$AmongMeanTempDevQuad<- fagus_df$AmongMeanTempDev^2
fagus_df$AmongPrecipDevQuad<- fagus_df$AmongPrecipDev^2
fagus_df$AmongVPDDevQuad<- fagus_df$AmongVPDDev^2
#calculating quadratic of original climate data for global optimum model
fagus_df$Avg_MaxTempQuad<- fagus_df$maxtemp^2
fagus_df$Avg_MeanTempQuad<- fagus_df$meantemp^2
fagus_df$Avg_PrecipQuad<- fagus_df$precip^2
fagus_df$Avg_VPDQuad<- fagus_df$VPD^2

#making sure Year is categorical not numerical
fagus_df$Year <- as.character(fagus_df$Year)

#running model here 
fagus_all_climate <- glmmTMB(log1p(Ring.width)~AmongMaxTempDev+AmongMeanTempDev+AmongPrecipDev+AmongVPDDev+
                 AmongMaxTempDevQuad+AmongMeanTempDevQuad+AmongPrecipDevQuad+AmongVPDDevQuad+
                 WithinMaxTemp+WithinMeanTemp+WithinPrecip+WithinVPD+
                 WithinMaxTempQuad4+WithinMeanTempQuad4+WithinPrecipQuad4+WithinVPDQuad4+
                 (AmongMaxTempDev*WithinMaxTemp)+
                 (AmongMeanTempDev*WithinMeanTemp)+
                 (AmongPrecipDev*WithinPrecip)+
                 (AmongVPDDev*WithinVPD)+
                 Latitude+
                 (1|Year)+(1|SiteID)+
                 ns(Centred_year, df=3)+
                 (1+ns(Centred_year, df=3)|TreeID)+
                 (1|Centred_year:SiteID),
               data=fagus_df)

summary(fagus_all_climate)
#significant - within max temp *, within mean temp **, within precip ***, within VPD ***, precip within quad *

#need to do again removing Spain sites because forgot to do so

#plotting figure to demonstrate 
###among quad fagus
#####
#skeleton dataset for prediction of VPD
#fixed continuous variables at median, year at 2000, tree ID at first tree ID in dataset, site ID at first site ID in dataset
#apart from the among deviation and among quad values so the only thing that the only thing that varies 
#is the among effect to show spatial relationship 
med_year <- median(fagus_df$Centred_year)
med_yr <- 2000
med_withinVPD <- median(fagus_df$WithinVPD)
med_withinVPDQuad <- med_withinVPD^2
MinAmongVPDDev <- min(fagus_df$AmongVPDDev)
MaxAmongVPDDev <- max(fagus_df$AmongVPDDev)
pred_amongVPDDev <- seq(MinAmongVPDDev, MaxAmongVPDDev+0.1, 0.1)#+0.1 to ensure that is capturing all data
pred_amongVPDDevQuad <- pred_amongVPDDev^2
tree_id <- as.character((fagus_df$TreeID[1])) 
site_id <- as.character((fagus_df$SiteID[1]))
med_lat <- fagus_df$Latitude[1]
med_amongMeanTempDev <- median(fagus_df$AmongMeanTempDev)
med_amongMeanTempDevQuad <- med_amongMeanTempDev^2
med_amongMaxTempDev <- median(fagus_df$AmongMaxTempDev)
med_amongMaxTempDevQuad <- med_amongMaxTempDev^2
med_amongPrecipDev <- median(fagus_df$AmongPrecipDev)
med_amongPrecipDevQuad <- med_amongPrecipDev^2
med_withinMeanTemp <- median(fagus_df$WithinMeanTemp)
med_withinMeanTempQuad <- med_withinMeanTemp^2
med_withinMaxTemp <- median(fagus_df$WithinMaxTemp)
med_withinMaxTempQuad <- med_withinMaxTemp^2
med_withinPrecip <- median(fagus_df$WithinPrecip)
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
                              AmongMeanTempDev=rep(med_amongMeanTempDev, length(pred_amongVPDDev)),
                              AmongMeanTempDevQuad=rep(med_amongMeanTempDevQuad, length(pred_amongVPDDev)),
                              WithinMeanTemp=rep(med_withinMeanTemp, length(pred_amongVPDDev)),
                              WithinMeanTempQuad4=rep(med_withinMeanTempQuad, length(pred_amongVPDDev)),
                              AmongMaxTempDev=rep(med_amongMaxTempDev, length(pred_amongVPDDev)),
                              AmongMaxTempDevQuad=rep(med_amongMaxTempDevQuad, length(pred_amongVPDDev)),
                              WithinMaxTemp=rep(med_withinMaxTemp, length(pred_amongVPDDev)),
                              WithinMaxTempQuad4=rep(med_withinMaxTempQuad, length(pred_amongVPDDev)))

#for prediction of MeanTemperature
med_year <- median(fagus_df$Centred_year)
med_yr <- 2000
med_withinMeanTemp <- median(fagus_df$WithinMeanTemp)
med_withinMeanTempQuad <- med_withinMeanTemp^2
MinAmongMeanTempDev <- min(fagus_df$AmongMeanTempDev)
MaxAmongMeanTempDev <- max(fagus_df$AmongMeanTempDev)
pred_amongMeanTempDev <- seq(MinAmongMeanTempDev, MaxAmongMeanTempDev+1, 0.5)
pred_amongMeanTempDevQuad <- pred_amongMeanTempDev^2
tree_id <- as.character((fagus_df$TreeID[1])) 
site_id <- as.character((fagus_df$SiteID[1]))
med_lat <- fagus_df$Latitude[1]
med_amongVPDDev <- median(fagus_df$AmongVPDDev)
med_amongVPDDevQuad <- med_amongVPDDev^2
med_amongDevPrecip <- median(fagus_df$AmongPrecipDev)
med_amongPrecipDevQuad <- med_amongPrecipDev^2
med_withinVPD <- median(fagus_df$WithinVPD)
med_withinVPDquad <- med_withinVPD^2
med_withinPrecip <- median(fagus_df$WithinPrecip)
med_withinPrecipQuad <- med_withinPrecip^2
med_amongMaxTempDev <- median(fagus_df$AmongMaxTempDev)
med_amongMaxTempDevQuad <- med_amongMaxTempDev^2
med_withinMaxTemp <- median(fagus_df$WithinMaxTemp)
med_withinMaxTempQuad <- med_withinMaxTemp^2

newdataAmongMeanTemp <- data.frame(Year=rep(med_yr, length(pred_amongMeanTempDev)),
                               Centred_year=rep(med_year, length(pred_amongMeanTempDev)),
                               Latitude=rep(med_lat, length(pred_amongMeanTempDev)),
                               WithinMeanTemp=rep(med_withinMeanTemp, length(pred_amongMeanTempDev)),
                               WithinMeanTempQuad4=rep(med_withinMeanTempQuad, length(pred_amongMeanTempDev)),
                               TreeID=rep(tree_id, length(pred_amongMeanTempDev)),
                               SiteID=rep(site_id, length(pred_amongMeanTempDev)),
                               AmongMeanTempDev=pred_amongMeanTempDev,
                               AmongMeanTempDevQuad = pred_amongMeanTempDevQuad,
                               AmongPrecipDev=rep(med_amongPrecipDev, length(pred_amongMeanTempDev)),
                               AmongPrecipDevQuad=rep(med_amongPrecipDevQuad, length(pred_amongMeanTempDev)),
                               WithinPrecip=rep(med_withinPrecip, length(pred_amongMeanTempDev)),
                               WithinPrecipQuad4=rep(med_withinPrecipQuad, length(pred_amongMeanTempDev)),
                               AmongVPDDev=rep(med_amongVPDDev, length(pred_amongMeanTempDev)),
                               AmongVPDDevQuad=rep(med_amongVPDDevQuad, length(pred_amongMeanTempDev)),
                               WithinVPD=rep(med_withinVPD, length(pred_amongMeanTempDev)),
                               WithinVPDQuad4=rep(med_withinVPDQuad, length(pred_amongMeanTempDev)),
                              AmongMaxTempDev=rep(med_amongMaxTempDev, length(pred_amongMeanTempDev)),
                              AmongMaxTempDevQuad=rep(med_amongMaxTempDevQuad, length(pred_amongMeanTempDev)),
                              WithinMaxTemp=rep(med_withinMaxTemp, length(pred_amongMeanTempDev)),
                              WithinMaxTempQuad4=rep(med_withinMaxTempQuad, length(pred_amongMeanTempDev)))

#for prediction for precipitation
med_year <- median(fagus_df$Centred_year)
med_yr <- 2000
med_withinPrecip <- median(fagus_df$WithinPrecip)
med_withinPrecipQuad <- med_withinPrecip^2
MinAmongPrecipDev <- min(fagus_df$AmongPrecipDev)
MaxAmongPrecipDev <- max(fagus_df$AmongPrecipDev)
pred_amongPrecipDev <- seq(MinAmongPrecipDev, MaxAmongPrecipDev+0.2, 0.2)
pred_amongPrecipDevQuad <- pred_amongPrecipDev^2
tree_id <- as.character((fagus_df$TreeID[1])) 
site_id <- as.character((fagus_df$SiteID[1]))
med_lat <- fagus_df$Latitude[1]
med_amongVPDDev <- median(fagus_df$AmongVPDDev)
med_amongVPDDevQuad <- med_amongVPDDev^2
med_amongMeanTempDev <- median(fagus_df$AmongMeanTempDev)
med_amongMeanTempDevQuad <- med_amongMeanTempDev^2
med_withinVPD <- median(fagus_df$WithinVPD)
med_withinVPDQuad <- med_withinVPD^2
med_withinMeanTemp <- median(fagus_df$WithinMeanTemp)
med_withinMeanTempQuad <- med_withinMeanTemp^2
med_amongMaxTempDev <- median(fagus_df$AmongMaxTempDev)
med_amongMaxTempDevQuad <- med_amongMaxTempDev^2
med_withinMaxTemp <- median(fagus_df$WithinMaxTemp)
med_withinMaxTempQuad <- med_withinMaxTemp^2


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
                                 AmongMeanTempDev=rep(med_amongMeanTempDev, length(pred_amongPrecipDev)),
                                 AmongMeanTempDevQuad=rep(med_amongMeanTempDevQuad, length(pred_amongPrecipDev)),
                                 WithinMeanTemp=rep(med_withinMeanTemp, length(pred_amongPrecipDev)),
                                 WithinMeanTempQuad4=rep(med_withinMeanTempQuad, length(pred_amongPrecipDev)),
                                 AmongMaxTempDev=rep(med_amongMaxTempDev, length(pred_amongPrecipDev)),
                                 AmongMaxTempDevQuad=rep(med_amongMaxTempDevQuad, length(pred_amongPrecipDev)),
                                 WithinMaxTemp=rep(med_withinMaxTemp, length(pred_amongPrecipDev)),
                                 WithinMaxTempQuad4=rep(med_withinMaxTempQuad, length(pred_amongPrecipDev)))

#for prediction of Max Temperature
med_year <- median(fagus_df$Centred_year)
med_yr <- 2000
med_withinMaxTemp <- median(fagus_df$WithinMaxTemp)
med_withinMaxTempQuad <- med_withinMaxTemp^2
MinAmongMaxTempDev <- min(fagus_df$AmongMaxTempDev)
MaxAmongMaxTempDev <- max(fagus_df$AmongMaxTempDev)
pred_amongMaxTempDev <- seq(MinAmongMaxTempDev, MaxAmongMaxTempDev+1, 0.5)
pred_amongMaxTempDevQuad <- pred_amongMaxTempDev^2
tree_id <- as.character((fagus_df$TreeID[1])) 
site_id <- as.character((fagus_df$SiteID[1]))
med_lat <- fagus_df$Latitude[1]
med_amongVPDDev <- median(fagus_df$AmongVPDDev)
med_amongVPDDevQuad <- med_amongVPDDev^2
med_amongDevPrecip <- median(fagus_df$AmongPrecipDev)
med_amongPrecipDevQuad <- med_amongPrecipDev^2
med_withinVPD <- median(fagus_df$WithinVPD)
med_withinVPDquad <- med_withinVPD^2
med_withinPrecip <- median(fagus_df$WithinPrecip)
med_withinPrecipQuad <- med_withinPrecip^2
med_amongMeanTempDev <- median(fagus_df$AmongMeanTempDev)
med_amongMeanTempDevQuad <- med_amongMeanTempDev^2
med_withinMeanTemp <- median(fagus_df$WithinMeanTemp)
med_withinMeanTempQuad <- med_withinMeanTemp^2

newdataAmongMaxTemp <- data.frame(Year=rep(med_yr, length(pred_amongMaxTempDev)),
                                   Centred_year=rep(med_year, length(pred_amongMaxTempDev)),
                                   Latitude=rep(med_lat, length(pred_amongMaxTempDev)),
                                   WithinMaxTemp=rep(med_withinMaxTemp, length(pred_amongMaxTempDev)),
                                   WithinMaxTempQuad4=rep(med_withinMaxTempQuad, length(pred_amongMaxTempDev)),
                                   TreeID=rep(tree_id, length(pred_amongMaxTempDev)),
                                   SiteID=rep(site_id, length(pred_amongMaxTempDev)),
                                   AmongMaxTempDev=pred_amongMaxTempDev,
                                   AmongMaxTempDevQuad = pred_amongMaxTempDevQuad,
                                   AmongPrecipDev=rep(med_amongPrecipDev, length(pred_amongMaxTempDev)),
                                   AmongPrecipDevQuad=rep(med_amongPrecipDevQuad, length(pred_amongMaxTempDev)),
                                   WithinPrecip=rep(med_withinPrecip, length(pred_amongMaxTempDev)),
                                   WithinPrecipQuad4=rep(med_withinPrecipQuad, length(pred_amongMaxTempDev)),
                                   AmongVPDDev=rep(med_amongVPDDev, length(pred_amongMaxTempDev)),
                                   AmongVPDDevQuad=rep(med_amongVPDDevQuad, length(pred_amongMaxTempDev)),
                                   WithinVPD=rep(med_withinVPD, length(pred_amongMaxTempDev)),
                                   WithinVPDQuad4=rep(med_withinVPDQuad, length(pred_amongMaxTempDev)),
                                   AmongMeanTempDev=rep(med_amongMeanTempDev, length(pred_amongMaxTempDev)),
                                   AmongMeanTempDevQuad=rep(med_amongMeanTempDevQuad, length(pred_amongMaxTempDev)),
                                   WithinMeanTemp=rep(med_withinMeanTemp, length(pred_amongMaxTempDev)),
                                   WithinMeanTempQuad4=rep(med_withinMeanTempQuad, length(pred_amongMaxTempDev)))

#predicting expected tree ring widths from the spatial relationship prediction dataframes created above
#using the fagus_all_climate 
VPD_pred_among_fagus <-predict(fagus_all_climate, newdata = newdataAmongVPD, re.form=NULL, se.fit=TRUE)
MeanTemp_pred_among_fagus <-predict(fagus_all_climate, newdata = newdataAmongMeanTemp, re.form=NULL, se.fit=TRUE)
precip_pred_among_fagus <-predict(fagus_all_climate, newdata = newdataAmongPrecip, re.form=NULL, se.fit=TRUE)
MaxTemp_pred_among_fagus <-predict(fagus_all_climate, newdata = newdataAmongMaxTemp, re.form=NULL, se.fit=TRUE)

#adding predicted points to prediction dataframe and standard error for calculation of confidence intervals
newdataAmongVPD$Prediction <- as.numeric(VPD_pred_among_fagus$fit)
newdataAmongVPD$SE <- as.numeric(VPD_pred_among_fagus$se.fit)
newdataAmongMeanTemp$Prediction  <- as.numeric(MeanTemp_pred_among_fagus$fit)
newdataAmongMeanTemp$SE <- as.numeric(MeanTemp_pred_among_fagus$se.fit)
newdataAmongPrecip$Prediction  <- as.numeric(precip_pred_among_fagus$fit)
newdataAmongPrecip$SE <- as.numeric(precip_pred_among_fagus$se.fit)
newdataAmongMaxTemp$Prediction  <- as.numeric(MaxTemp_pred_among_fagus$fit)
newdataAmongMaxTemp$SE <- as.numeric(MaxTemp_pred_among_fagus$se.fit)

#calculating 95% confidence intervals of among slope
newdataAmongVPD$LCI <- newdataAmongVPD$Prediction-1.96*newdataAmongVPD$SE
newdataAmongVPD$HCI <- newdataAmongVPD$Prediction+1.96*newdataAmongVPD$SE
newdataAmongMeanTemp$LCI <- newdataAmongMeanTemp$Prediction-1.96*newdataAmongMeanTemp$SE
newdataAmongMeanTemp$HCI <- newdataAmongMeanTemp$Prediction+1.96*newdataAmongMeanTemp$SE
newdataAmongPrecip$LCI <- newdataAmongPrecip$Prediction-1.96*newdataAmongPrecip$SE
newdataAmongPrecip$HCI <- newdataAmongPrecip$Prediction+1.96*newdataAmongPrecip$SE
newdataAmongMaxTemp$LCI <- newdataAmongMaxTemp$Prediction-1.96*newdataAmongMaxTemp$SE
newdataAmongMaxTemp$HCI <- newdataAmongMaxTemp$Prediction+1.96*newdataAmongMaxTemp$SE

#calculating predictions for temporal/local/within slopes
######

#within slopes
#fixing among at specific values to create separate curves along the global gradient
#for mean MeanTemperature
amongmeanMeanTemps <- seq(MinAmongMeanTempDev, MaxAmongMeanTempDev, 2) 
within_points_df <- list()

#creating new dataframe where among is now fixed but within varies along a sequence ranging from
#the minimum to maximum deviation within the dataset
for(i in 1:length(amongmeanMeanTemps)){
  med_amongMeanTempDev <- amongmeanMeanTemps[i]
  med_amongMeanTempDevQuad <- med_amongMeanTempDev^2
  
  MinWithinMeanTemp <- min(fagus_df$WithinMeanTemp)
  MaxWithinMeanTemp <- max(fagus_df$WithinMeanTemp)
  pred_withinMeanTemp <- seq(MinWithinMeanTemp, MaxWithinMeanTemp, 0.5)#sequence of values for MeanTemporal effect
  pred_withinMeanTempQuad <- pred_withinMeanTemp^2
  
  newdatasubset <- data.frame(Year=rep(med_yr, length(pred_withinMeanTemp)),
                              Centred_year=rep(med_year, length(pred_withinMeanTemp)),
                              Latitude=rep(med_lat, length(pred_withinMeanTemp)),
                              WithinMeanTemp=pred_withinMeanTemp,
                              WithinMeanTempQuad4=pred_withinMeanTempQuad,
                              TreeID=tree_id,
                              SiteID=site_id,
                              AmongMeanTempDev=rep(med_amongMeanTempDev, length(pred_withinMeanTemp)),
                              AmongMeanTempDevQuad=rep(med_amongMeanTempDevQuad, length(pred_withinMeanTemp)),
                              AmongPrecipDev=rep(med_amongPrecipDev, length(pred_withinMeanTemp)),
                              AmongPrecipDevQuad=rep(med_amongPrecipDevQuad, length(pred_withinMeanTemp)),
                              WithinPrecip=rep(med_withinPrecip, length(pred_withinMeanTemp)),
                              WithinPrecipQuad4=rep(med_withinPrecipQuad, length(pred_withinMeanTemp)),
                              AmongVPDDev=rep(med_amongVPDDev, length(pred_withinMeanTemp)),
                              AmongVPDDevQuad=rep(med_amongVPDDevQuad, length(pred_withinMeanTemp)),
                              WithinVPD=rep(med_withinVPD, length(pred_withinMeanTemp)),
                              WithinVPDQuad4=rep(med_withinVPDQuad, length(pred_withinMeanTemp)),
                              AmongMaxTempDev=rep(med_amongMaxTempDev, length(pred_withinMeanTemp)),
                              AmongMaxTempDevQuad=rep(med_amongMaxTempDevQuad, length(pred_withinMeanTemp)),
                              WithinMaxTemp=rep(med_withinMaxTemp, length(pred_withinMeanTemp)),
                              WithinMaxTempQuad4=rep(med_withinMaxTempQuad, length(pred_withinMeanTemp)),
                              AvgAmongMeanTemp = rep(unique(as.numeric(fagus_df$AvgAmongMeanTemp)), length(pred_withinMeanTemp)))
  #calculating deviations away from  global average for each within deviation
  newdatasubset$ActualAmongMeanTemp <- newdatasubset$AvgAmongMeanTemp+newdatasubset$AmongMeanTempDev
  newdatasubset$ActualMeanTemp <- newdatasubset$ActualAmongMeanTemp+newdatasubset$WithinMeanTemp
  newdatasubset$WithinGlobDev <- newdatasubset$ActualMeanTemp-newdatasubset$AvgAmongMeanTemp
  
  MeanTemp_pred_subset <-predict(fagus_all_climate_fagus, newdata = newdatasubset, re.form=NULL, se.fit=TRUE)
  MeanTemp_pred_subset_vals <- MeanTemp_pred_subset$fit
  MeanTemp_pred_subset_HCI <- MeanTemp_pred_subset_vals+1.96*MeanTemp_pred_subset$se.fit
  MeanTemp_pred_subset_LCI <- MeanTemp_pred_subset_vals-1.96*MeanTemp_pred_subset$se.fit
  within_points_df[[i]] <- data.frame(Prediction=MeanTemp_pred_subset_vals, 
                                      AmongMeanTempDev=newdatasubset$AmongMeanTempDev,
                                      WithinMeanTemp=newdatasubset$WithinMeanTemp,
                                      WithinGlobDev=newdatasubset$WithinGlobDev,
                                      HCI=MeanTemp_pred_subset_HCI,
                                      LCI=MeanTemp_pred_subset_LCI)
}  

#moving dataframes from loop list into dataframe to be used to calculate curves from
within_curves1 <- within_points_df[[1]]
within_curves2 <- within_points_df[[2]]
within_curves3 <- within_points_df[[3]]
within_curves4 <- within_points_df[[4]]

#creating figure with a curve for among/spatial, and four curves for within/MeanTemporal
library(ggplot2)
library(cowplot)
fagus_MeanTemp <- ggplot(data=fagus_df, aes(x=AmongMeanTemp, y=log1p(Ring.width)))+
  geom_line(data = data.frame(AmongMeanTempDev = newdataAmongMeanTemp$AmongMeanTempDev, Ring.width = newdataAmongMeanTemp$Prediction), aes(x = AmongMeanTempDev, y = Ring.width), colour = "blue", linetype="dashed")+#spatial curve
  geom_line(data = data.frame(WithinGlobDev = within_curves1$WithinGlobDev, Ring.width = within_curves1$Prediction), aes(x = WithinGlobDev, y = Ring.width), colour = "red", linetype="dashed")+#MeanTemporal curve
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
  xlab("Deviation from global average MeanTemperature (°C)")+
  ylab("Ring width (log+1)")+
  ggtitle("Fagus sylvatica")+
  theme(axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12), 
        title=element_text(size=12))+
  theme(plot.title = element_text(face = "italic"))+
  ylim(4, 8)+ #keeping axis consistent for figure containing all species
  xlim(-8, 6)

fagus_MeanTemp
#for VPD
amongVPDs <- seq(MinAmongVPDDev, MaxAmongVPDDev, 0.2) 
within_points_df <- list()

library(splines)

for(i in 1:length(amongVPDs)){
  med_amongVPDDev <- amongVPDs[i]
  med_amongVPDDevQuad <- med_amongVPDDev^2
  
  MinWithinVPD <- min(fagus_df$WithinVPD)
  MaxWithinVPD <- max(fagus_df$WithinVPD)
  pred_withinVPD <- seq(MinWithinVPD, MaxWithinVPD, 0.025)
  pred_withinVPDQuad <- pred_withinVPD^2
  
  med_withinMeanTemp <- median(fagus_df$WithinMeanTemp)
  med_withinMeanTempQuad <- med_withinMeanTemp^2
  med_amongMeanTempDev <- median(fagus_df$AmongMeanTempDev)
  med_amongMeanTempDevQuad <- med_amongMeanTempDev^2
  
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
                              AmongMeanTempDev=rep(med_amongMeanTempDev, length(pred_withinVPD)),
                              AmongMeanTempDevQuad=rep(med_amongMeanTempDevQuad, length(pred_withinVPD)),
                              WithinMeanTemp=rep(med_withinVPD, length(pred_withinVPD)),
                              WithinMeanTempQuad4=rep(med_withinVPDQuad, length(pred_withinVPD)),
                              AmongMaxTempDev=rep(med_amongMaxTempDev, length(pred_withinVPD)),
                              AmongMaxTempDevQuad=rep(med_amongMaxTempDevQuad, length(pred_withinVPD)),
                              WithinMaxTemp=rep(med_withinVPD, length(pred_withinVPD)),
                              WithinMaxTempQuad4=rep(med_withinVPDQuad, length(pred_withinVPD)),
                              AvgAmongVPD = rep(unique(as.numeric(fagus_df$AvgAmongVPD)), length(pred_withinVPD)))
  
  newdatasubset$ActualAmongVPD <- newdatasubset$AvgAmongVPD+newdatasubset$AmongVPDDev
  newdatasubset$ActualVPD <- newdatasubset$ActualAmongVPD+newdatasubset$WithinVPD
  newdatasubset$WithinGlobDev <- newdatasubset$ActualVPD-newdatasubset$AvgAmongVPD
  
  VPD_pred_subset <-predict(fagus_all_climate, newdata = newdatasubset, re.form=NULL, se.fit=TRUE)
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

#creating figure with a curve for among/spatial, and four curves for within/MeanTemporal
library(ggplot2)
library(cowplot)
fagus_VPD <- ggplot(data=fagus_df, aes(x=AmongVPD, y=log1p(Ring.width)))+
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
  med_year <- median(fagus_df$Centred_year)
  med_yr <- 2000
  med_amongPrecipDev <- amongPrecips[i]
  med_amongPrecipDevQuad <- med_amongPrecipDev^2
  tree_id <- as.character((fagus_df$TreeID[1]))
  site_id <- as.character((fagus_df$SiteID[1]))
  med_lat <- fagus_df$Latitude[1]
  
  MinWithinPrecip <- min(fagus_df$WithinPrecip)
  MaxWithinPrecip <- max(fagus_df$WithinPrecip)
  pred_withinPrecip <- seq(MinWithinPrecip, MaxWithinPrecip, 0.25)
  pred_withinPrecipQuad <- pred_withinPrecip^2
  
  med_withinVPD <- median(fagus_df$WithinVPD)
  med_withinVPDquad <- med_withinVPD^2
  med_amongVPDDev <- median(fagus_df$AmongVPDDev)
  med_amongVPDDevQuad <- med_amongVPDDev^2
  
  med_withinMeanTemp <- median(fagus_df$WithinMeanTemp)
  med_withinMeanTempQuad <- med_withinMeanTemp^2
  med_amongMeanTempDev <- median(fagus_df$AmongMeanTempDev)
  med_amongMeanTempDevQuad <- med_amongMeanTempDev^2
  
  newdatasubset <- data.frame(Year=rep(med_yr, length(pred_withinPrecip)),
                              Centred_year=rep(med_year, length(pred_withinPrecip)),
                              Latitude=rep(med_lat, length(pred_withinPrecip)),
                              WithinPrecip=pred_withinPrecip,
                              WithinPrecipQuad4=pred_withinPrecipQuad,
                              AmongPrecipDev=rep(med_amongPrecipDev, length(pred_withinPrecip)),
                              AmongPrecipDevQuad=rep(med_amongPrecipDevQuad, length(pred_withinPrecip)),
                              TreeID=tree_id,
                              SiteID=site_id,
                              AmongMeanTempDev=rep(med_amongMeanTempDev, length(pred_withinPrecip)),
                              AmongMeanTempDevQuad=rep(med_amongMeanTempDevQuad, length(pred_withinPrecip)),
                              AmongVPDDev=rep(med_amongVPDDev, length(pred_withinPrecip)),
                              AmongVPDDevQuad=rep(med_amongVPDDevQuad, length(pred_withinPrecip)),
                              WithinVPD=rep(med_withinVPD, length(pred_withinPrecip)),
                              WithinVPDQuad4=rep(med_withinVPDQuad, length(pred_withinPrecip)),
                              WithinMeanTemp=rep(med_withinMeanTemp, length(pred_withinPrecip)),
                              WithinMeanTempQuad4=rep(med_withinMeanTempQuad, length(pred_withinPrecip)),
                              AmongMaxTempDev=rep(med_amongMaxTempDev, length(pred_withinPrecip)),
                              AmongMaxTempDevQuad=rep(med_amongMaxTempDevQuad, length(pred_withinPrecip)),
                              WithinMaxTemp=rep(med_withinMaxTemp, length(pred_withinPrecip)),
                              WithinMaxTempQuad4=rep(med_withinMaxTempQuad, length(pred_withinPrecip)),
                              AvgAmongPrecip = rep(unique(as.numeric(fagus_df$AvgAmongPrecip)), length(pred_withinPrecip)))
  
  newdatasubset$ActualAmongPrecip <- newdatasubset$AvgAmongPrecip+newdatasubset$AmongPrecipDev
  newdatasubset$ActualPrecip <- newdatasubset$ActualAmongPrecip+newdatasubset$WithinPrecip
  newdatasubset$WithinGlobDev <- newdatasubset$ActualPrecip-newdatasubset$AvgAmongPrecip
  
  Precip_pred_subset <-predict(fagus_all_climate, newdata = newdatasubset, re.form=NULL, se.fit=TRUE)
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
fagus_Precip <- ggplot(data=fagus_df, aes(x=AmongPrecip, y=log1p(Ring.width)))+
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