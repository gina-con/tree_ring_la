#loading in the GenTree collection data 
#####
#downloaded from: https://springernature.figshare.com/collections/The_GenTree_Dendroecological_Collection_tree-ring_and_wood_density_data_from_seven_tree_species_across_Europe/4561037
#this dataset was posted on 2019-12-19
tree_ring_data <- read.csv("data/gentree_data/trw_long_format.csv")
site_data <- read.csv("data/gentree_data/site.csv")
tree_data <- read.csv("data/gentree_data/tree.csv")

#exploring data 
head(site_data)
#contains metadata on each site 
head(tree_data)
#contains tree, lat and long, length, pith, age, dbh, height, density
#metadata surrounding tree 
head(tree_ring_data)
unique(tree_ring_data$Species)
#contains tree, year and ring width
#####

#extracting elevation data and latitude data from site dataframe and adding to tree ring dataframe
#####
library(dplyr)
elevdata <- data.frame(site_data$SiteID, site_data$Elevation)
colnames(elevdata) <- c("SiteID", "Elevation")
compl_data <- left_join(tree_ring_data, elevdata, "SiteID")

latdata <- data.frame(site_data$SiteID, site_data$Latitude)
colnames(latdata) <- c("SiteID", "Latitude")
compl_data <- left_join(compl_data, latdata, "SiteID")

#####

#subsetting fagus sylvatica data
#####
library(dplyr)
fagus_sylv_dat <- compl_data %>% 
  filter(Species %in% "Fagus sylvatica")
#####

#calculating age 
#####
tree_id_list_2 <- c()
n <- 1
tree_id_list_2 <- unique(fagus_sylv_dat$TreeID)

fagus_sylv_age_df <- data.frame()

#creating an age variable
#This is sometimes a few years out from the actual age listed in the dataset...
for (n in 1:length(tree_id_list_2)){
  subset <- fagus_sylv_dat %>% 
    filter(TreeID == tree_id_list_2[n])
  subset$Age <- 1:nrow(subset)
  fagus_sylv_age_df <- rbind(fagus_sylv_age_df, subset)
}

#####

#climate data wrangling 
######---
#downloaded from Copernicus Climate Data Store on 03/05/2026 at 11:00 
#ensemble mean, maximum temperature, across whole time period
#downloaded from Copernicus Climate Data Store on 19/05/2026 at 14:00
#ensemble mean, rel humidity, across whole time period
#downloaded from Copernicus Climate Data Store on 26/05/2026
#ensemble mean for precipitation amount 
library(sp)
library(raster)
library(ncdf4) 
library(here)
library(ggplot2)

#loading in E-obs maxtemp data
maxtemp <- brick("Data/e_obs/climate_raw/maxtempdata.nc", varname="tx")
meantemp <- brick("Data/e_obs/climate_raw/meantempdata.nc", varname="tg")

#relhum and precip info is incomplete, many NAs, have replaced with coordinates of 
#nearest trees 
relhum <- brick("Data/e_obs/climate_raw/relhumdata.nc", varname="hu")
precip <- brick("Data/e_obs/climate_raw/precipdata.nc", varname="rr")

##climate data for fagus sylvatica, VPD, mean and max temp; precipitation will be added later
#####
#subsetting fagus sylvatica sites
library(dplyr)
fagus_sites_clim <- site_data %>% 
  filter(Species == "Fagus sylvatica")
fagus_sites_latlon <- fagus_sites_clim[,c(1,6,5)]

#creating a dataframe with the coordinates in 
write.csv(fagus_sites_latlon, "Data/e_obs/fagus_sites_coords.csv")

#reading the coordinates in 
fagus_coords <- read.csv("Data/e_obs/fagus_sites_coords.csv", header=TRUE, row.names="SiteID", sep=",")
fagus_coords <- fagus_coords[,-1]

#creating example map to visualise where sites are 
plot(relhum$X2024.06.01)
points(fagus_coords,pch=16,col="red")

#extracting max temperature for each site 
#returns max temperature of every day from 1950-2024
maxtemp_fagus_site <- data.frame(extract(maxtemp, fagus_coords, ncol=2))
row.names(maxtemp_fagus_site) <- row.names(fagus_coords)
write.csv(maxtemp_fagus_site, 'Data/e_obs/maxtempsitedata.csv')

#for mean temp
meantemp_fagus_site <- data.frame(extract(meantemp, fagus_coords, ncol=2))
row.names(meantemp_fagus_site) <- row.names(fagus_coords)
write.csv(meantemp_fagus_site, 'Data/e_obs/meantempsitedata.csv')

#for relative humidity
relhum_fagus_site <- data.frame(extract(relhum, fagus_coords, ncol=2))
row.names(relhum_fagus_site) <- row.names(fagus_coords)
write.csv(relhum_fagus_site, 'Data/e_obs/relhumsitedata.csv')

#for precip
precip_fagus_site <- data.frame(extract(precip, fagus_coords, ncol=2))
row.names(precip_fagus_site) <- row.names(fagus_coords)
write.csv(precip_fagus_site, 'Data/e_obs/precip.csv')

#reading in the csvs with the climate data in
maxtemp_fagus_sites <- read.csv("Data/e_obs/maxtempsitedata.csv", header=TRUE, sep=",", check.names=FALSE)
meantemp_fagus_sites <- read.csv("Data/e_obs/meantempsitedata.csv", header=TRUE, sep=",", check.names=FALSE)
relhum_fagus_sites <- read.csv("Data/e_obs/relhumsitedata.csv", header=TRUE, sep=",", check.names=FALSE)
precip_fagus_sites <- read.csv("Data/e_obs/precip.csv", header=TRUE, sep=",", check.names=FALSE)

#renaming the column with site IDs in to SiteID 
colnames(maxtemp_fagus_sites)[1] = 'SiteID'
colnames(meantemp_fagus_sites)[1] = 'SiteID'
colnames(relhum_fagus_sites)[1] = 'SiteID'
colnames(precip_fagus_sites)[1] = 'SiteID'

#pivoting table from wide to long to make it more accessible for subsetting 
library(tidyr)
maxtemp_fagus_sites_long <- maxtemp_fagus_sites %>%
  pivot_longer(cols=X1950.01.01:X2024.12.31,
               names_to = "Date",
               values_to = "MaxTemp")%>%
               separate(Date, into = c("Year", "Month", "Day"), sep = "\\.")

meantemp_fagus_sites_long <- meantemp_fagus_sites %>%
  pivot_longer(cols=X1950.01.01:X2024.12.31,
               names_to = "Date",
               values_to = "MeanTemp")%>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "\\.")

relhum_fagus_sites_long <- relhum_fagus_sites %>%
  pivot_longer(cols=X1950.01.01:X2024.12.31,
               names_to = "Date",
               values_to = "Relhum")%>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "\\.")

precip_fagus_sites_long <- precip_fagus_sites %>%
  pivot_longer(cols=X1950.01.01:X2024.12.31,
               names_to = "Date",
               values_to = "Precip")%>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "\\.")

#subsetting summer months 
months <- c("04", "05", "06", "07", "08")
maxtemp_fagus_summer <- data.frame()
library(dplyr)
for(i in 1:length(months)){
  subset <- maxtemp_fagus_sites_long %>% 
    filter(Month %in% months[i])
  maxtemp_fagus_summer <- rbind(maxtemp_fagus_summer, subset)
}

meantemp_fagus_summer <- data.frame()
for(i in 1:length(months)){
  subset <- meantemp_fagus_sites_long %>% 
    filter(Month == months[i])
  meantemp_fagus_summer <- rbind(meantemp_fagus_summer, subset)
}

relhum_fagus_summer <- data.frame()
for(i in 1:length(months)){
  subset <- relhum_fagus_sites_long %>% 
    filter(Month == months[i])
  relhum_fagus_summer <- rbind(relhum_fagus_summer, subset)
}

precip_fagus_summer <- data.frame()
for(i in 1:length(months)){
  subset <- precip_fagus_sites_long %>% 
    filter(Month == months[i])
  precip_fagus_summer <- rbind(precip_fagus_summer, subset)
}

#averaging variables for each year at each site 
years <- as.character(unique(maxtemp_fagus_summer$Year))
sites <- as.character(unique(maxtemp_fagus_summer$SiteID))

avg_maxtemp_fagus <- data.frame()
  
for(i in 1:length(sites)){
  subset <- maxtemp_fagus_summer %>%
    filter(SiteID == sites[i])
  for(n in 1:length(years)){
  subset_year <- subset %>%
    filter(Year %in% years[n])
  avg_maxtemp <- mean(subset_year$MaxTemp)
  df <- data.frame(SiteID=sites[i], Year=years[n], Avg_maxtemp=avg_maxtemp)
  avg_maxtemp_fagus <- rbind(avg_maxtemp_fagus, df)
}}

avg_meantemp_fagus <- data.frame()

for(i in 1:length(sites)){
  subset <- meantemp_fagus_summer %>%
    filter(SiteID == sites[i])
  for(n in 1:length(years)){
    subset_year <- subset %>%
      filter(Year %in% years[n])
    avg_meantemp <- mean(subset_year$MeanTemp)
    df <- data.frame(SiteID=sites[i], Year=years[n], Avg_meantemp=avg_meantemp)
    avg_meantemp_fagus <- rbind(avg_meantemp_fagus, df)
  }}

avg_precip_fagus <- data.frame()

for(i in 1:length(sites)){
  subset <- precip_fagus_summer %>%
    filter(SiteID == sites[i])
  for(n in 1:length(years)){
    subset_year <- subset %>%
      filter(Year %in% years[n])
    avg_precip <- mean(subset_year$Precip)
    df <- data.frame(SiteID=sites[i], Year=years[n], Avg_precip=avg_precip)
    avg_precip_fagus <- rbind(avg_precip_fagus, df)
  }}


#calculating VPD of the air for each day 
#actual VPD = (RH*VPsat)/100
#VPsat = 0.6108*exp(17.27*T/T+237.3)
VPD_dataset <- relhum_fagus_summer
VPD_dataset$MeanTemp <- meantemp_fagus_summer$MeanTemp

VPD_dataset$SVP <- 0.6108*exp((17.27*VPD_dataset$MeanTemp)/(VPD_dataset$MeanTemp+237.3))
VPD_dataset$AVP <- ((VPD_dataset$Relhum/100)*VPD_dataset$SVP)
VPD_dataset$VPD <- VPD_dataset$SVP-VPD_dataset$AVP

#calculating average VPD 
avg_VPD_fagus <- data.frame()

for(i in 1:length(sites)){
  subset <- VPD_dataset %>%
    filter(SiteID == sites[i])
  for(n in 1:length(years)){
    subset_year <- subset %>%
      filter(Year %in% years[n])
    avg_VPD <- mean(na.omit(subset_year$VPD))
    df <- data.frame(SiteID=sites[i], Year=years[n], Avg_VPD=avg_VPD)
    avg_VPD_fagus <- rbind(avg_VPD_fagus, df)
  }}

#creating one big dataset with all average climate variables in               
avg_fagus_climate <- avg_maxtemp_fagus
avg_fagus_climate$Avg_meantemp <- avg_meantemp_fagus$Avg_meantemp
avg_fagus_climate$Avg_precip <- avg_precip_fagus$Avg_precip
avg_fagus_climate$Avg_VPD <- avg_VPD_fagus$Avg_VPD

#need to get rid of X at start of year 
avg_fagus_climate$Year <- gsub("X", "", avg_fagus_climate$Year)

#merging average climate data into fagus sylv tree ring width dataset
climate_dataset_fagus <- merge(fagus_sylv_age_df, avg_fagus_climate, by=c("SiteID", "Year"))
write.csv(climate_dataset_fagus, "Data/e_obs/climate_dataset_fagus.csv")

ggplot(data=climate_dataset_fagus, aes(y=Avg_meantemp, x=Year, colour=Country))+
  geom_line()

#wrangling quercus petraea climate data
#####
library(dplyr)
quercus_sites_clim <- site_data %>% 
  filter(Species == "Quercus petraea")
quercus_sites_latlon <- quercus_sites_clim[,c(1,6,5)]

#creating a dataframe with the coordinates in 
write.csv(quercus_sites_latlon, "Data/e_obs/quercus_sites_coords.csv")

#reading the coordinates in 
quercus_coords <- read.csv("Data/e_obs/quercus_sites_coords.csv", header=TRUE, row.names="SiteID", sep=",")
quercus_coords <- quercus_coords[,-1]

#creating example map to visualise where sites are 
plot(relhum$X2024.06.01)
points(quercus_coords,pch=16,col="black")

#extracting max temperature for each site 
#returns max temperature of every day from 1950-2024
maxtemp_quercus_site <- data.frame(extract(maxtemp, quercus_coords, ncol=2))
row.names(maxtemp_quercus_site) <- row.names(quercus_coords)
write.csv(maxtemp_quercus_site, 'Data/e_obs/maxtempsitedata_quercus.csv')

#for mean temp
meantemp_quercus_site <- data.frame(extract(meantemp, quercus_coords, ncol=2))
row.names(meantemp_quercus_site) <- row.names(quercus_coords)
write.csv(meantemp_quercus_site, 'Data/e_obs/meantempsitedata_quercus.csv')

#for relative humidity
relhum_quercus_site <- data.frame(extract(relhum, quercus_coords, ncol=2))
row.names(relhum_quercus_site) <- row.names(quercus_coords)
write.csv(relhum_quercus_site, 'Data/e_obs/relhumsitedata_quercus.csv')

#for precip
precip_quercus_site <- data.frame(extract(precip, quercus_coords, ncol=2))
row.names(precip_quercus_site) <- row.names(quercus_coords)
write.csv(precip_quercus_site, 'Data/e_obs/precip_quercus.csv')

#reading in the csvs with the climate data in
maxtemp_quercus_sites <- read.csv("Data/e_obs/maxtempsitedata_quercus.csv", header=TRUE, sep=",", check.names=FALSE)
meantemp_quercus_sites <- read.csv("Data/e_obs/meantempsitedata_quercus.csv", header=TRUE, sep=",", check.names=FALSE)
relhum_quercus_sites <- read.csv("Data/e_obs/relhumsitedata_quercus.csv", header=TRUE, sep=",", check.names=FALSE)
precip_quercus_sites <- read.csv("Data/e_obs/precip_quercus.csv", header=TRUE, sep=",", check.names=FALSE)

#renaming the column with site IDs in to SiteID 
colnames(maxtemp_quercus_sites)[1] = 'SiteID'
colnames(meantemp_quercus_sites)[1] = 'SiteID'
colnames(relhum_quercus_sites)[1] = 'SiteID'
colnames(precip_quercus_sites)[1] = 'SiteID'

#pivoting table from wide to long to make it more accessible for subsetting 
library(tidyr)
maxtemp_quercus_sites_long <- maxtemp_quercus_sites %>%
  pivot_longer(cols=X1950.01.01:X2024.12.31,
               names_to = "Date",
               values_to = "MaxTemp")%>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "\\.")

meantemp_quercus_sites_long <- meantemp_quercus_sites %>%
  pivot_longer(cols=X1950.01.01:X2024.12.31,
               names_to = "Date",
               values_to = "MeanTemp")%>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "\\.")

relhum_quercus_sites_long <- relhum_quercus_sites %>%
  pivot_longer(cols=X1950.01.01:X2024.12.31,
               names_to = "Date",
               values_to = "Relhum")%>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "\\.")

precip_quercus_sites_long <- precip_quercus_sites %>%
  pivot_longer(cols=X1950.01.01:X2024.12.31,
               names_to = "Date",
               values_to = "Precip")%>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "\\.")

#subsetting summer months 
months <- c("04", "05", "06", "07", "08")
maxtemp_quercus_summer <- data.frame()
for(i in 1:length(months)){
  subset <- maxtemp_quercus_sites_long %>% 
    filter(Month %in% months[i])
  maxtemp_quercus_summer <- rbind(maxtemp_quercus_summer, subset)
}

meantemp_quercus_summer <- data.frame()
for(i in 1:length(months)){
  subset <- meantemp_quercus_sites_long %>% 
    filter(Month == months[i])
  meantemp_quercus_summer <- rbind(meantemp_quercus_summer, subset)
}

relhum_quercus_summer <- data.frame()
for(i in 1:length(months)){
  subset <- relhum_quercus_sites_long %>% 
    filter(Month == months[i])
  relhum_quercus_summer <- rbind(relhum_quercus_summer, subset)
}

precip_quercus_summer <- data.frame()
for(i in 1:length(months)){
  subset <- precip_quercus_sites_long %>% 
    filter(Month == months[i])
  precip_quercus_summer <- rbind(precip_quercus_summer, subset)
}

#averaging variables for each year at each site 
years <- as.character(unique(maxtemp_quercus_summer$Year))
sites <- as.character(unique(maxtemp_quercus_summer$SiteID))

avg_maxtemp_quercus <- data.frame()

for(i in 1:length(sites)){
  subset <- maxtemp_quercus_summer %>%
    filter(SiteID == sites[i])
  for(n in 1:length(years)){
    subset_year <- subset %>%
      filter(Year %in% years[n])
    avg_maxtemp <- mean(subset_year$MaxTemp)
    df <- data.frame(SiteID=sites[i], Year=years[n], Avg_maxtemp=avg_maxtemp)
    avg_maxtemp_quercus <- rbind(avg_maxtemp_quercus, df)
  }}

avg_meantemp_quercus <- data.frame()

for(i in 1:length(sites)){
  subset <- meantemp_quercus_summer %>%
    filter(SiteID == sites[i])
  for(n in 1:length(years)){
    subset_year <- subset %>%
      filter(Year %in% years[n])
    avg_meantemp <- mean(subset_year$MeanTemp)
    df <- data.frame(SiteID=sites[i], Year=years[n], Avg_meantemp=avg_meantemp)
    avg_meantemp_quercus <- rbind(avg_meantemp_quercus, df)
  }}

avg_precip_quercus <- data.frame()

for(i in 1:length(sites)){
  subset <- precip_quercus_summer %>%
    filter(SiteID == sites[i])
  for(n in 1:length(years)){
    subset_year <- subset %>%
      filter(Year %in% years[n])
    avg_precip <- mean(subset_year$Precip)
    df <- data.frame(SiteID=sites[i], Year=years[n], Avg_precip=avg_precip)
    avg_precip_quercus <- rbind(avg_precip_quercus, df)
  }}


#calculating VPD for each day 
#actual VPD = (RH*VPsat)/100
#VPsat = 0.6108*exp(17.27*T/T+237.3)
VPD_dataset <- relhum_quercus_summer
VPD_dataset$MeanTemp <- meantemp_quercus_summer$MeanTemp

VPD_dataset$SVP <- 0.6108*exp((17.27*VPD_dataset$MeanTemp)/(VPD_dataset$MeanTemp+237.3))
VPD_dataset$AVP <- ((VPD_dataset$Relhum/100)*VPD_dataset$SVP)
VPD_dataset$VPD <- (VPD_dataset$SVP-VPD_dataset$AVP)

#calculating average VPD 
avg_VPD_quercus <- data.frame()

for(i in 1:length(sites)){
  subset <- VPD_dataset %>%
    filter(SiteID == sites[i])
  for(n in 1:length(years)){
    subset_year <- subset %>%
      filter(Year %in% years[n])
    avg_VPD <- mean(na.omit(subset_year$VPD))
    df <- data.frame(SiteID=sites[i], Year=years[n], Avg_VPD=avg_VPD)
    avg_VPD_quercus <- rbind(avg_VPD_quercus, df)
  }}

#creating one big dataset with all average climate variables in               
avg_quercus_climate <- avg_maxtemp_quercus
avg_quercus_climate$Avg_meantemp <- avg_meantemp_quercus$Avg_meantemp
avg_quercus_climate$Avg_precip <- avg_precip_quercus$Avg_precip
avg_quercus_climate$Avg_VPD <- avg_VPD_quercus$Avg_VPD

#need to get rid of X at start of year 
avg_quercus_climate$Year <- gsub("X", "", avg_quercus_climate$Year)

#subsetting quercus data
#####
library(dplyr)
quercus_pet_dat <- compl_data %>% 
  filter(Species %in% "Quercus petraea")
#####

#calculating age 
#####
tree_id_list_3 <- c()
n <- 1
tree_id_list_3 <- unique(quercus_pet_dat$TreeID)

quercus_pet_age_df <- data.frame()

#creating an age variable
#This is sometimes a few years out from the actual age listed in the dataset...
for (n in 1:length(tree_id_list_3)){
  subset <- quercus_pet_dat %>% 
    filter(TreeID == tree_id_list_3[n])
  subset$Age <- 1:nrow(subset)
  quercus_pet_age_df <- rbind(quercus_pet_age_df, subset)
}

#merging average climate data into quercus pet tree ring width dataset
climate_dataset_quercus <- merge(quercus_pet_age_df, avg_quercus_climate, by=c("SiteID", "Year"))
write.csv(climate_dataset_quercus, "Data/e_obs/climate_dataset_quercus.csv")

#including only data from trees ranked as A
tree_data <- read.csv("Data/gentree_data/tree.csv")
fagus <- read.csv("Data/e_obs/climate_dataset_fagus.csv")
quercus <- read.csv("Data/e_obs/climate_dataset_quercus.csv")

library(dplyr)
#filtering so only trees that have rank of A 
a_rank <- tree_data %>%
  filter(Crossdating %in% "A")

#creating a list of names of trees ranked A 
a_trees <- as.list(unique(a_rank$TreeID))

#subsetting fagus climate dataset so only includes observations from trees ranked A 
fagus_rank_A <- fagus %>% 
  filter(TreeID %in% a_trees)

#subsetting quercus climate dataset so only includes observations from trees ranked A
quercus_rank_A <- quercus %>% 
  filter(TreeID %in% a_trees)

write.csv(fagus_rank_A, "Data/e_obs/climate_dataset_fagus.csv")
write.csv(quercus_rank_A, "Data/e_obs/climate_dataset_quercus.csv")

#checking spain temp in fagus dataset due to looking a little odd
#just extracting Spain 
#####
library(dplyr)
spain_sites_latlon <- fagus_sites_clim[c(17,18),c(1,6,5)]

write.csv(spain_sites_latlon, "Data/e_obs/spain_sites_coords.csv")

spain_coords <- read.csv("Data/e_obs/spain_sites_coords.csv", header=TRUE, row.names="SiteID", sep=",")
spain_coords <- spain_coords[,-1]

meantemp_spain_site <- data.frame(extract(meantemp, spain_coords, method='simple'))
row.names(meantemp_spain_site) <- row.names(spain_coords)
meantemp_spain_site$SiteID <- row.names(meantemp_spain_site)

library(tidyr)
meantemp_spains_sites_long <- meantemp_spain_site %>%
  pivot_longer(cols=X1950.01.01:X2024.12.31,
               names_to = "Date",
               values_to = "MeanTemp")%>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "\\.")

sites <- as.character(unique(meantemp_spains_sites_long$SiteID))
years <- as.character(unique(meantemp_spains_sites_long$Year))

months <- c("04","05","06","07","08")
meantemp_spain_summer <- data.frame()
library(dplyr)
for(i in 1:length(months)){
  subset <- meantemp_spains_sites_long %>% 
    filter(Month == months[i])
  meantemp_spain_summer <- rbind(meantemp_spain_summer, subset)
}

avg_meantemp_spain<-data.frame()

for(i in 1:length(sites)){
  subset <- meantemp_spains_sites_long %>%
    filter(SiteID == sites[i])
  for(n in 1:length(years)){
    subset_year <- subset %>%
      filter(Year == years[n])
    avg_meantemp <- mean(subset_year$MeanTemp)
    df <- data.frame(SiteID=sites[i], Year=years[n], Avg_meantemp=avg_meantemp)
    avg_meantemp_spain <- rbind(avg_meantemp_spain, df)
  }}

year <- c("X1950","X1960","X1970","X1980","X1990","X2000","X2010","X2020")

small_df <- data.frame()
for(i in 1:length(year)){
  subset <- avg_meantemp_spain%>% 
    filter(Year==year[i])
  small_df <- rbind(small_df, subset)
}

plot(meantemp$X2024.06.01)
points(spain_coords,pch=16,col="black")
#####

