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

#extracting elevation data from site dataframe and adding to tree ring dataframe
#####
elevdata <- data.frame(site_data$SiteID, site_data$Elevation)
colnames(elevdata) <- c("SiteID", "Elevation")
compl_data <- left_join(tree_ring_data, elevdata, "SiteID")
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
n <- as.numeric(1)
tree_id_list_2 <- unique(fagus_sylv_dat$TreeID)

fagus_sylv_age_df <- data.frame()

#creating an age variable
for (n in 1:length(tree_id_list_2)){
  subset <- fagus_sylv_dat %>% 
    filter(TreeID == tree_id_list_2[n])
  subset$Age <- 1:nrow(subset)
  fagus_sylv_age_df <- rbind(fagus_sylv_age_df, subset)
}
#####

#subsetting Quercus petraea
#####
library(dplyr)
quercus_pet_dat <- compl_data %>% 
  filter(Species %in% "Quercus petraea")
#####

#calculating age 
#####
tree_id_list_3 <- c()
n <- as.numeric(1)
tree_id_list_3 <- unique(quercus_pet_dat$TreeID)

quercus_pet_age_df <- data.frame()

#creating an age variable
for (n in 1:length(tree_id_list_3)){
  subset <- quercus_pet_dat %>% 
    filter(TreeID == tree_id_list_3[n])
  subset$Age <- 1:nrow(subset)
  quercus_pet_age_df <- rbind(quercus_pet_age_df, subset)
}
#####

#climate data wrangling 
######---
#downloaded from Copernicus Climate Data Store on 03/05/2026 at 11:00 
#ensemble mean, maximum temperature, across whole time period
#downloaded from Copernicus Climate Data Store on 19/05/2026 at 14:00
#ensemble mean, rel humidity, across whole time period
library(sp)
library(raster)
library(ncdf4)
library(here)
library(ggplot2)

#loading in E-obs maxtemp data
maxtemp <- brick(here::here("Data/e_obs/maxtempdata.nc"))
meantemp <- brick(here::here("Data/e_obs/meantempdata.nc"))
relhum <- brick(here::here("Data/e_obs/relhumdata.nc"))

##climate data for fagus sylvatica, VPD, mean and max temp; precipitation will be added later
#####
#subsetting fagus sylvatica sites
library(dplyr)
fagus_sites_clim <- site_data %>% 
  filter(Species == "Fagus sylvatica")
fagus_sites_latlon_only <- fagus_sites_clim[,c(6,5)]

#creating example map to visualise where sites are 
plot(relhum$X2000.01.16)
points(fagus_sites_latlon_only,pch=16)

#extracting max temperature for each site 
#returns max temperature of every day from 1950-2024
maxtemp_fagus_sites <- data.frame(extract(maxtemp,fagus_sites_latlon_only, ncol="2"))
write.csv(maxtemp_fagus_sites, 'data/e_obs/maxtempsitedata.csv')
#for mean temp
meantemp_fagus_sites <- data.frame(extract(meantemp,fagus_sites_latlon_only, ncol="2"))
write.csv(meantemp_fagus_sites, 'data/e_obs/meantempsitedata.csv')
#for relative humidity
relhum_fagus_sites <- data.frame(extract(relhum,fagus_sites_latlon_only, ncol="2"))
write.csv(relhum_fagus_sites, 'data/e_obs/relhumsitedata.csv')

###subsetting growing season of each year 
#accounting for leap years 
days <- c(365, 365, 366, 365)
days2 <- rep(days,20)

#75 years so 75 summers of temperature to extract
df_names <- 1:75
df_names2 <- rep(c("df"), 75)
dfnames <- as.list(paste(df_names2, df_names, sep="_"))
dfnames_meantemp <- as.list(paste(df_names2, df_names, sep="_"))
dfnames_relhum <- as.list(paste(df_names2, df_names, sep="_"))

#basing on study by Hurdebise et al looking at affect of increasing temp over 18yr period on growing season of F. sylvatica 
#beginning of range of carbon uptake increase start day and end of range of carbon uptake decrease end day.
dayname <- 48:299

s <- 0
e <- 0

for(i in 1:length(dfnames)){
  dfnames[[i]] <- matrix(data=NA, nrow=24, ncol=251)
  dfnames[[i]] <- maxtemp_fagus_sites[,(48+s):(299+e)]
  colnames(dfnames[[i]]) <- dayname
  s <- s+days2[i]
  e <- e+days2[i]
} 

s <- 0
e <- 0

for(i in 1:length(dfnames_meantemp)){
  dfnames_meantemp[[i]] <- matrix(data=NA, nrow=24, ncol=251)
  dfnames_meantemp[[i]] <- meantemp_fagus_sites[,(48+s):(299+e)]
  colnames(dfnames_meantemp[[i]]) <- dayname
  s <- s+days2[i]
  e <- e+days2[i]
} 

s <- 0
e <- 0

for(i in 1:length(dfnames_relhum)){
  dfnames_relhum[[i]] <- matrix(data=NA, nrow=24, ncol=251)
  dfnames_relhum[[i]] <- relhum_fagus_sites[,(48+s):(299+e)]
  colnames(dfnames_relhum[[i]]) <- dayname
  s <- s+days2[i]
  e <- e+days2[i]
} 

#freeing up space by removing huge dataset once finished with
rm(maxtemp_fagus_sites)
rm(meantemp_fagus_sites)
rm(relhum_fagus_sites)

#binding all year dfs together into one big df to calculate avg max summer temps of each year
bigsummdf <- plyr::ldply(dfnames, data.frame, .id=NULL)
bigsummdf_meantemp <- plyr::ldply(dfnames_meantemp, data.frame, .id=NULL)
bigsummdf_relhum <- plyr::ldply(dfnames_relhum, data.frame, .id=NULL)

#calculating VPD for each day 
#actual VPD = (RH*VPsat)/100
#VPsat = 0.6108*exp(17.27*T/T+273.3)
VPDdf <- matrix(ncol=252, nrow=1800)

r <- 1:1800
c <- 1:252

#calculating VPD for each day 
for(i in 1:length(r)){
  for(n in 1:length(c)){
    VPDdf[r[i],c[n]] <- (bigsummdf_relhum[r[i],c[n]] * (0.6108*exp((17.27*bigsummdf_meantemp[r[i],c[n]])/(bigsummdf_meantemp[r[i],c[n]]+237.3))))/100
  }
}

#calculating average max growing season for each site each year
#each row represents the max temperature of every day of summer of each year 
avgtempnames <- rep("avg", )
years <- rep(1950:2024,each=24)
sitenames2 <- fagus_sites_clim$SiteID
fagus_site_avgmxsummtemps <- as.list(paste(avgtempnames, years, sitenames2, sep="_"))
fagus_site_avgmeansummtemps <- as.list(paste(avgtempnames, years, sitenames2, sep="_"))
fagus_site_avgrelsummhum <- as.list(paste(avgtempnames, years, sitenames2, sep="_"))

for(i in 1:length(fagus_site_avgmxsummtemps)){
  fagus_site_avgmxsummtemps[[i]] <- sum(bigsummdf[i,])/252
}

#for mean temp
for(i in 1:length(fagus_site_avgmeansummtemps)){
  fagus_site_avgmeansummtemps[[i]] <- sum(bigsummdf_meantemp[i,])/252
}

#for VPD
for(i in 1:length(fagus_site_avgVPD)){
  fagus_site_avgrelsummhum[[i]] <- sum(VPDdf[i,])/252
}

#creating df with max summer temperature for each site for each year 
fagus_site_avgmxsummtemps_char <- as.character(fagus_site_avgmxsummtemps)
avg_temp_df <- data.frame(years, sitenames2, fagus_site_avgmxsummtemps_char, fagus_site_avgmeansummtemps, fagus_site_avgVPD)
colnames(avg_temp_df) <- c("Year", "SiteID", "Avg_max_summ_temp", "Avg_mean_summ_temp", "Avg_VPD")

#duplicate values for sites that are really close together (psuedoreplication???)

#adding averages to Fagus sylv data
fagus_sylv_dat_maxtemp <- merge(fagus_sylv_age_df, avg_temp_df, 
                                by=c("SiteID", "Year"))

write.csv(fagus_sylv_dat_maxtemp, 'data/e_obs/complete_fagus_data.csv')

#big reduction in rows is due to years not included in climate data
##original dataset has 30058 measurements pre 1950 that aren't included in final dataset due to lack of climate data

#####


###FOR LATER DATE; to be updated later.
##climate data for Quercus petraea 
#####
#subsetting Quercus petraea sites
library(dplyr)
quercus_petraea_clim <- site_data %>% 
  filter(Species == "Quercus petraea")
quercus_petraea_sites_latlon_only <- quercus_petraea_clim[,c(6,5)]

#creating example map to visualise where sites are 
plot(maxtemp$X2000.01.16)
points(quercus_petraea_sites_latlon_only,pch=16)

#extracting max temperature for each site 
#returns max temperature of every day from 1950-2024
maxtemp_quercus_petraea_sites <- data.frame(extract(maxtemp,quercus_petraea_sites_latlon_only, ncol="2"))
write.csv(maxtemp_quercus_petraea_sites, 'data/e_obs/maxtempsitedata_quercus_petraea.csv')

###subsetting summer of each year 
#accounting for leap years 
days <- c(365, 365, 366, 365)
days2 <- rep(days,20)

#75 years so 75 summers of temperature to extract
df_names_quercus_petraea <- 1:75
df_names2_quercus_petraea <- rep(c("df"), 75)
dfnames_quercus_petraea <- as.list(paste(df_names2, df_names, sep="_"))

#will be pulling out days 152 (1st May) to day 243 (31st August) of each year
dayname <- 152:243

s <- 0
e <- 0

for(i in 1:length(dfnames_quercus_petraea)){
  dfnames_quercus_petraea[[i]] <- matrix(data=NA, nrow=24, ncol=92)
  dfnames_quercus_petraea[[i]] <- maxtemp_quercus_petraea_sites[,(152+s):(243+e)]
  colnames(dfnames_quercus_petraea[[i]]) <- dayname
  s <- s+days2[i]
  e <- e+days2[i]
} 

#freeing up space by removing huge dataset once finished with
rm(maxtemp_quercus_petraea_sites)

#binding all year dfs together into one big df to calculate avg max summer temps of each year
bigsummdf_quercus_petraea <- plyr::ldply(dfnames_quercus_petraea, data.frame, .id=NULL)

#calculating average max summer temperature for each site each year
#each row represents the max temperature of every day of summer of each year 
avgtempnames <- rep("avg", )
years <- rep(1950:2024,each=24)
sitenames2_quercus_petraea <- quercus_petraea_clim$SiteID
quercus_petraea_site_avgmxsummtemps <- as.list(paste(avgtempnames, years, sitenames2_quercus_petraea, sep="_"))

for(i in 1:length(quercus_petraea_site_avgmxsummtemps)){
  quercus_petraea_site_avgmxsummtemps[[i]] <- sum(bigsummdf_quercus_petraea[i,])/93
}

#creating df with max summer temperature for each site for each year 
quercus_petraea_site_avgmxsummtemps_char <- as.character(quercus_petraea_site_avgmxsummtemps)
avg_temp_df_quercus_petraea <- data.frame(years, sitenames2, quercus_petraea_site_avgmxsummtemps_char)
colnames(avg_temp_df_quercus_petraea) <- c("Year", "SiteID", "Avg_max_summ_temp")

#adding averages to Quercus pet data
#for some reason doesn't like this code??????
quercus_petraea_dat_maxtemp <- merge(quercus_pet_age_df, avg_temp_df_quercus_petraea,
                                     by=c("SiteID", "Year"))

write.csv(quercus_petraea_dat_maxtemp, 'data/e_obs/complete_quercus_petraea_data.csv')

#####

