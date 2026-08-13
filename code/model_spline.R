library(splines)
library(stats)
library(dplyr)
library(ggplot2)
library(glmmTMB)

#loading in data
#####
#fagus
fagus_sylv_dat <- read.csv('data/e_obs/climate_dataset_fagus.csv')
#putting each column into correct form
fagus_sylv_dat$Avg_max_summ_temp <- as.numeric(fagus_sylv_dat$Avg_max_summ_temp)
fagus_sylv_dat$Year <- as.factor(fagus_sylv_dat$Year)
fagus_sylv_dat$TreeID <-as.factor(fagus_sylv_dat$TreeID)
fagus_sylv_dat$SiteID <- as.factor(fagus_sylv_dat$SiteID)
#quercus
quercus_pet_dat <- read.csv('data/e_obs/climate_dataset_quercus.csv')
#putting each column into correct form
quercus_pet_dat$Avg_max_summ_temp <- as.numeric(quercus_pet_dat$Avg_max_summ_temp)
quercus_pet_dat$Year <- as.factor(quercus_pet_dat$Year)
quercus_pet_dat$TreeID <-as.factor(quercus_pet_dat$TreeID)
quercus_pet_dat$SiteID <- as.factor(quercus_pet_dat$SiteID)
#betula
betula_pen_dat <- read.csv('data/e_obs/climate_dataset_betula.csv')
#putting each column into correct form
betula_pen_dat$Avg_max_summ_temp <- as.numeric(betula_pen_dat$Avg_max_summ_temp)
betula_pen_dat$Year <- as.factor(betula_pen_dat$Year)
betula_pen_dat$TreeID <-as.factor(betula_pen_dat$TreeID)
betula_pen_dat$SiteID <- as.factor(betula_pen_dat$SiteID)

#####

#standardising elevation, age and centring year 
#####
#so that they are intepretable within model as were on very different scales before 
#scaling involves calculating the mean and sd of vector and scaling elements by those values
#by subtracting mean and dividing by sd; put them on the same scale for model
#fagus
scaled_fagus_dataset <- fagus_sylv_dat
scaled_fagus_dataset$Scaled_Elevation <- scale(scaled_fagus_dataset$Elevation)
scaled_fagus_dataset$Scaled_Age <- scale(scaled_fagus_dataset$Age)

#centering year so that intercept is median year 
#intercept is -1.5 because number of years is odd?? should intercept be 0? 
scaled_fagus_dataset$Centred_year <- as.numeric(scaled_fagus_dataset$Year) - median(as.numeric(scaled_fagus_dataset$Year))

#quercus
scaled_quercus_dataset <- quercus_pet_dat
scaled_quercus_dataset$Scaled_Elevation <- scale(scaled_quercus_dataset$Elevation)
scaled_quercus_dataset$Scaled_Age <- scale(scaled_quercus_dataset$Age)

#centering year so that intercept is median year 
#intercept is -1.5 because number of years is odd?? should intercept be 0? 
scaled_quercus_dataset$Centred_year <- as.numeric(scaled_quercus_dataset$Year) - median(as.numeric(scaled_quercus_dataset$Year))

#betula
scaled_betula_dataset <- betula_pen_dat
scaled_betula_dataset$Scaled_Elevation <- scale(scaled_betula_dataset$Elevation)
scaled_betula_dataset$Scaled_Age <- scale(scaled_betula_dataset$Age)

#centering year so that intercept is median year 
#intercept is -1.5 because number of years is odd?? should intercept be 0? 
scaled_betula_dataset$Centred_year <- as.numeric(scaled_betula_dataset$Year) - median(as.numeric(scaled_betula_dataset$Year))


#model 
#instead of fitting a spline to age, which doesn't perform well as each tree has a different 
#age span so has trouble converging, fitting to Centred_year instead, which captures additional
#biological and climatic patterns not incorporated into the model
model_spline_2_fagus <- glmmTMB(log1p(Ring.width)~ns(Centred_year, df=3)+
                          (1+ns(Centred_year, df=3)|TreeID)+
                          (1|Centred_year:SiteID),
                        data=scaled_fagus_dataset)

model_spline_2_quercus <- glmmTMB(log1p(Ring.width)~ns(Centred_year, df=3)+
                                  (1+ns(Centred_year, df=3)|TreeID)+
                                  (1|Centred_year:SiteID),
                                data=scaled_quercus_dataset)

model_spline_2_betula <- glmmTMB(log1p(Ring.width)~ns(Centred_year, df=3)+
                                  (1+ns(Centred_year, df=3)|TreeID)+
                                  (1|Centred_year:SiteID),
                                data=scaled_betula_dataset)

#prediction for the first 75 trees in the dataset, applying the spline to each tree's chronology
#empty list for each figure for each individual tree
tree_plot_fagus <- list()
tree_plot_quercus <- list()
tree_plot_betula <- list()
for(i in 1:75){
  onetree_fagus<-unique(scaled_fagus_dataset$TreeID)[i]#subsetting one tree
  trees_subset_fagus<-subset(scaled_fagus_dataset,TreeID == onetree_fagus)
  
  model_spline_pred_fagus <-predict(model_spline_2_fagus, newdata = trees_subset_fagus, re.form=NULL)#predicting a spline for individual tree
#creating tree plot
  tree_plot_fagus[[i]] <- ggplot(data=trees_subset_fagus, aes(y=log1p(Ring.width), x=Age, col=TreeID))+
    geom_point()+
    theme_cowplot()+
    theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y = element_blank())+
    labs(y= "Ring width (log+1)", x = "Age")+
    labs(title=trees_subset_fagus$TreeID, size=0.25)+
    ylim(0, 7)+
    geom_line(data = data.frame(Age = trees_subset_fagus$Age, y = model_spline_pred_fagus), aes(x = Age, y = y), colour = "blue")
#repeating for quercus
  onetree_quercus<-unique(scaled_quercus_dataset$TreeID)[i]
  trees_subset_quercus<-subset(scaled_quercus_dataset,TreeID == onetree_quercus)
  
  model_spline_pred_quercus <-predict(model_spline_2_quercus, newdata = trees_subset_quercus, re.form=NULL)
  
  tree_plot_quercus[[i]] <- ggplot(data=trees_subset_quercus, aes(y=log1p(Ring.width), x=Age, col=TreeID))+
    geom_point()+
    theme_cowplot()+
    theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y = element_blank())+
    labs(y= "Ring width (log+1)", x = "Age")+
    labs(title=trees_subset_quercus$TreeID, size=0.25)+
    geom_line(data = data.frame(Age = trees_subset_quercus$Age, y = model_spline_pred_quercus), aes(x = Age, y = y), colour = "blue")+
  ylim(0,7)
#repeating for betula
  onetree_betula<-unique(scaled_betula_dataset$TreeID)[i]
  trees_subset_betula<-subset(scaled_betula_dataset,TreeID == onetree_betula)
  
  model_spline_pred_betula <-predict(model_spline_2_betula, newdata = trees_subset_betula, re.form=NULL)
  
  tree_plot_betula[[i]] <- ggplot(data=trees_subset_betula, aes(y=log1p(Ring.width), x=Age, col=TreeID))+
    geom_point()+
    theme_cowplot()+
    theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y = element_blank())+
    labs(y= "Ring width (log+1)", x = "Age")+
    labs(title=trees_subset_betula$TreeID, size=0.25)+
    ylim(0,7)+
    geom_line(data = data.frame(Age = trees_subset_betula$Age, y = model_spline_pred_betula), aes(x = Age, y = y), colour = "blue")
  }

#creating multipanel figure
library(cowplot)
#plot with two trees from same site and one tree from a different site for each species
#to demonstrate individual and site differences
plot_grid(tree_plot_fagus[[1]], tree_plot_fagus[[2]], tree_plot_fagus[[40]],
          tree_plot_quercus[[1]], tree_plot_quercus[[2]], tree_plot_quercus[[40]],
          tree_plot_betula[[1]], tree_plot_betula[[2]], tree_plot_betula[[40]], 
          labels=c("", ""),
          nrow=3,
          ncol=3)

#plots for supplementary of 25 trees from each species
plot_grid(tree_plot_fagus[[1]], tree_plot_fagus[[2]], tree_plot_fagus[[3]],
          tree_plot_fagus[[4]], tree_plot_fagus[[5]], tree_plot_fagus[[40]],
          tree_plot_fagus[[41]], tree_plot_fagus[[42]], tree_plot_fagus[[43]],
          tree_plot_fagus[[44]], tree_plot_fagus[[45]],
          tree_plot_fagus[[60]], tree_plot_fagus[[61]], tree_plot_fagus[[62]],
          tree_plot_fagus[[63]], tree_plot_fagus[[64]], tree_plot_fagus[[65]],
          tree_plot_fagus[[70]], tree_plot_fagus[[71]], tree_plot_fagus[[72]],
          labels=c("", ""),
          scale=0.75,
          nrow=4,
          ncol=5)

plot_grid(tree_plot_quercus[[1]], tree_plot_quercus[[2]], tree_plot_quercus[[3]],
          tree_plot_quercus[[4]], tree_plot_quercus[[5]], tree_plot_quercus[[40]],
          tree_plot_quercus[[41]], tree_plot_quercus[[42]], tree_plot_quercus[[43]],
          tree_plot_quercus[[44]], tree_plot_quercus[[45]],
          tree_plot_quercus[[60]], tree_plot_quercus[[61]], tree_plot_quercus[[62]],
          tree_plot_quercus[[63]], tree_plot_quercus[[64]], tree_plot_quercus[[65]],
          tree_plot_quercus[[70]], tree_plot_quercus[[71]], tree_plot_quercus[[72]],
          labels=c("", ""),
          scale=0.75,
          nrow=4,
          ncol=5)+
  draw_label("Age (scaled)", x=0.5, y=  0, vjust=-0.5, angle= 0) +
  draw_label("Tree ring width (log + 1)", x=  0, y=0.5, vjust= 1.5, angle=90)

plot_grid(tree_plot_betula[[1]], tree_plot_betula[[2]], tree_plot_betula[[3]],
          tree_plot_betula[[4]], tree_plot_betula[[5]], tree_plot_betula[[40]],
          tree_plot_betula[[41]], tree_plot_betula[[42]], tree_plot_betula[[43]],
          tree_plot_betula[[44]], tree_plot_betula[[45]],
          tree_plot_betula[[60]], tree_plot_betula[[61]], tree_plot_betula[[62]],
          tree_plot_betula[[63]], tree_plot_betula[[64]], tree_plot_betula[[65]],
          tree_plot_betula[[70]], tree_plot_betula[[71]], tree_plot_betula[[72]],
          labels=c("", ""),
          scale=0.75,
          nrow=4,
          ncol=5)+
  draw_label("Age (scaled)", x=0.5, y=  0, vjust=-0.5, angle= 0) +
  draw_label("Tree ring width (log + 1)", x=  0, y=0.5, vjust= 1.5, angle=90)

