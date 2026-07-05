library(lme4)
library(lmerTest)
library(splines)
library(stats)
library(dplyr)
library(ggplot2)
library(glmmTMB)

#loading in data
#####
fagus_sylv_dat <- read.csv('data/e_obs/climate_dataset_fagus.csv')
fagus_sylv_dat$Avg_max_summ_temp <- as.numeric(fagus_sylv_dat$Avg_max_summ_temp)
fagus_sylv_dat$Year <- as.factor(fagus_sylv_dat$Year)
fagus_sylv_dat$TreeID <-as.factor(fagus_sylv_dat$TreeID)
fagus_sylv_dat$SiteID <- as.factor(fagus_sylv_dat$SiteID)
#####

#standardising elevation, age and centring year 
#####
#so that they are intepretable within model as were on very different scales before 
#scaling involves calculating the mean and sd of vector and scaling elements by those values
#by subtracting mean and dividing by sd; put them on the same scale for model
scaled_fagus_dataset <- fagus_sylv_dat
scaled_fagus_dataset$Scaled_Elevation <- scale(scaled_fagus_dataset$Elevation)
scaled_fagus_dataset$Scaled_Age <- scale(scaled_fagus_dataset$Age)

#centering year so that intercept is median year 
#intercept is -1.5 because number of years is odd?? should intercept be 0? 
scaled_fagus_dataset$Centred_year <- as.numeric(as.character(scaled_fagus_dataset$Year)) - median(as.numeric(as.character(scaled_fagus_dataset$Year)))

#model 
#instead of fitting a spline to age, which doesn't perform well as each tree has a different 
#age span so has trouble converging, fitting to Centred_year instead, which captures additional
#biological and climatic patterns not incorporated into the model
model_spline_2 <- glmmTMB(log1p(Ring.width)~ns(Centred_year, df=3)+
                          (1+ns(Centred_year, df=3)|TreeID)+
                          (1|Centred_year:SiteID),
                        data=scaled_fagus_dataset)

#prediction for the first 75 trees in the dataset, applying the spline to each tree's chronology
tree_plot <- list()
for(i in 1:75){
  onetree<-unique(scaled_fagus_dataset$TreeID)[i]
  trees_subset<-subset(scaled_fagus_dataset,TreeID == onetree)
  
  model_spline_pred <-predict(model_spline_2, newdata = trees_subset, re.form=NULL)

  tree_plot[[i]] <- ggplot(data=trees_subset, aes(y=log1p(Ring.width), x=Scaled_Age, col=TreeID))+
    geom_point()+
    theme_cowplot()+
    theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y = element_blank())+
    labs(y= "Ring width (log+1)", x = "Scaled Age")+
    labs(title=trees_subset$TreeID, size=1)+
    geom_line(data = data.frame(Scaled_Age = trees_subset$Scaled_Age, y = model_spline_pred), aes(x = Scaled_Age, y = y), colour = "blue")
}

#creating multipanel figure
library(cowplot)

plot_grid(tree_plot[[1]], tree_plot[[2]], tree_plot[[3]],
          tree_plot[[4]], tree_plot[[5]], tree_plot[[40]],
          tree_plot[[41]], tree_plot[[42]], tree_plot[[43]],
          tree_plot[[44]], tree_plot[[45]],
          tree_plot[[60]], tree_plot[[61]], tree_plot[[62]],
          tree_plot[[63]], tree_plot[[64]], tree_plot[[65]],
          tree_plot[[70]], tree_plot[[71]], tree_plot[[72]],
          labels=c("", ""),
          scale=0.75,
          nrow=4,
          ncol=5)+
  draw_label("Age (scaled)", x=0.5, y=  0, vjust=-0.5, angle= 0) +
  draw_label("Tree ring width (log + 1)", x=  0, y=0.5, vjust= 1.5, angle=90)


###quercus
#loading in data
#####
querc_pet_dat <- read.csv('data/e_obs/climate_dataset_quercus.csv')
#####

#standardising elevation, age and centring year 
#####
#so that they are intepretable within model as were on very different scales before 
#scaling involves calculating the mean and sd of vector and scaling elements by those values
#by subtracting mean and dividing by sd; put them on the same scale for model
scaled_quercus_dataset <- querc_pet_dat
scaled_quercus_dataset$Scaled_Elevation <- scale(scaled_quercus_dataset$Elevation)
scaled_quercus_dataset$Scaled_Age <- scale(scaled_quercus_dataset$Age)

#centering year so that intercept is median year 
#intercept is -1.5 because number of years is odd?? should intercept be 0? 
scaled_quercus_dataset$Centred_year <- as.numeric(as.character(scaled_quercus_dataset$Year)) - median(as.numeric(as.character(scaled_quercus_dataset$Year)))

#model 
model_spline_2 <- glmmTMB(log1p(Ring.width)~ns(Centred_year, df=3)+
                            (1+ns(Centred_year, df=3)|TreeID)+
                            (1|Centred_year:SiteID),
                          data=scaled_quercus_dataset)
#prediction 
tree_plot_quercus <- list()
for(i in 1:75){
  onetree<-unique(scaled_quercus_dataset$TreeID)[i]
  trees_subset<-subset(scaled_quercus_dataset,TreeID == onetree)
  
  model_spline_pred <-predict(model_spline_2, newdata = trees_subset, re.form=NULL)
  
  tree_plot_quercus[[i]] <- ggplot(data=trees_subset, aes(y=log1p(Ring.width), x=Scaled_Age, col=TreeID))+
    geom_point()+
    theme_cowplot()+
    theme(legend.position = "none")+
    theme(axis.title.y=element_blank())+
    theme(axis.title.x=element_blank())+
    labs(title=trees_subset$TreeID, size=1)+
    geom_line(data = data.frame(Scaled_Age = trees_subset$Scaled_Age, y = model_spline_pred), aes(x = Scaled_Age, y = y), colour = "blue")
}

#creating figure
library(cowplot)

print(tree_plot_quercus[[1]])

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

