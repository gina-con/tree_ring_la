##fitting age spline to tree to show accuracy 

#subsetting one site 
library(dplyr)
fagus_site_ATFS13 <- scaled_mean_centred_fagus_dataset %>%
  filter(SiteID %in% "ATFS13")

#model for one site
model_ATFS13<- lmer(log1p(Ring.width)~Within+WithinQuad4+
                    Scaled_Elevation+Centred_year+
                    (1|Year)+
                      ns(Scaled_Age, df=4)+
                      (1+ns(Scaled_Age, df=4)[,3]+
                         ns(Scaled_Age, df=4)[,4]|TreeID),
                  data=fagus_site_ATFS13)

model_ATFS13_simple<- lmer(log1p(Ring.width)~Within+WithinQuad4+
                      Scaled_Elevation+Centred_year+
                      (1|TreeID)+
                      (1|Year)+
                      ns(Scaled_Age, df=4),
                    data=fagus_site_ATFS13)

summary(model_ATFS13_simple)

#plotting model onto graph by predicting (without random effects)
#using sequence of among or within and fixing other fixed effects, besides quadratic which varies correspondingly
med_year <- median(fagus_site_ATFS13$Centred_year)
med_elev <- median(fagus_site_ATFS13$Scaled_Elevation)
med_age <- median(fagus_site_ATFS13$Scaled_Age)
range(fagus_site_ATFS13$Within)
pred<-c(-2:4)
pred_quad <- c(-2:4)^2

newdata_ATFS13 <- data.frame(Centred_Year=rep(med_year, length(pred)), Scaled_Age = rep(med_age, length(pred)),
                      Scaled_Elevation=rep(med_elev, length(pred)), Within=pred,
                      WithinQuad4=pred_quad)


newdata_ATFS13$Centred_year <- as.numeric(newdata_ATFS13$Centred_Year)
newdata_ATFS13$Scaled_Age <- as.numeric(newdata_ATFS13$Scaled_Age)
newdata_ATFS13$Scaled_Elevation <- as.numeric(newdata_ATFS13$Scaled_Elevation)

summary(newdata_ATFS13)
#w/out random effects 
modelpred_simple<-predict(model_ATFS13_simple,newdata=newdata_ATFS13,re.form=NA, type = "response")
modelpred<-predict(model_ATFS13,newdata=newdata_ATFS13,re.form=NA, type = "response")

plot(log1p(Ring.width)~Within, data=fagus_site_ATFS13)
points(newdata_ATFS13$Within, modelpred, lwd="1", col="red")

plot(log1p(Ring.width)~Within, data=fagus_site_ATFS13)
points(newdata_ATFS13$Within, modelpred_simple, lwd="1", col="red")

#individual climate models
for(i in 1:length(trees_ATFS13)){
  onetree<-unique(fagus_site_ATFS13$TreeID)[i]
  trees_subset<-subset(fagus_site_ATFS13,TreeID == onetree)
  
  model_tree<- lm(log1p(Ring.width)~Within+WithinQuad4+
                        as.numeric(Scaled_Elevation)+
                        ns(Scaled_Age, df=4),
                      data=trees_subset)
  
  med_year <- median(trees_subset$Centred_year)
  med_elev <- median(trees_subset$Scaled_Elevation)
  med_age <- median(trees_subset$Scaled_Age)
  mintree <- min(trees_subset$Within)
  maxtree <- max(trees_subset$Within)
  pred<-c(mintree:maxtree)
  pred_quad <- c(mintree:maxtree)^2
  
  newdata_tree <- data.frame(Scaled_Age = rep(med_age, length(pred)),
                               Scaled_Elevation=rep(med_elev, length(pred)), Within=pred,
                               WithinQuad4=pred_quad)

  modelpred<-predict(model_tree,newdata=newdata_tree,re.form=NA, type = "response")
  
  plot(log1p(Ring.width)~Within, data=trees_subset)
  lines(newdata_tree$Within, modelpred, lwd="1", col="red")
}

#individual age splines fitted
for(i in 1:length(trees_ATFS13)){
  onetree<-unique(fagus_site_ATFS13$TreeID)[i]
  trees_subset<-subset(fagus_site_ATFS13,TreeID == onetree)
  
  model_tree_age<- lm(log1p(Ring.width)~Within+WithinQuad4+
                    as.numeric(Scaled_Elevation)+
                    ns(Scaled_Age, df=4),
                  data=trees_subset)
  
  med_year <- median(trees_subset$Centred_year)
  med_elev <- median(trees_subset$Scaled_Elevation)
  med_age <- median(trees_subset$Scaled_Age)
  pred_age <- seq(-1.25, 2.50, 0.25)
  med_within <- median(trees_subset$Within)
  med_withinquad <- median(trees_subset$WithinQuad4)
  
  
  newdata_tree_age <- data.frame(Scaled_Age = pred_age,
                             Scaled_Elevation=rep(med_elev, length(pred_age)), 
                             Within=rep(med_within, length(pred_age)),
                             WithinQuad4=rep(med_withinquad, length(pred_age)))
  
  modelpred_age<-predict(model_tree_age, newdata=newdata_tree_age, re.form=NA, type = "response")
  
  plot(log1p(Ring.width)~Scaled_Age, data=trees_subset)
  lines(newdata_tree_age$Scaled_Age, modelpred_age, lwd="1", col="red")
}
