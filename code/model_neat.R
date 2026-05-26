library(lme4)
library(lmerTest)
library(splines)
library(stats)
library(dplyr)
library(ggplot2)

#loading in data
#####
fagus_sylv_dat_maxtemp <- read.csv('data/e_obs/complete_fagus_data.csv')
summary(fagus_sylv_dat_maxtemp)
fagus_sylv_dat_maxtemp$Avg_max_summ_temp <- as.numeric(fagus_sylv_dat_maxtemp$Avg_max_summ_temp)
fagus_sylv_dat_maxtemp$Year <- as.factor(fagus_sylv_dat_maxtemp$Year)
fagus_sylv_dat_maxtemp$TreeID <-as.factor(fagus_sylv_dat_maxtemp$TreeID)
fagus_sylv_dat_maxtemp$SiteID <- as.factor(fagus_sylv_dat_maxtemp$SiteID)
#####

#mean centring climate data and calculating quadratics
#####
mean_centred_fagus_dataset <- data.frame()
fagus_populations <- unique(fagus_sylv_dat_maxtemp$SiteID)

for (i in 1:length(fagus_populations)){
  subset <- fagus_sylv_dat_maxtemp %>% 
    filter(SiteID == fagus_populations[i])
  subset$Among <- mean(subset$Avg_max_summ_temp)#among is the mean max summer temperature of each site
  subset$Within <- (subset$Avg_max_summ_temp)-(mean(subset$Avg_max_summ_temp))#within is the deviation of each observation from the mean of each site
  subset$WithinQuad4 <- ((subset$Avg_max_summ_temp)-(mean(subset$Avg_max_summ_temp)))^2 #quadratic calculated using equation 4 from Fay et al
  #equation 4 assumes that the effect of temperature varies among populations (which would be the case if locally adapted)
  subset$WithinQuad3 <- (((subset$Avg_max_summ_temp)^2)-((mean(subset$Avg_max_summ_temp))^2)) #quadratic calculated using equation 3 from Fay et al
  #equation 3 assumes that individuals show a common quadratic pattern over the range of temperature (absolute process)
  subset$AmongQuad <- (mean(subset$Avg_max_summ_temp))^2
  mean_centred_fagus_dataset <- rbind(mean_centred_fagus_dataset, subset)
}

rm(subset)
#####

#standardising elevation, age and centring year 
#####
#so that they are intepretable within model as were on very different scales before 
#scaling involves calculating the mean and sd of vector and scaling elements by those values
#by subtracting mean and dividing by sd; put them on the same scale for model
scaled_mean_centred_fagus_dataset <- mean_centred_fagus_dataset
scaled_mean_centred_fagus_dataset$Scaled_Elevation <- scale(scaled_mean_centred_fagus_dataset$Elevation)
scaled_mean_centred_fagus_dataset$Scaled_Age <- scale(scaled_mean_centred_fagus_dataset$Age)
scaled_mean_centred_fagus_dataset <- scaled_mean_centred_fagus_dataset[,-c(1,5,6,7,8)]

#mean centering year so that intercept is median year 
scaled_mean_centred_fagus_dataset$Centred_year <- as.numeric(scaled_mean_centred_fagus_dataset$Year) - median(as.numeric(scaled_mean_centred_fagus_dataset$Year))
#####

#modelling
#####
#visualising distribution of ring width data to determine appropriate model
hist(scaled_mean_centred_fagus_dataset$Ring.width)
hist(log1p(scaled_mean_centred_fagus_dataset$Ring.width))
#####

#model 1 
#####
#model with log transformed ring widths to account for no negative values (intercept is proportional change per unit)
#random effects are TreeID, SiteID, Year and SiteID by Year
#Fixed effects are scaled_elevation and Among (mean max summ temp of each site)
#Age effects are fitted using a natural spline with 3 degrees of freedom with scaled age
#This model represents the global relationship 
#uses quadratic equation 4 from Fay et al 
model1 <- lmer(log1p(Ring.width)~Among+Within+WithinQuad4+AmongQuad+
                               Scaled_Elevation+as.numeric(Year)+
                               (1|TreeID)+
                               (1|Year)+
                               (1|SiteID:Year)+
                               ns(Scaled_Age, df=4),
               REML = FALSE,
                             data=scaled_mean_centred_fagus_dataset)

#uses quadratic equation 3 from Fay et al 
model1_2 <- lmer(log1p(Ring.width)~Among+Within+WithinQuad3+AmongQuad+
                 Scaled_Elevation+Centred_year+
                 (1|TreeID)+
                 (1|Year)+
                 (1|SiteID:Year)+
                 ns(Scaled_Age, df=4),
                 REML = FALSE,
               data=scaled_mean_centred_fagus_dataset)

#plotting among prediction 
plot(log1p(Ring.width)~Among, data=scaled_mean_centred_fagus_dataset)
points(newdata$Among, modelpred, lwd="1", col="red")

#plotting within prediction
plot(log1p(Ring.width)~Avg_max_summ_temp, data=scaled_mean_centred_fagus_dataset)
plot(log1p(Ring.width)~Within, data=scaled_mean_centred_fagus_dataset)
points(newdata$Within, modelpred2, lwd="1", col="red")
#####

#model 2 <- model2_90 is working model
#####
#in this model, a random effect for age effects by site is included.
#uses quadratic equation 4 from Fay et al 
#1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#Model failed to converge with max|grad| = 0.00351187 (tol = 0.002, component 1)
#2: 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#unable to evaluate scaled gradient
#2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                  Model failed to converge: degenerate  Hessian with 3 negative eigenvalues

#trying without the outlier age site
#Warning messages:
#  1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
 #                   unable to evaluate scaled gradient
  #                2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
   #                                 Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

library(dplyr)
library(ggplot2)
ggplot(data=no_outlier, aes(x=Age, y=Ring.width, colour=SiteID))+
  geom_line()+
  facet_wrap(vars(SiteID))

#this dataset has a tree that is over 400 years old, much older than the other trees
no_outlier <- scaled_mean_centred_fagus_dataset %>% 
  filter(SiteID != "FRFS06")

#No errors for this model
#Applying the random site effect to the 2nd and 3rd polynomial
model2_90 <- lmer(log1p(Ring.width)~Among+Within+WithinQuad4+AmongQuad+
                      Scaled_Elevation+Centred_year+
                      (1|TreeID)+
                      (1|Year)+
                 (1|SiteID:Year)+
                      ns(Scaled_Age, df=4)+
                      (1+ns(Scaled_Age, df=4)[,2]+
                         ns(Scaled_Age, df=4)[,3]|SiteID),
                    REML = FALSE,
                    data=scaled_mean_centred_fagus_dataset)
summary(model2_90)

#plotting model onto graph by predicting (without random effects)
#using sequence of among or within and fixing other fixed effects, besides quadratic which varies correspondingly
pred<-c(16:28)#among
med_year <- median(scaled_mean_centred_fagus_dataset$Centred_year)
med_elev <- median(scaled_mean_centred_fagus_dataset$Scaled_Elevation)
med_within <- median(scaled_mean_centred_fagus_dataset$Within)
med_age <- median(scaled_mean_centred_fagus_dataset$Scaled_Age)
med_withinquad4 <- median(scaled_mean_centred_fagus_dataset$WithinQuad4)
pred_quad <- c(16:28)^2

range(scaled_mean_centred_fagus_dataset$Within)
pred2<-c(-3:5)
pred2_quad <- c()

med_among <- median(scaled_mean_centred_fagus_dataset$Among)
med_amongquad3 <- median(scaled_mean_centred_fagus_dataset)

newdata <- data.frame(Among=pred, Centred_Year=rep(med_year, length(pred)), Scaled_Age = rep(med_age, length(pred)),
                      Scaled_Elevation=rep(med_elev, length(pred)), Within=rep(med_within, length(pred)),
                      WithinQuad3=rep(med_withinquad4, length(pred)), AmongQuad=pred_quad)

newdata2 <- data.frame(Among=rep(med_among, length(pred2)), Centred_Year=rep(med_year, length(pred2)), Scaled_Age = rep(med_age, length(pred2)),
                       Scaled_Elevation=rep(med_elev, length(pred2)), Within=pred2,
                       WithinQuad4=pred2_quad, AmongQuad=rep(med_amongquad3, length(pred2)))


newdata$Centred_year <- as.numeric(newdata$Centred_Year)
newdata$Scaled_Age <- as.numeric(newdata$Scaled_Age)
newdata$Among <- as.numeric(newdata$Among)
newdata$Scaled_Elevation <- as.numeric(newdata$Scaled_Elevation)

summary(newdata)
#w/out random effects 
modelpred<-predict(model1_2,newdata=newdata,re.form=NA, type = "response")
modelpred2<-predict(model1_2,newdata=newdata2,re.form=NA, type = "response")

modelpred

#fitting individual tree age spline 

#boundary (singular) fit: see help('isSingular')
#Warning message:
#  Model failed to converge with 2 negative eigenvalues: -8.7e-03 -2.7e+01 
model2_test <- lmer(log1p(Ring.width)~Among+Within+WithinQuad4+AmongQuad+
                  Scaled_Elevation+Centred_year+
                  (1|TreeID)+
                  (1|Year)+
                  ns(Scaled_Age, df=4)+
                  (1+ns(Scaled_Age, df=4)|SiteID:Year),
               REML = FALSE,
                data=scaled_mean_centred_fagus_dataset)


#uses quadratic equation 3 from Fay et al 
#SiteID random effect has been removed.
#1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#Model failed to converge with max|grad| = 0.00351187 (tol = 0.002, component 1)
#2: Model failed to converge with 1 negative eigenvalue: -7.7e-06 
model2_2 <- lmer(log1p(Ring.width)~Among+Within+WithinQuad3+AmongQuad+
                               Scaled_Elevation+Centred_year+
                               (1|TreeID)+
                               (1|Year)+
                               (1|SiteID:Year)+
                               ns(Scaled_Age, df=4)+
                               (1+ns(Scaled_Age, df=4)|SiteID),
                 REML = FALSE,
                             data=scaled_mean_centred_fagus_dataset)
summary(model2_2)

#with SiteID added back in 
#Warning messages:
#  1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                    Model failed to converge with max|grad| = 0.00222859 (tol = 0.002, component 1)
#                  2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                    Model is nearly unidentifiable: large eigenvalue ratio
#                                  - Rescale variables?

model2_2 <- lmer(log1p(Ring.width)~Among+Within+WithinQuad3+AmongQuad+
                   Scaled_Elevation+Centred_year+
                   (1|TreeID)+
                   (1|Year)+
                   (1|SiteID)+
                   (1|SiteID:Year)+
                   ns(Scaled_Age, df=4)+
                   (1+ns(Scaled_Age, df=4)|SiteID),
                 REML = FALSE,
                 data=scaled_mean_centred_fagus_dataset)
summary(model2_2)

#taking SiteID out improves AIC by only 2 and also returns model failed to converge degenerate Hessian warning.
#####

#model 3
#####
#Warning messages:
 # 1: Some predictor variables are on very different scales: consider rescaling 
#2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  #                unable to evaluate scaled gradient
   #             3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
    #                              Model failed to converge: degenerate  Hessian with 3 negative eigenvalues
     #                           4: Some predictor variables are on very different scales: consider rescaling 
model3 <- lmer(log1p(Ring.width)~Among+Within+WithinQuad3+AmongQuad+
                   Elevation+Centred_year+
                   (1|TreeID)+
                   (1|Year)+
                   (1|SiteID:Year)+
                   ns(Age, df=4)+
                 (1+ns(Age, df=4)|SiteID),
               REML = FALSE,
                 data=scaled_mean_centred_fagus_dataset)
summary(model3)

aicresults <- AIC(model1, model1_2, model2, model2_test, model2_2, model3)
aicresults
#fit with gamma 
#error: non positive values not allowed for the Gamma family 
library(glmmTMB)
?glmmTMB
model3_in <- glmer(Ring.width~Among+Within+WithinQuad3+AmongQuad+Centred_year+
                 Scaled_Elevation+
                 (1|TreeID)+
                 (1|SiteID)+
                 (1|Year)+
                 (1|SiteID:Year)+
                 ns(Scaled_Age, df=3)+
                (1+ns(Scaled_Age, df=4)|SiteID),
                REML = FALSE,
               data=scaled_mean_centred_fagus_dataset,
               family = "Gamma")
#####

#running AIC to compare models 
#####
aic_analysis <- AIC(model1, model1_2, model2, model2_2, model3)
#####
