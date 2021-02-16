##### Packages #####
library(MASS)
library(corrplot)
library(car)
library(leaps)
library(magrittr)
library(tidyverse)
library(rsample)
library(caTools)
library(Metrics)
library(caret) 
library(ggplot2)
library(boot)

##### Functions #####

HistogramFunction <- function(df, xvar, xlab){
  ggplot(df, aes(x=xvar)) + 
    geom_histogram(aes(y=stat(density)), bins=30, colour="black", fill="white")+
    geom_density(size=1, color="blue")+
    geom_vline(aes(xintercept=mean(xvar)), color="blue",
               linetype="dashed")+
    labs(title="",x=xlab, y = "Density")+
    theme_bw()
}


BoxCoxFunction <- function(df, xvar){
  bc = boxcox(xvar~1)
  lambda = bc$x[which.max(bc$y)]
  return(lambda)
}

QQplotFunction <- function(df, xvar){
  ggplot(df, aes(sample=xvar))+
    stat_qq(size=2, alpha=.4)+
    stat_qq_line(col="blue", size=1)+
    theme_bw()
}

ScatterPlotFunction <- function(df, xvar, xlabname){
  plot(x=xvar, y=linternet, xlab=xlabname, ylab="Log No Internet", data=df )
  lines(lowess(xvar, linternet), col="blue")
}

ValidationTable <- function(fit, model_type){
  mod <- fit
  fit_summary <- tibble(Model=model_type,
                       "Number of Features"=length((coef(mod) %>% names())[-1]),
                        MSE=mean(mod$residuals^2), 
                        RMSE=sqrt(MSE),
                        Adj.R.squared=summary(mod)$adj.r.squared,
                        F.statistics=summary(mod)$fstatistic[[1]],
                        AIC=AIC(mod))
  return(fit_summary)
}

##### Load data #####
Internet<-read.csv("C:/Users/happy/Documents/Github/broadband.csv", header=T)
sum(is.na(Internet)) # no missing value
dim(Internet) # 3000*11
head(Internet,10)
names(Internet) 
# "X" "no_internet" "population"  "unemp"  "health_ins"  "poverty"  "SNAP"  "broad_num"   "broad_avail" "broad_cost"  "all25_bbn" 


###### Summary Statistic #####
summary(Internet)
str(Internet)
attach(Internet)

###### Exploratory Data Analysis #####
# Response variable
HistogramFunction(Internet, no_internet, "no_internet") 
QQplotFunction(Internet, no_internet)
shapiro.test(Internet$no_internet) 
#The null hypothesis for this test is that the data are normally distributed. 
#p-value < 2.2e-16, then the null hypothesis is rejected.
#Not normally distributed distribution

# Box-Cox transformation 
BoxCoxFunction(Internet, no_internet)  
linternet <- log(no_internet)

# Transformed response variable
HistogramFunction(Internet, linternet, "Log No Internet") 
QQplotFunction(Internet, linternet)

# Graphics for explorations of relationships
Internetvars = data.frame(no_internet, population, unemp, health_ins, poverty, SNAP, broad_num, broad_avail,broad_cost, all25_bbn)
scatterplotMatrix(Internetvars) # in CARS package

# Independent variable
# Histogram of non-transformed variables
HistogramFunction(Internet, population, "Population")  # right skewed
HistogramFunction(Internet, unemp, "unemp")           
HistogramFunction(Internet, health_ins, "health_insurance") 
HistogramFunction(Internet, poverty, "poverty")      
HistogramFunction(Internet, SNAP, "SNAP")
HistogramFunction(Internet, broad_num, "broad_num")     
HistogramFunction(Internet, broad_avail, "broad_avail") 
HistogramFunction(Internet, broad_cost, "broad_cost")   
HistogramFunction(Internet, all25_bbn, "all25_bbn")

# Box-Cox transformation
BoxCoxFunction(Internet, population)   

# Add transformed variables
lpop <- log(population)
health <- health_ins
Bnum <- broad_num
Bavail <- broad_avail
Bcost <- broad_cost
b25 <- all25_bbn

# Histogram of transformed variables 
HistogramFunction(Internet, lpop, "log-population")
QQplotFunction(Internet, lpop)

# Relation between response variable with predictors and transformed predictors
ScatterPlotFunction(Internet,lpop,"Log Population") 
lines(c(9.8,9.8),c(0,6), col="grey", lwd=3)
# lpop > 9.8
ScatterPlotFunction(Internet,unemp,"Unemployment")       
ScatterPlotFunction(Internet,health,"No Health Insurance") 
ScatterPlotFunction(Internet,poverty,"Poverty")         
ScatterPlotFunction(Internet,SNAP,"Receiving Food Stamps")               
ScatterPlotFunction(Internet,Bnum,"Number of Broadband Service Provider") 
ScatterPlotFunction(Internet,Bavail,"Broadband Availability")
ScatterPlotFunction(Internet,Bcost,"Broadband Cost")     
lines(c(130,130),c(0,6), col="grey", lwd=3)
# Bcost > 130
ScatterPlotFunction(Internet,b25,"Number of Internet Service Provider with 25Mbps")

#Correlation
TInternet <- data.frame(linternet,lpop, unemp, health, poverty, SNAP, Bnum, Bavail, Bcost, b25)
CorInternet <- cor(TInternet)
corrplot(CorInternet)    # Multicollinearity issues between SNAP, poverty, and unemp
corrplot(CorInternet, method = "number")


##### Model Development #####

# split data into train and test set
set.seed(123)
training.samples <- Internet$no_internet %>%
  createDataPartition(p=0.8, list=F)
train <- Internet[training.samples, ] # 2402
test <- Internet[-training.samples, ] # 598

detach(Internet)
attach(train)

train %<>%
  mutate(linternet=log(no_internet),
         lpop=log(population),
         health=health_ins,
         Bavail=broad_avail,
         Bnum=broad_num,
         Bcost=broad_cost,
         b25=all25_bbn)

# Regression model/ Model selection
fit <- lm(linternet~lpop+unemp+health+poverty+SNAP+Bnum+Bavail+Bcost+b25, data=train)
summary(fit1)
stepAIC(fit1, direction = "both")
# Suggest removal of unemp, Bnum, and b25

fit1 <- lm(linternet~lpop+health+poverty+SNAP+Bavail+Bcost, data=train)
summary(fit1)
stepAIC(fit1, direction = "both")
# Suggest removal of Bcost

fit2 <- lm(linternet~lpop+health+poverty+SNAP+Bavail, data=train)
summary(fit2)
stepAIC(fit2, direction = "both")

# Interaction
varsinteraction <- data.frame(linternet,lpop,unemp,health,poverty,SNAP,Bnum,Bavail,Bcost,b25)
fit33 <- lm(linternet~.*., data=varsinteraction, subset=train)  # .*. means everyhing include all interaction
fitint = stepAIC(fit33)
summary(fitint)
extractAIC(fitint)


fit3 <- lm( linternet ~ lpop+health+poverty+SNAP+Bavail+
              lpop:health+lpop:poverty+lpop:SNAP+lpop:Bavail+
              health:poverty+health:SNAP+health:Bavail+
              poverty:SNAP+poverty:Bavail, data=train )
summary(fit3)
stepAIC(fit3, direction = "both")
# Health and Bavail are not right direction.

fit4 <- lm(linternet ~ lpop + health + poverty + SNAP + Bavail + 
           lpop:health + lpop:poverty + lpop:SNAP + health:poverty + 
           health:SNAP + health:Bavail + poverty:SNAP, data = train)
summary(fit4)
stepAIC(fit4, direction = "both")
# Health is still not right direction.

# Generate iteratin log
fit_summary <- ValidationTable(fit, "Full Model")
fit1_summary <- ValidationTable(fit1, "Stepwise Model")
fit2_summary <- ValidationTable(fit2, "Stepwise Model with adjust")
fit3_summary <- ValidationTable(fit4, "Stepwise Model with Interaction Terms I")
fit4_summary <- ValidationTable(fit4, "Stepwise Model with Interaction Terms II")

bind_rows(fit_summary, fit1_summary, fit2_summary, fit3_summary, fit4_summary) %>%
  modify_if(is.numeric, round,3)

# Final Model
fit_final <- fit2

summary(fit_final)$coefficient
confint(fit_final)

# Multicollinearity
car::vif(fit_final)  # no multicollinearity issue ( all VIF values are less than 4)

MSE = mean(fit_final$residuals^2)
MSE  # 0.04529894

# RSE error rate
error_rate<-sigma(fit_final)/mean(train$linternet) # sigma=RSE
error_rate #0.06629226
# RSE is 0.2128 corresponding to 6.6% error rate.

fit_final %>%
  tidy() %>%
  modify_if(is.numeric, round,3)

##### Model Diagnostics #####

# Diagnostic plots
par(mfrow=c(1,2))
plot(predict(fit_final), rstudent(fit_final), main="Residual vs Fitted", ylab="Studentized Residuals", xlab="Predicted")
predict(fit_final)[rstudent(fit_final)==min(rstudent(fit_final))]
abline(h=0, lty=1, col="red")
qqPlot(fit_final, "Normal QQplot", ylab="Studentized Residuals", xlab="Theoretical quantiles")


model.diag <- augment(fit_final) #broom package
head(model.diag, 10)

# Check VIF of final model
vif(fit_final)

# Cook's D
cutoff <- 4/((nrow(train)-length(fit_final$coefficients)-2))
plot(fit_final, which=4, cook.levels=cutoff) # Outliers: 3, 1814, 1843

# Influential Observations
#influence plot: studentized residuals vs hat matrix diagnosis(leverage) with bubbles a function of Cook's D
influencePlot(fit_final, id.method="identify", main="Influence Plot", sub="Circle size is proportional to Cook's D")
# Influence points: 3, 10, 43, 132, 1814, 188

# Outliers
train[3,]
train[10,]
train[43,]
train[132,]
train[1814,]
train[1843,]

adjtrain <- train[-c(3,10,43,132,188,1814,1843),]
fit2_final <- lm(linternet~lpop+health+poverty+SNAP+Bavail, data=adjtrain)
plot(fit2_final)

#10 fold cross validation
mod1 <- glm(linternet~lpop+health+poverty+SNAP+Bavail+Bcost, data=train)
cv.err <- cv.glm(train, mod1, K=10)  #boot package
cv.err$delta  
#0.04557628 0.04555303

detach(train)
attach(test)
names(test)

#Predict
test %<>%
  mutate(linternet=log(no_internet),
         lpop=log(population),
         health=health_ins,
         Bavail=broad_avail,
         Bnum=broad_num,
         Bcost=broad_cost,
         b25=all25_bbn)

test %<>% 
  mutate(internet_preds=predict(fit_final, newdata=.))

test %<>%
  mutate(internet_error=linternet-internet_preds)


predictions <- predict(fit_final, newdata=test)
data.frame(R2=R2(predictions, test$linternet),
           MSE=mse(predictions, test$linternet))

#   R2        MSE      
# 0.6453539 0.04724417 


# Prediction error rate
P_error_rate=rmse(predictions, test$linternet)/mean(test$linternet)
P_error_rate  #0.06779982
# Prediction error rate is 6.8%

