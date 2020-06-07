Cars <- read.csv("Data Science Assignments/Multiple Linear regression/Cars.csv") # choose the Cars.csv data set
View(Cars)
attach(Cars)

# Exploratory Data Analysis(60% of time)
# 1. Measures of Central Tendency
# 2. Measures of Dispersion
# 3. Third Moment Business decision
# 4. Fourth Moment Business decision
# 5. Probability distributions of variables
# 6. Graphical representations
  #  > Histogram,Box plot,Dot plot,Stem & Leaf plot, 
  #     Bar plot

summary(Cars)

# 7. Find the correlation b/n Output (MPG) & (HP,VOL,SP)-Scatter plot
pairs(Cars)

# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
cor(Cars)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Cars))

# The Linear Model of interest with all the columns
model.car <- lm(MPG~VOL+HP+SP+WT)

summary(model.car)

# Multicollinearity check
# Model based on only Volume 
model.carV<-lm(MPG~VOL)
summary(model.carV) # Volume became significant

# Model based on only Weight
model.carW<-lm(MPG~WT)
summary(model.carW) # Weight became significant

# Model based on Volume and Weight
model.carVW<-lm(MPG~VOL+WT)
summary(model.carVW) # Both became Insignificant

### Scatter plot matrix along with Correlation Coefficients
panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
{
  usr<- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<- format(c(r,0.123456789),digits=digits)[1]
  txt<- paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex<-0.4/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
}
pairs(Cars,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")

# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(model.car)
library(car)
## plotting Influential measures 

influenceIndexPlot(model.car,id.n=3) # index plots for infuence measures
influencePlot(model.car,id.n=3) # A user friendly representation of the above

# Regression after deleting the 77th observation, which is influential observation
model_1<-lm(MPG~VOL+HP+SP+WT,data=Cars[-77,])
summary(model_1)
#Variance inflation Factors is a format way to check for collinearity 
vif(model.car) # VIF is > 10 =>collinearity
plot(VOL,WT, col="dodgerblue4",pch=20)
plot(HP,SP, col="dodgerblue4",pch=20)
layout(matrix(c(1,2,3,4),2,2))
plot(reg.model)
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.car,id.n=2,id.cex=0.7)

## VIF and AV plot has given us an indication to delete "wt" variable
finalmodel <- lm(MPG~VOL+HP+SP)
summary(finalmodel)

finalmodel<-lm(MPG~.-WT,data=Cars[-c(77,79),])
summary(finalmodel)

## Final model
plot(lm(MPG~.-WT,data=Cars[-c(77),])) # 77
summary(lm(MPG~.-WT,data=Cars[-c(77),]))
plot(lm(MPG~.-WT,data=Cars[-c(77,79),])) # 77,79
summary(lm(MPG~.-WT,data=Cars[-c(77,79),]))
plot(lm(MPG~.-WT,data=Cars[-c(77,79,80),])) # 77,79,80
summary(lm(MPG~.-WT,data=Cars[-c(77,79,80),]))

# Evaluate model LINE assumptions
plot(finalmodel)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance
qqPlot(finalmodel, id.n=5) # QQ plots of studentized residuals, helps identify outliers

hist(residuals(finalmodel)) # close to normal distribution

### TASK ####### 
## Apply different transformations on input check if they
# are they giving better Adjusted R_Squared and 
# less RMSE also check different plots which we used them
# for evaluating our model

## Apply Ridge and Lasso Regression on this data
# and check whether it is giving better Adjst.R_sqrd 
# and less rmse values are not 

