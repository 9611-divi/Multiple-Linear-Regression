cigarette_consumption <- read.csv("Data Science Assignments/Multiple Linear regression/CigaretteConsumption.csv",header=T)
attach(cigarette_consumption)

cg <- cigarette_consumption[,-1]

summary(cigarette_consumption[,2:8]) #[rows, columns]

pairs(cigarette_consumption[,2:8],col="dodgerblue4",pch=20)

cor(cigarette_consumption[,2:8])  # correlation matrix

reg.model <- lm(Sales~Age+HS+Income+Black+Female+Price)
summary(reg.model)
layout(matrix(c(1,2,3,4),2,2))
plot(reg.model)
confint(reg.model,level=0.95)

reg.reduced<-lm(Sales~Age+Income+Black+Price)
summary(reg.model)
summary(reg.reduced)
anova(reg.model)
anova(reg.reduced)

f=((34960-34926)/(6-4))/(34926/(51-6-1))
pf(f,2,(51-6-1))
