setwd("G:/Data Science/Machine Learning/An Introduction to Statistical Learning/Practice")
library(MASS)
library(ISLR)
str(Boston)
View(Boston)
fix(Boston)
names(Boston)
lm.fit =lm(medv~lstat, data=Boston)
lm.fit
attach(Boston)
summary(lm.fit)
confint(lm.fit)
predict (lm.fit ,data.frame(lstat=c(5 ,10 ,15) ), interval ="confidence")
predict (lm.fit ,data.frame(lstat=c(5 ,10 ,15) ), interval ="prediction")
plot(lstat ,medv); abline(lm.fit, lwd = 3, col ="red ")
plot(lstat ,medv ,col ="red ")
plot(lstat ,medv ,pch =20)
plot(lstat ,medv ,pch ="+")
plot (1:20 ,1:20, pch =1:20)
par(mfrow =c(2,2))
plot(lm.fit)
plot(predict (lm.fit), residuals (lm.fit))
plot(predict (lm.fit), rstudent (lm.fit))
plot(hatvalues (lm.fit ))
which.max (hatvalues (lm.fit))
lm.fit =lm(medv~lstat+age ,data=Boston )
summary (lm.fit)
summary(lm.fit)$r.sq
summary(lm.fit)$sigma
summary(lm.fit)$df
lm.fit =lm(medv~.,data=Boston )
summary (lm.fit)
lm.fit1=lm(medv~.-age ,data=Boston )
summary (lm.fit1)
summary (lm(medv~lstat *age ,data=Boston ))
lm.fit2=lm(medv~lstat +I(lstat ^2))
summary (lm.fit2)
lm.fit =lm(medv~lstat)
anova(lm.fit ,lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
lm.fit5=lm(medv~poly(lstat ,5))
summary (lm.fit5)
summary (lm(medv~log(rm),data=Boston ))

#*************************************************

fix(Carseats)
names(Carseats)
lm.fit = lm(Sales~.+ Income :Advertising +Price :Age ,data=Carseats)
summary(lm.fit)
attach (Carseats)
contrasts(ShelveLoc)
ShelveLocGood
fix(Carseats)
#*************************************************

#Create Functions

LoadLibraries=function (){
  library (ISLR)
  library (MASS)
  print (" The libraries have been loaded .")
  }

LoadLibraries()
#*************************************************

lm.auto.fit = lm(mpg~horsepower, data = Auto)
summary(lm.auto.fit)
predict(lm.auto.fit, data.frame(horsepower=98), interval="confidence")
predict(lm.auto.fit, data.frame(horsepower=98), interval="prediction")

par(mfrow=c(1,1))
plot(Auto$horsepower,Auto$mpg)
abline(lm.auto.fit, lwd=3, col="red")
par(mfrow=c(2,2))
plot(lm.auto.fit)


lm.auto.fit2=lm(mpg~horsepower+I(horsepower^2), data=Auto)
hprange=seq(min(Auto$horsepower),max(Auto$horsepower))
plot(Auto$horsepower,Auto$mpg)
lines(hprange,predict(lm.auto.fit2,data.frame(horsepower=hprange)),col="red",lwd=3)
par(mfrow=c(2,2))
plot(lm.auto.fit2)

#***********************************************************

pairs(Auto)
cor(subset(Auto, select=-c(name)))
lm.auto.fit3 = lm(mpg~.-name, data=Auto)
summary(lm.auto.fit3)

par(mfrow=c(2,2))
plot(lm.auto.fit3)

#*****************************************************************
lm.car.fit = lm(Sales ~ Population + Urban + US, data=Carseats)
summary(lm.car.fit)

lm.car.fit2 = lm(Sales ~ US, data=Carseats)
summary(lm.car.fit2)
confint(lm.car.fit2)
par(mfrow=c(2,2))
plot(lm.car.fit2)
#**********************************************************************

set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)
lm.misc.fit=lm(y~x+0)
summary(lm.misc.fit)

lm.misc.fit1=lm(x~y+0)
summary(lm.misc.fit1)

lm.fit1=lm(y~x)
lm.fit2=lm(x~y)
t.1=summary(lm.fit1)$coefficients[2,3]
t.2=summary(lm.fit2)$coefficients[2,3]
round(t.1,4) == round(t.2,4)
#*********************************************************************

x=rnorm(100)
y=1*x
coefficients(lm(x~y+0))
coefficients(lm(y~x+0))
#************************************************************************

x=rnorm(100)
eps=rnorm(100,0,0.25)
y=-1+0.5*x+eps
plot(x,y)
abline(lm(y~x))

lm.fit=lm(y~x)
summary(lm.fit)
plot(x,y)
abline(lm(y~x),col="red",lwd=3)
legend("topleft",
       legend="Least Squares Fit",
       lty=1,
       lwd=3,
       col="red") # gives the legend lines the correct color and width

lm.fit2=lm(y~poly(x,2))
summary(lm.fit2)

#********************************************************************

set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)
cor(data.frame(y=y, x1=x1, x2=x2))
pairs(data.frame(y=y, x1=x1, x2=x2))
lm.fit=lm(y~x1+x2)
summary(lm.fit)
lm.fit2=lm(y~x1)
summary(lm.fit2)

#********************************************************************

x1=c(x1, 0.1)
x2=c(x2, 0.8)
y=c(y,6)
lm.fit=lm(y~x1+x2)
summary(lm.fit)
lm.fit2=lm(y~x1)
summary(lm.fit2)
lm.fit3=lm(y~x2)
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit, main="y~x1+x2")
par(mfrow=c(2,2))
plot(lm.fit2, main="y~x1")
par(mfrow=c(2,2))
plot(lm.fit3, main="y~x2")

#********************************************************************

fix(Boston)
lm.fit.zn=lm(crim~zn,data=Boston)
summary(lm.fit.zn)
lm.fit.indus=lm(crim~indus,data=Boston)
summary(lm.fit.indus)
lm.fit.chas=lm(crim~chas,data=Boston)
summary(lm.fit.chas)
lm.fit.nox=lm(crim~nox,data=Boston)
summary(lm.fit.nox)
lm.fit.rm=lm(crim~rm,data=Boston)
summary(lm.fit.rm)
lm.fit.age=lm(crim~age,data=Boston)
summary(lm.fit.age)
lm.fit.dis=lm(crim~dis,data=Boston)
summary(lm.fit.dis)
lm.fit.rad=lm(crim~rad,data=Boston)
summary(lm.fit.rad)
lm.fit.tax=lm(crim~tax,data=Boston)
summary(lm.fit.tax)
lm.fit.ptratio=lm(crim~ptratio,data=Boston)
summary(lm.fit.ptratio)
lm.fit.black=lm(crim~black,data=Boston)
summary(lm.fit.black)
lm.fit.lstat=lm(crim~lstat,data=Boston)
summary(lm.fit.lstat)
lm.fit.medv=lm(crim~medv,data=Boston)
summary(lm.fit.medv)

par(mfrow=c(3,4))
plot(Boston$zn, Boston$crim)
abline(lm.fit.zn, col="red",lwd=3)

plot(Boston$indus, Boston$crim)
abline(lm.fit.indus, col="red",lwd=3)

plot(Boston$nox, Boston$crim)
abline(lm.fit.nox, col="red",lwd=3)

plot(Boston$rm, Boston$crim)
abline(lm.fit.zn, col="red",lwd=3)

plot(Boston$age, Boston$crim)
abline(lm.fit.age, col="red",lwd=3)

plot(Boston$dis, Boston$crim)
abline(lm.fit.dis, col="red",lwd=3)

plot(Boston$rad, Boston$crim)
abline(lm.fit.rad, col="red",lwd=3)

plot(Boston$tax, Boston$crim)
abline(lm.fit.tax, col="red",lwd=3)

plot(Boston$ptratio, Boston$crim)
abline(lm.fit.ptratio, col="red",lwd=3)

plot(Boston$black, Boston$crim)
abline(lm.fit.black, col="red",lwd=3)

plot(Boston$lstat, Boston$crim)
abline(lm.fit.lstat, col="red",lwd=3)

plot(Boston$medv, Boston$crim)
abline(lm.fit.medv, col="red",lwd=3)

plot(Boston$chas, Boston$crim)
abline(lm.fit.chas, col="red",lwd=3)

lm.fit.all=lm(crim~.,data=Boston)
summary(lm.fit.all)

uni.coef <- c(coefficients(lm.fit.zn)[2],
              coefficients(lm.fit.indus)[2],
              coefficients(lm.fit.chas)[2],
              coefficients(lm.fit.nox)[2],
              coefficients(lm.fit.rm)[2],
              coefficients(lm.fit.age)[2],
              coefficients(lm.fit.dis)[2],
              coefficients(lm.fit.rad)[2],
              coefficients(lm.fit.tax)[2],
              coefficients(lm.fit.ptratio)[2],
              coefficients(lm.fit.black)[2],
              coefficients(lm.fit.lstat)[2],
              coefficients(lm.fit.medv)[2])

multi.coef <- coefficients(lm.fit.all)[-1] #discard intercept

par(mfrow=c(1,1))
plot(uni.coef,multi.coef)
