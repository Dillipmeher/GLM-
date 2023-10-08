#Simple Linear Regression Model(On Health care data)
# it is prediction problem
# Question1 :- Can we predict cost if i know age of individual
# Question 2:- How much extra cost will be incurred for a person whose age is say 
#  10years more than another person
#Question3:- Does cost statically depend on age ?
a <- read.csv("D:/data_set/Healthcare.csv")
View
a=a[,-1]
a
a=a[order(a[,1],decreasing = F),]
a
plot(a[,1],a[,2])
plot(a[,1],a[,2],xlab="Age",ylab="Cost")
# y= β0+ β1x1 + Error

b=lm(Cost.of.Treatment~AGE,data=a)
summary(b)

#Expected_Cost= β0+ β1*Age
y_hat=predict(b)
y_hat

# to join two data point by a straight line.

n=nrow(a)
segments(a[1,1],y_hat[1],a[n,1],y_hat[n],col="red")


#Identifying/Plotting the error
for(i in 1:n)
{
 segments(a[i,1],a[i,2],a[i,1],y_hat[i])
}

#Calculate Confidence Intervals
confint(b,level = 0.95)

confint(b,level = 0.90)

#Estimated cost? (for Age=72)

new_data=data.frame(AGE=72)
predict(b,newdata=new_data)
#Prediction Interval ?
predict(b,newdata=new_data,interval = "prediction", level=0.95)

#mean of the observed cost
mean_observed_y=mean(a[,2])
mean_observed_y

#mean of the estimated cost
mean_y_hat=mean(y_hat)
mean_y_hat

#R² value calculation
#R_sq=var(y_hat)/var(y)
R_sq=var(y_hat)/var(a[,2])
R_sq

#Correlation Coefficient between age and observed cost
r_x_y=(cov(a[,1],a[,2]))^2/(var(a[,1])*var(a[,2]))
r_x_y

 #Correlation coefficient with x and y_hat (Observed and estimated cost)
r_y_y_hat=cov(a[,2],y_hat)/(sqrt(var(a[,2]))*sqrt(var(y_hat)))
(r_y_y_hat)^2
y_hat


#Multiple linear Regression Model
# TRP Data working
b <- read.csv("D:/data_set/TRP_data Multiple leniar Regression.csv")
b
b=b[,-1]
b

c=lm(R~CTRP+P,data=b)
summary(c)

#----------------------------------------------------------------------

#Fit a regression model of R on CTRP        R²= 31.81 %
#Fit a regression model of R on CTRP+P      R²= 83.19 %

#Fit a regression model of R on P           R²= 32.37 %
#Fit a regression model of R on P+CTRP      R²= 83.19 %

#----------------------------------------------------------------------
#Fit a regression model of R on CTRP
b_1=lm(R~CTRP,data=b)
summary(b_1)


b_2=lm(R~P,data=b)
summary(b_2)


#######################################################################################################
#Multicollinearity
#We fit x1 on X2
# mean fit CTRP on P
b_3=lm(CTRP~P,data=b)
summary(b_3)                   #R2=0.05219

#calculation of estimated CTRP
estimated_CTRP=predict(b_3)
estimated_CTRP

b_4=lm(P~CTRP,data=b)
summary(b_4)                   #R2=0.05219

#checking the co-relation between x & y
x=b$CTRP
y=b$P
(cor(x,y))^2                  #R2=0.05219  


#checking corelation between TRP and Estimated TRP
(cor(x,estimated_CTRP))^2       #R2=0.05219             

################################################################

#Variance inflation Factor(VIF) =  1/(1-R²)
#VIF Calculations
1/(1-0.90)

install.packages('car',dependencies = T)

library(car)
library(carData)
vif(c)

############################################################################
# LM with categorical x variable.

x <- read.csv("D:/data_set/salary_dummy.csv")
x


y=lm(Salary~WE.in.Year+z1+z2+z3+z4,data=x)
summary(y)

y_drop4=lm(Salary~WE.in.Year+z1+z2+z3,data=x)
summary(y_drop4)

y_drop3=lm(Salary~WE.in.Year+z1+z2+z4,data=x)
summary(y_drop3)

y_drop2=lm(Salary~WE.in.Year+z1+z3+z4,data=x)
summary(y_drop2)

y_drop1=lm(Salary~WE.in.Year+z2+z2+z3,data=x)
summary(y_drop1)






