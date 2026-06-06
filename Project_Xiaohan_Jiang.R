data <- read.csv("Student_Marks.csv")
data

#Correlation of variables
plot(data$time_study, data$Marks,
     xlab = "time_study", ylab = "Marks",
     main = "Scatterplot of time_study vs. Marks")
correlation <- cor(data$time_study, data$Marks)
cat("Correlation Coefficient:", correlation, "\n")

plot(data$number_courses, data$Marks, 
     xlab = "number_courses", ylab = "Marks", 
     main = "Scatterplot of number_courses vs. Marks")
correlation <- cor(data$number_courses, data$Marks)
cat("Correlation Coefficient:", correlation, "\n")

#multiple_regression_model
multiple_regression_model <- lm(Marks ~ time_study + number_courses , data = data)
summary(multiple_regression_model)

#Critical value of F test
qf(1-0.06, df1 = 2, df2 = 97)

# p value 
1-pf(764.8,df1=2, df2=97)

# Critical value of t test
qt(0.97,df=97)

#Multiple linear regression
m <- lm(data$Marks~data$number_courses+data$time_study)
m
m <- lm(Marks~number_courses+time_study, data=data)
m
coefficients(m) 
coef(m) 
summary(m)

anova(m)
confint(m, level=0.94)

resid(m) 
residuals(m) 
par(mfrow=c(2,2)) 
plot(fitted(m), resid(m), axes=TRUE, 
     frame.plot=TRUE, xlab='fitted values', 
     ylab='residue')
abline(a=0, b=0)
plot(data$number_courses, resid(m), axes=TRUE, frame.plot=TRUE, xlab='number_courses', ylab='residue')
plot(data$time_study, resid(m), axes=TRUE, frame.plot=TRUE, xlab='time_study', ylab='residue')
hist(resid(m))

# Residual plot of number_courses
par(mfrow=c(1,1)) 
data2 <- rbind(data[1:nrow(data),], c(41, 65, 600))
plot(data$number_courses, data$Marks)
data2 <- rbind(data[1:nrow(data),], c(75, 65, 500))
plot(data$number_courses, data$Marks)

m <- lm(Marks~number_courses, data=data)
coef(m)
coefficients(m)
influence.measures(m) #Identify influential observations
plot(data$number_courses, data$Marks)
abline(a=m$coefficients[1], b=m$coefficients[2])
plot(m, which=1) 

m1 <- lm(Marks~number_courses, data = data)
coefficients(m)
plot(data$number_courses, data$Marks)
abline(a=m$coefficients[1], b=m$coefficients[2], lty=3, col="red")
plot(m1, which=1)
influence.measures(m) 

# Resual plot of time_study 
par(mfrow=c(1,1)) 
data2 <- rbind(data[1:nrow(data),], c(41, 65, 600))
plot(data$time_study, data$Marks)
data2 <- rbind(data[1:nrow(data),], c(75, 65, 500))
plot(data$time_study, data$Marks)

m <- lm(Marks~time_study, data=data)
coef(m)
coefficients(m)
influence.measures(m) #Identify influential observations
plot(data$time_study, data$Marks)
abline(a=m$coefficients[1], b=m$coefficients[2])
plot(m, which=1) 

m1 <- lm(Marks~time_study, data = data)
coefficients(m)
plot(data$time_study, data$Marks)
abline(a=m$coefficients[1], b=m$coefficients[2], lty=3, col="red")
plot(m1, which=1)
influence.measures(m) 

# QQ plot of time_study VS Marks
regression_model <- lm(Marks ~ `time_study`, data = data)
resid(regression_model)# model residuals
residuals<-residuals(regression_model) # same as resid()
plot(data$Marks,residuals, xlab = "Marks",ylab ="Residuals", main = "Residual Plot for Simple Linear Regression")
qqnorm(residuals)
qqline(residuals)
plot(regression_model,whicle = c(1,3))
library(car)
influencePlot(regression_model)

#QQ plot of number_courses VS Marks
regression_model1 <- lm(Marks ~ `number_courses`, data = data)
resid(regression_model1)
residuals1<-residuals(regression_model1)
plot(data$Marks,residuals1, xlab = "Marks",ylab ="Residuals", main = "Residual Plot for Simple Linear Regression")
qqnorm(residuals1)
qqline(residuals1)
plot(regression_model1,whicle = c(1,3))
library(car)
influencePlot(regression_model1)



