# ---------------------------------------------
# Skill Craft Dataset
# Group 88
# Elena Gillis: emg3sc
# Kanika Dawar: kd2hr
# Karan Gadiya: khg8mh
# Varshini Sriram: vs4vx
# ---------------------------------------------

# ---------------------------------------------
# Data Cleaning and Exploration
# ---------------------------------------------

#Setting the working directory to the source file location
setwd("C:/Users/Kanika/Downloads/FALL 2018/STAT 6021/Stat Project/")

#Loading the required packages
library(readr)
library(dplyr)
library(tidyverse)
library(car)
library(perturb)
library(caret)
library(MASS)
library(car)
library(alr3)
library(glmnet)

#Loading the dataset
data = read_csv("skillcraft.csv")

#Data Cleaning and Exploration

#Getting the number of distinct/unique values in each of the columns in the train dataset
feat_uniq_values <- as.data.frame(sapply(data, n_distinct))
feat_uniq_values$column = row.names(feat_uniq_values)
names(feat_uniq_values) = c("uniqvalcount", "column")
row.names(feat_uniq_values) = NULL
feat_uniq_values = feat_uniq_values[,c(2,1)]

#Getting the number of missing values in each column
na_values = as.data.frame(sapply(data, function(x)sum(is.na(x))))
na_values$column = row.names(na_values)
names(na_values) = c("missingvalcount", "column")
row.names(na_values) = NULL
na_values = na_values[,c(2,1)]
nrow(data[data$TotalHours == '?',])
#57 missing values in TotalHours
#Removing those rows
data = data[data$TotalHours != '?',]

#Converting variables to their proper form
data$TotalHours = as.numeric(data$TotalHours)

#Plotting the response variable and feature variables distribution
hist(data$TotalHours, breaks = 50)
#Skewed plot
#Plotting the response variable distribution
hist(log1p(data$TotalHours), xlab = 'Log of Total Hours',
     main='Distribution of Log Transformed Response Variable')
#Apprx normal

hist(data$Age, breaks = 50)
#Skewed
hist(data$APM, breaks = 50)
#Skewed
hist(data$HoursPerWeek, breaks = 50)
#Skewed
hist(data$SelectByHotkeys, breaks = 50)
#Skewed
hist(data$AssignToHotkeys, breaks = 50)
#Skewed
hist(data$UniqueHotkeys)
#Apprx normal
hist(data$MinimapAttacks, breaks = 100)
#Skewed
hist(data$MinimapRightClicks, breaks = 50)
#Skewed
hist(data$NumberOfPACs)
#Apprx normal
hist(data$GapBetweenPACs)
#Apprx okay
hist(data$ActionLatency)
#Apprx okay
hist(data$ActionsInPAC)
#Apprx okay
hist(data$TotalMapExplored)
#Apprx okay
hist(data$WorkersMade)
#Apprx okay
hist(data$UniqueUnitsMade)
#Apprx normal
hist(data$ComplexUnitsMade)
#Skewed
hist(data$ComplexAbilitiesUsed)
#Skewed

#Removing gameID
gameid = data$GameID
data$GameID = NULL

# ---------------------------------------------
# Outlier Analysis
# ---------------------------------------------

# HAT MATRIX
str(data)
X0.mat <- as.matrix(data[,-4])
n <- dim(data)[1]
X.mat <- cbind(rep(x=1, times=n), X0.mat)
X.mat.t <- t(X.mat)
colnames(X.mat)[1] <- "intercept"
p <- dim(X.mat)[[2]]

XpX.mat <- t(X.mat) %*% X.mat

XpX.inv <- solve(XpX.mat)

hat.matrix <- X.mat %*% XpX.inv %*% X.mat.t

hat <- cbind (diag(hat.matrix), diag(hat.matrix) > (2*p/n))

colnames(hat) <- c("hatvalue", "flag4" )
sum(hat[,2] == 1)
# 213 outliers

# ------------------------------------------------------------------------
# Cook's distance

model.lm = lm(TotalHours ~ ., data = data)
distance.inf <- cooks.distance(model.lm)

# The associated cutoff value for identifying an influential observation is 
cut.inf <- 1

# The results from comparing against the cutoff value are displayed as follows                 
cookD <-  cbind(distance.inf, distance.inf > cut.inf)
colnames(cookD) <- c("cookD", "flag1")
# 1793 is an outlier

# ------------------------------------------------------------------------
# DFFITS

# The values of DFFITS may be calculated using the       
# function "dffits":                                     

distance.inf <- dffits(model.lm)
head(distance.inf)

# The associated cutoff value for identifying an         
# influential observation is                             

p <- length(coefficients(model.lm))
cut.inf <- 2*sqrt(p/n)
cut.inf

# The results from comparing against the cutoff value    
# are displayed as follows                               

dffits <- cbind(distance.inf, distance.inf > cut.inf)

colnames(dffits) <- c("dffits", "flag2")
# 1793 is an outlier

# ------------------------------------------------------------------------
# COVRATIO

# The values of COVRATIO may be calculated using the     
# function "covratio":                                   

distance.inf <- covratio(model.lm)
head(distance.inf)

# The associated cutoff value for identifying an         
# influential observation is                             

n <- dim(data)[1]
p <- length(coefficients(model.lm))
cut.inf.lo <- 1 - 3*p/n
cut.inf.lo
cut.inf.hi <- 1 + 3*p/n
cut.inf.hi

# The results from comparing against the cutoff values   
# are displayed as follows                               

covratio = cbind(distance.inf, (distance.inf < cut.inf.lo) | (distance.inf > cut.inf.hi))

colnames(covratio) <- c("covratio", "flag3")
sum(covratio[,2] == 1)
#219 outliers

# ------------------------------------------------------------------------

inf <-  as.data.frame (cbind(cookD, dffits, covratio, hat ))

inf['flag'] <- inf$flag1+inf$flag2+inf$flag3+inf$flag4

# Removing all the observations where two or more statistics agree on outlier removal

outliers <- which(inf$flag >= 3)
outliers

#1793 is the consistent outlier

data = data[-outliers,]

# Box plot with removed outliers
boxplot(data$TotalHours, outline = F, horizontal = T,
        main='Box Plot of Response with Removed Outliers')

# ------------------------------------------------------
# Multi-collinearity Analysis
# ------------------------------------------------------

#Normalizing features
normalize = function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
data_norm <- data.frame(data$LeagueIndex, apply(data[, 2:19], 2, normalize))
colnames(data_norm)[1] <- "LeagueIndex"

#Pairwise correlations
correlations = cor(data_norm)
#SelectByHotkeys & APM = 0.8146242
#NumberOfPACs  & ActionLatency  = -0.8171618

#Variance inflation factors
model.lm = lm(TotalHours ~ ., data = data_norm)
as.matrix(vif(model.lm))
#APM 36.734348
#SelectByHotkeys 12.317681
#NumberOfPACs 13.814888

#Variance decomposition proportions
colldiag(model.lm, scale=TRUE, center=TRUE, add.intercept=FALSE)
#No multicollinearity detected

#Can remove the variable APM

# ------------------------------------------------------------------------
# PCA with function prcomp
# ------------------------------------------------------------------------

pca1 = prcomp(data[,c(-5,-4)], scale. = TRUE)

# sqrt of eigenvalues
pca1$sdev

# loadings
head(pca1$rotation)

# PCs (aka scores)
head(pca1$x)

# create data frame with scores
scores = as.data.frame(pca1$x)

# plot of principal component with y
plot(scores$PC1, log(data$TotalHours), ylim = c(0,10), 
     xlab='First Principle Component',
     ylab='Log of Total Hours',
     main='First Principle Component vs. Log of Total Hours', pch = 19, cex = .5)

# ------------------------------------------------------------------------
# Initial Residual Analysis
# ------------------------------------------------------------------------

model.lm = lm(TotalHours ~ .-APM, data = data)

# Student Residuals

qqnorm(rstudent(model.lm), datax=T, ylab="Residuals",
       xlab="Probability",
       main="Normal Probability Plot with x3")

qqline(rstudent(model.lm)) 

y.hat <- fitted(model.lm)

# plot of the residuals versus the predicted response
plot(y.hat, rstudent(model.lm), ylab="Residuals",
     xlab="Fitted Values",
     main="Residuals vs Predicted Response with x3")
lines(c(min(y.hat), max(y.hat)), c(0,0))

# ------------------------------------------------------------------------

# Studentized residuals

qqnorm(rstandard(model.lm), datax=T, ylab="Residuals",
       xlab="Probability",
       main="Normal Probability Plot with x3")

qqline(rstandard(model.lm)) 

y.hat <- fitted(model.lm)

# plot of the residuals versus the predicted response
plot(y.hat, rstandard(model.lm), ylab="Residuals",
     xlab="Fitted Values",
     main="Residuals vs Predicted Response with x3")
lines(c(min(y.hat), max(y.hat)), c(0,0))

# ------------------------------------------------------------------------
# TRANSFORMATIONS
# ------------------------------------------------------------------------

# Box cox procedure application

boxcox(TotalHours ~.-APM, data=data, lambda=seq(from=0, to=1, by=0.01))

# Box cox generates an alpha value of 0.19

y.trans <-  data$TotalHours^0.19

data <- cbind(data, y.trans)

# Adding 0.01 to everything so that boxtidwell is valid

data.trans = as.data.frame(sapply(3:19, function(i) sapply(1:nrow(data), function(j) data[j,i] = as.numeric(data[j,i]+0.01))))

data.trans = cbind(data[,1:2], data.trans, y.trans)

colnames(data.trans) <- colnames(data)
rownames(data.trans) <- 1:nrow(data.trans)

alpha.vect <- rep(x=1, times=ncol(data.trans)-2)

i = 1

for(column in colnames(data.trans)[c(-2,-4,-5,-20)]){
  alpha.vect <- boxTidwell(y=data.trans$TotalHours, x1= data.trans[column])$result[1]
  i = i+1
  data.trans[,column] <- data.trans^alpha.vect
}

# -------------------------------------------------------
# VARIABLE STEPWISE SELECTION
# -------------------------------------------------------

# Alpha in = 0.15
# Alpha out = 0.10

# Using Box Tidwell transformed variables
reg.names <- colnames(data[,c(-4,-5,-20)])
alpha.in <- 0.15
alpha.out <- 0.10
resp.name <- "TotalHours"
data.name <- "data.trans"
model.lm.sub2 <- stepwise.selection(alpha.in, alpha.out, resp.name, reg.names, data.name)
summary(model.lm.sub2)

# lm(formula = TotalHours ~ LeagueIndex, data = data.trans)
# Residual standard error: 823.1 on 3335 degrees of freedom
# Multiple R-squared:  0.07645,	Adjusted R-squared:  0.07617 
# F-statistic:   276 on 1 and 3335 DF,  p-value: < 2.2e-16

# Using Box Cox transformed variables
resp.name <- "y.trans"
reg.names <- colnames(data[,c(-4,-5,-20)])
data.name <- "data"
alpha.in <- 0.15
alpha.out <- 0.10

model.lm.sub <- stepwise.selection(alpha.in, alpha.out, resp.name, reg.names, data.name)
summary(model.lm.sub)

# lm(formula = y.trans ~ LeagueIndex + Age + HoursPerWeek + SelectByHotkeys + 
# AssignToHotkeys + UniqueHotkeys + ActionLatency + ActionsInPAC + 
#   UniqueUnitsMade, data = data)
# Residual standard error: 0.4362 on 3327 degrees of freedom
# Multiple R-squared:  0.3448,	Adjusted R-squared:  0.343 
# F-statistic: 194.5 on 9 and 3327 DF,  p-value: < 2.2e-16

# In here, we see that Box Cox response transformation yeilds much better results compared to Box Tidwell
# We decide to go ahead with Box Cox procedure

# ------------------------------------------------------------------------
# FINAL MODEL
# ------------------------------------------------------------------------

data$LeagueIndex <- factor(data$LeagueIndex, ordered = TRUE)
model.lm = lm(y.trans ~ LeagueIndex + Age + HoursPerWeek + SelectByHotkeys +
                AssignToHotkeys + UniqueHotkeys + ActionLatency + ActionsInPAC +
                UniqueUnitsMade, data = data)

summary(model.lm)

anova(model.lm)

# Student Residuals

qqnorm(rstudent(model.lm), datax=T, ylab="Residuals",
       xlab="Probability",
       main="Normal Probability Plot with x3")

qqline(rstudent(model.lm)) 

y.hat <- fitted(model.lm)

# plot of the residuals versus the predicted response
plot(y.hat, rstudent(model.lm), ylab="Residuals",
     xlab="Fitted Values",
     main="Residuals vs Predicted Response with x3")
lines(c(min(y.hat), max(y.hat)), c(0,0))

# ------------------------------------------------------------------------
# RESIDUAL PLOTS
# ------------------------------------------------------------------------

# Studentized residuals

qqnorm(rstandard(model.lm), datax=T, ylab="Residuals",
       xlab="Probability",
       main="Normal Probability Plot with x3")

qqline(rstandard(model.lm)) 

y.hat <- fitted(model.lm)

# plot of the residuals versus the predicted response
plot(y.hat, rstandard(model.lm), ylab="Residuals",
     xlab="Fitted Values",
     main="Residuals vs Predicted Response with x3")
lines(c(min(y.hat), max(y.hat)), c(0,0))

# Here we see a drastic imporvement in the normality assumption and the plot looks good
# We also see that the residuals are almost uniformly distributed

# ------------------------------------------------------------------------
pairs(data[,c('Age', 'HoursPerWeek', 'SelectByHotkeys', 'AssignToHotkeys', 'UniqueHotkeys', 
              'ActionLatency', 'ActionsInPAC', 'UniqueUnitsMade', 'y.trans')])
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Hypothesis Testing 
# ------------------------------------------------------------------------

#Test for significance of Regression

alpha = 0.05
k = 14
#Calculating the F-critical value
F.crit = qf(alpha, df1=k, df2=n-k-1, lower.tail=FALSE)
F.crit

summary(model.lm)

#[1] 1.694746
#F-statistic: 126, p-value: < 2.2e-16
#Null Hypothesis is rejected since F > F.crit and 
#Regression is significant
#All individual regressors are also significant (F > F.crit)

anova(model.lm)

# ------------------------------------------------------------------------
# 95% CI on Coefficients of Regression
# ------------------------------------------------------------------------

summary.model <- summary(model.lm)

coefficients <- as.matrix(summary.model$coefficients[,1])
colnames(coefficients) <- "Coefficients"

std.err <- as.matrix(summary.model$coefficients[,2])
colnames(std.err)[1] <- "Error"

as.data.frame(cbind(coefficients-std.err, coefficients,  coefficients+std.err))

#                 Coeff.lwr     Coefficients  Coeff.upr
# (Intercept)      2.647506e+00  2.720128e+00  2.792751e+00
# LeagueIndex.L    6.998200e-01  7.558883e-01  8.119566e-01
# LeagueIndex.Q   -9.836887e-02 -5.206672e-02 -5.764578e-03
# LeagueIndex.C   -3.249431e-02  4.176597e-03  4.084750e-02
# LeagueIndex^4   -2.209763e-03  2.459867e-02  5.140710e-02
# LeagueIndex^5   -4.091501e-02 -2.055693e-02 -1.988538e-04
# LeagueIndex^6    1.592876e-02  3.276105e-02  4.959333e-02
# Age              9.693426e-03  1.158092e-02  1.346842e-02
# HoursPerWeek     1.270547e-02  1.337343e-02  1.404139e-02
# SelectByHotkeys  6.349790e+00  8.271072e+00  1.019235e+01
# AssignToHotkeys -1.589989e+02 -1.134262e+02 -6.785340e+01
# UniqueHotkeys   -1.063948e-02 -6.949921e-03 -3.260360e-03
# ActionLatency   -2.413970e-03 -1.846995e-03 -1.280020e-03
# ActionsInPAC     1.695023e-02  2.216659e-02  2.738295e-02
# UniqueUnitsMade  7.818619e-03  1.213007e-02  1.644152e-02

# ------------------------------------------------------------------------
# SUM SQUARED ERRORS
# ------------------------------------------------------------------------

# predicting values on our dataset

y.hat <-  fitted(model.lm)

y.predict1 <- predict(model.lm, newdata=data, interval="confidence", level=0.95)

y.predict2 <- predict(model.lm, newdata=data, interval="prediction", level=0.95)

# Here we noticed that the prediction intervals are wider than confidence intervals
# Confidence interval tells us about the likely location of the true population parameter
# Prediction intervals must account for both the uncertainty in knowing the value of the population mean, plus data scatter
# So our observations are in line with the fact that a prediction interval is always wider than a confidence interval

# --------------------------------

df.R <- 14
n <- dim(data)[[1]]
df.Res <- n-(df.R + 1)

# Sum of squares - Residual

SS.Res <- sum((y.trans - y.hat)^2)

MS.Res <- SS.Res / df.Res

SS.Res
# [1] 631.0751

MS.Res
# [1] 0.1899112

# --------------------------------
# Sum of squares - Regression

y.bar <- mean(y.trans)
SS.R = sum((y.hat - y.bar)^2)

MS.R <- SS.R / df.R

SS.R
# [1] 335.0047

MS.R
# [1] 23.92891

# --------------------------------
# Sum of squares - Total

SS.T <- SS.Res + SS.R
SS.T
# [1] 966.0798

# ------------------------------------------------------------------------
# PREDICTIONS FOR NEW DATA
# ------------------------------------------------------------------------

li = c(3,4,5,2,1, 3, 4,5,6,3,4,5,6,3,4)
age = c(23,31,18,23,41, 34,32,32,23,24,25,14,13,15,16)
hpw = c(6, 20, 34, 23, 5, 30, 20, 40, 23, 25, 23, 24, 27, 30, 20)
sby = c(0.001, 0.0045, 0.003, 0.031, 0.02, 0.001, 0.0045, 0.003, 0.031, 0.02, 0.001, 0.0045, 0.003, 0.031, 0.02)
athk = c(0.0016, 0.0013, 0.0029, 0.0023, 0.0012, 0.0016, 0.0013, 0.0029, 0.0023, 0.0012, 0.0016, 0.0013, 0.0029, 0.0023, 0.0012)
uhk = c(5,6,7,5,4, 5,6,7,5,4, 5,6,7,5,4)
al = c(56, 84, 35, 163, 28, 46, 74, 35, 63, 28, 56, 84, 35, 163, 28)
aip = c(4.8, 4.5, 5.6,6.2, 4.8, 4.8, 4.5, 5.6,6.2, 4.8, 4.8, 4.5, 5.6,6.2, 4.8)
uum = c(4,5,6,3,4, 4,5,6,3,4, 4,5,6,3,4)

test = data.frame(li, age, hpw, sby, athk, uhk, al, aip, uum)
colnames(test) = c('LeagueIndex', 'Age', 'HoursPerWeek', 'SelectByHotkeys', 'AssignToHotkeys', 'UniqueHotkeys', 
                   'ActionLatency', 'ActionsInPAC', 'UniqueUnitsMade')


# Checking for interpolation, extrapolation, hidden extrapolation

data$LeagueIndex <- as.numeric(data$LeagueIndex)

X0.mat <- as.matrix(data[,c('LeagueIndex', 'Age', 'HoursPerWeek', 'SelectByHotkeys', 'AssignToHotkeys', 'UniqueHotkeys', 
                            'ActionLatency', 'ActionsInPAC', 'UniqueUnitsMade')])
n <- dim(data)[1]
X.mat <- cbind(rep(x=1, times=n), X0.mat)
X.mat.t <- t(X.mat)
colnames(X.mat)[1] <- "intercept"
p <- dim(X.mat)[[2]]

data$LeagueIndex <- factor(data$LeagueIndex, ordered = TRUE)

XpX.mat <- t(X.mat) %*% X.mat

XpX.inv <- solve(XpX.mat)

hat.matrix <- X.mat %*% XpX.inv %*% X.mat.t

# The the diagonal elements of the hat matrix are

h.val <- diag(hat.matrix)

# The maximum of these elements is

h.max <- max(h.val)

# The values h00 are calculated and compared to the
# maximum hat-diagonal value for predictions made at
# specified sets of regressor values

x0.vect <- as.matrix(cbind(rep(x=1, times=nrow(test)),test))
colnames(x0.vect)[1] <- "intercept"

h00 <- data.frame(h00 = NA, h.max = NA)

for (i in 1:dim(x0.vect)[1]){
  dummy <- as.matrix(x0.vect[i,])
  h00.dummy <- as.numeric(t(dummy) %*% XpX.inv %*% dummy)
  h00[i,] <- c(h00.dummy, h.max)
}

h00['flag'] <- as.numeric(h00$h00 > h00$h.max)

# In here we see that the 3,4,8,13, and 14 observations have hidden extrapolation
# Even thought the x values fall in the range of the given data
# Hat matrix suggests that the response variable might be out of bounds comapred to given y value
# Hence, the linear regression results might not be too reliable for these

# Predicting values for the new dataset with 95% interval

test$LeagueIndex <- factor(test$LeagueIndex, ordered = TRUE)

y.predict1.test <- predict(model.lm, newdata=test, interval="confidence", level=0.95)
y.predict2.test <- predict(model.lm, newdata=test, interval="prediction", level=0.95)

y.predict1.test

# CI
# fit      lwr      upr
# 1  2.815520 2.690162 2.940877
# 2  3.212986 3.112607 3.313365
# 3  3.327899 3.105885 3.549913
# 4  2.814307 2.584861 3.043753
# 5  2.917137 2.760174 3.074099
# 6  3.282342 3.143730 3.420954
# 7  3.243037 3.143639 3.342435
# 8  3.570272 3.341751 3.798794
# 9  3.584938 3.411598 3.758278
# 10 3.342382 3.234907 3.449858
# 11 3.173830 3.052205 3.295455
# 12 3.221663 3.121076 3.322250
# 13 3.283883 3.065168 3.502598
# 14 3.033843 2.802446 3.265240
# 15 3.290668 3.194039 3.387297

y.predict2.test

# PI
# fit      lwr      upr
# 1  2.815520 1.951805 3.679234
# 2  3.212986 2.352542 4.073430
# 3  3.327899 2.444962 4.210836
# 4  2.814307 1.929472 3.699142
# 5  2.917137 2.048273 3.786001
# 6  3.282342 2.416605 4.148079
# 7  3.243037 2.382707 4.103367
# 8  3.570272 2.685677 4.454868
# 9  3.584938 2.712966 4.456909
# 10 3.342382 2.481082 4.203683
# 11 3.173830 2.310649 4.037010
# 12 3.221663 2.361195 4.082132
# 13 3.283883 2.401769 4.165996
# 14 3.033843 2.148500 3.919186
# 15 3.290668 2.430653 4.150683

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------