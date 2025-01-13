setwd("/Users/epainter/Desktop/STA 567/Final Project")

# Libraries
library(dplyr)
library(magrittr)
library(ggfortify)
library(corrplot)
library(car)
library(tidyverse)
library(MASS) ## may need this for transformations later

# 1.) Loading Data
data <- read.csv("clinton92.csv", header=TRUE)

# 2.) Setting up data
pa_data <- data %>%
  filter(grepl(", PA", Name)) #%>%
  #dplyr::select(-Name)

colnames(pa_data) <- c("name", "pvote", "ma", "ms", 
                       "pci", "pp", "pv", 
                       "pf","pd", "pnh", "ci")

rownames(pa_data) <- NULL

# 3.) EDA
# Summary statistics
summary(pa_data)

par(mfrow=c(3,3))
# Create histograms for each variable
hist(pa_data$pvote, main = "% Vote", xlab = "y", col = 'black')
hist(pa_data$ma, main = "Median Age", xlab = "x", col = "lightgreen")
hist(pa_data$ms, main = "Mean Savings", xlab = "x", col = "lightcoral")
hist(pa_data$pci, main = "PC Income", xlab = "x", col = "lightpink")
hist(pa_data$pp, main = "% Poverty", xlab = "x", col = "lightyellow")
hist(pa_data$pv, main = "% Veteran", xlab = "x", col = "lightgray")
hist(pa_data$pf, main = "% Female", xlab = "x", col = "lightcyan")
hist(pa_data$pd, main = "Pop. Density", xlab = "x", col = "lightsteelblue")
hist(pa_data$pnh, main = "% Nursing Home", xlab = "x", col = "lightgoldenrod")
hist(pa_data$ci, main = "Crime Index", xlab = "x", col = "lightblue")

# Matrix plot
pairs(pa_data[,!(names(pa_data) %in% 'name')], pch=19, bg="dimgray", cex.labels=1, lower.panel = NULL)

# Correlation Matrix
cor(pa_data[,!(names(pa_data) %in% 'name')])

par(mfrow=c(1,1))
# Identifying outliers
plot(pa_data$ci, pa_data$pvote)
identify(pa_data$ci, pa_data$pvote)

##################################### INITIAL CLEANING #####################################
pa_data_c <- pa_data[-c(14,12,31,57),] # Drop 4 observations
rownames(pa_data_c) <- 1:nrow(pa_data_c)
pairs(pa_data_c[,!(names(pa_data_c) %in% 'name')], pch=19, bg="dimgray", cex.labels=1, lower.panel = NULL)

cor(pa_data_c[,!(names(pa_data_c) %in% 'name')])

# TRANSFORMATIONS
# Want to use boxcox transformations to find transformations that increase correlation with response
# If lambda = 0, then use log transformation
library(MASS)

# Median Age
lm1 = lm(ma ~ pvote ,data = pa_data_c)
mtr = boxCox(lm1, lambda = seq(-2, 2, 1/10), plotit = TRUE)

lam = mtr$x[which(mtr$y==max(mtr$y))]
lam

pa_data_c$ma_new <- (pa_data_c$ma^lam-1)/lam

plot(pa_data_c$ma, pa_data_c$pvote)
plot(pa_data_c$ma_new, pa_data_c$pvote)

cor(pa_data_c$ma, pa_data_c$pvote)
cor(pa_data_c$ma_new, pa_data_c$pvote) # no change

# Mean Savings
lm2 = lm(ms ~ pvote ,data = pa_data_c)
mtr2 = boxCox(lm2, lambda = seq(-2, 2, 1/10), plotit = TRUE)

lam2 = mtr2$x[which(mtr2$y==max(mtr2$y))]
lam2

pa_data_c$ms_new <- (pa_data_c$ms^lam2-1)/lam2

par(mfrow=c(1,2))
plot(pa_data_c$ms, pa_data_c$pvote)
plot(pa_data_c$ms_new, pa_data_c$pvote)

cor(pa_data_c$ms, pa_data_c$pvote)
cor(pa_data_c$ms_new, pa_data_c$pvote) # 8% worse

# Pop. Density
lm3 = lm(pd ~ pvote ,data = pa_data_c)
mtr3 = boxCox(lm3, lambda = seq(-2, 2, 1/10), plotit = TRUE)

lam3 = mtr3$x[which(mtr3$y==max(mtr3$y))]
lam3

pa_data_c$pd_new <- log(pa_data_c$pd)

par(mfrow=c(1,2))
plot(pa_data_c$pd, pa_data_c$pvote)
plot(pa_data_c$pd_new, pa_data_c$pvote)

cor(pa_data_c$pd, pa_data_c$pvote)
cor(pa_data_c$pd_new, pa_data_c$pvote) # 1% better (keep)

# PC Income
lm4 = lm(pci ~ pvote ,data = pa_data_c)
mtr4 = boxCox(lm4, lambda = seq(-2, 2, 1/10), plotit = TRUE)

lam4 = mtr4$x[which(mtr4$y==max(mtr4$y))]
lam4

pa_data_c$pci_new <-  (pa_data_c$pci^lam4-1)/lam4

plot(pa_data_c$pci, pa_data_c$pvote)
plot(pa_data_c$pci_new, pa_data_c$pvote)

cor(pa_data_c$pci, pa_data_c$pvote)
cor(pa_data_c$pci_new, pa_data_c$pvote) # no change

# Female
lm5 = lm(pf ~ pvote ,data = pa_data_c)
mtr5 = boxCox(lm5, lambda = seq(-2, 2, 1/10), plotit = TRUE)

lam5 = mtr5$x[which(mtr5$y==max(mtr5$y))]
lam5

pa_data_c$pf_new <-  (pa_data_c$pf^lam5-1)/lam5

plot(pa_data_c$pf, pa_data_c$pvote)
plot(pa_data_c$pf_new, pa_data_c$pvote)

cor(pa_data_c$pf, pa_data_c$pvote)
cor(pa_data_c$pf_new, pa_data_c$pvote) # no change

# Nursing Home
lm6 = lm(pnh ~ pvote ,data = pa_data_c)
mtr6 = boxCox(lm6, lambda = seq(-2, 2, 1/10), plotit = TRUE)

lam6 = mtr6$x[which(mtr6$y==max(mtr6$y))]
lam6

pa_data_c$pnh_new <-  (pa_data_c$pnh^lam6-1)/lam6

plot(pa_data_c$pnh, pa_data_c$pvote)
plot(pa_data_c$pnh_new, pa_data_c$pvote)

cor(pa_data_c$pnh, pa_data_c$pvote)
cor(pa_data_c$pnh_new, pa_data_c$pvote) # no change

# Crime Index
lm7 = lm(ci ~ pvote ,data = pa_data_c)
mtr7 = boxCox(lm7, lambda = seq(-2, 2, 1/10), plotit = TRUE)

lam7 = mtr7$x[which(mtr7$y==max(mtr7$y))]
lam7

pa_data_c$ci_new <-  log(pa_data_c$ci)

plot(pa_data_c$ci, pa_data_c$pvote)
plot(pa_data_c$ci_new, pa_data_c$pvote)

cor(pa_data_c$ci, pa_data_c$pvote)
cor(pa_data_c$ci_new, pa_data_c$pvote) # worse

# Dropping columns
pa_data_c <- pa_data_c %>%
  dplyr::select(c(pvote, ma, ms, pci, pp, pv, pf, pd_new, pnh, ci))

# Correlation matrix
cor(pa_data_c)

#################################### INITIAL MODEL ####################################
m1 = lm(pvote ~ ., data = pa_data_c)
summary(m1)
autoplot(m1, which = 1:6, ncol = 2, label.size = 3)

# Residuals vs each predictor
m1_r <- residuals(m1)
plot(pa_data_c$ms, m1_r) # Looks good for all predictors (no transformation needed at this stage) 
plot(pa_data_c$pvote, m1_r)

# Making a log transformation to Y, there is a curved shape in residuals vs fitted
pa_data_c$pvote <- log(pa_data_c$pvote)

m2 = lm(pvote ~ ., data = pa_data_c)
summary(m2)
autoplot(m2, which = 1:6, ncol = 2, label.size = 3)

m2_r <- residuals(m2)
plot(pa_data_c$ci, m2_r) # Looks good for all predictors (no transformation needed at this stage) 
plot(pa_data_c$pvote, m2_r)

# Based on this analysis, we are missing a postively related variable to % vote
# Think it could be education level, race, etc.
###### SEARCING FOR BEST MODEL (1) ######
### Will be using Adjusted R^2 and BIC for model selection criterion
library(leaps)

# All Possible Subsets
regfit.full <- regsubsets(pvote ~., data = pa_data_c, nvmax=9)
reg.summary <- summary(regfit.full)

names(reg.summary)

reg.summary$adjr2
reg.summary$bic

par(mfrow=c(1,2))
# Plotting Adj. R^2 and BIC
# R-squared
plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l") # 6 variables
# BIC
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type='l') # 4 variables

plot(regfit.full, scale='adjr2') # 69%
plot(regfit.full, scale='bic') # -56

# Forward, Backward, and Step-wise Selection
n1 = dim(pa_data_c)[1]

# Forward
fit0 <- lm(pvote ~ 1, data = pa_data_c)
fitall <- lm(pvote ~ ., data = pa_data_c)

# Forward
m1f <- step(fit0, fitall, direction = "forward", k = log(n1)) # use k = log(n) for BIC selection

# Backward
m1b <- step(fitall, direction = "backward", k = log(n1)) # 

# Both
m3s <- step(fitall, direction = "both", k = log(n1)) #

# Results
summary(m1f)$coefficients
summary(m1b)$coefficients
summary(m3s)$coefficients

# K - Cross Validation
library(DAAG)
fm1 <- "pvote ~ pp + pv + pd_new" # Step-wise selection (BIC)
fm2 <- "pvote ~ ma + ms + pp + pv + pf + pd_new + pnh" # Adj. R^2

fm1cv <- CVlm(pa_data_c, form.lm = formula(fm1), m = 5, seed = 7, plotit = F) # 0.017
fm2cv <- CVlm(pa_data_c, form.lm = formula(fm2), m = 5, seed = 7, plotit = F) # 0.019

fm1.fit <- lm(pvote ~ pp + pv + pd_new, data = pa_data_c) 
summary(fm1.fit) # SELECTED MODEL (66%)

fm2.fit <- lm(pvote ~ ma + ms + pp + pv + pf + pd_new + pnh, data = pa_data_c) 
summary(fm2.fit)

autoplot(fm1.fit, which = 1:6, ncol = 2, label.size = 3) 

#################################### SECOND MODEL ####################################
pa_data_c2 <- pa_data_c[-c(28),] # 10 observations total dropped
rownames(pa_data_c2) <- 1:nrow(pa_data_c2)

###### SEARCING FOR BEST MODEL (2) ######
### Will be using Adjusted R^2 and BIC for model selection criterion

# All Possible Subsets
regfit.full2 <- regsubsets(pvote ~., data = pa_data_c2, nvmax=9)
reg.summary2 <- summary(regfit.full2)

names(reg.summary2)

reg.summary2$adjr2
reg.summary2$bic

par(mfrow=c(1,2))
# Plotting Adj. R^2 and BIC
# R-squared
plot(reg.summary2$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l") # 5 variables
# BIC
plot(reg.summary2$bic ,xlab="Number of Variables ",ylab="BIC",type='l') # 4 variables

plot(regfit.full2, scale='adjr2') # 0.70
plot(regfit.full2, scale='bic') # -59

# Forward, Backward, and Step-wise Selection
n2 = dim(pa_data_c2)[1]

# Forward
fit0 <- lm(pvote ~ 1, data = pa_data_c2)
fitall <- lm(pvote ~ ., data = pa_data_c2)

# Forward
m1f2 <- step(fit0, fitall, direction = "forward", k = log(n2)) # use k = log(n) for BIC selection

# Backward
m1b2 <- step(fitall, direction = "backward", k = log(n2)) # 

# Both
m3s2 <- step(fitall, direction = "both", k = log(n2)) #

# Results
summary(m1f2)$coefficients
summary(m1b2)$coefficients
summary(m3s2)$coefficients

# K - Cross Validation
library(DAAG)
fm1.2 <- "pvote ~ pp + pv + pd_new" # Step-wise selection (BIC)
fm2.2 <- "pvote ~ ma + ms + pp + pv + pd_new + pnh" # Adj. R^2

fm1cv2 <- CVlm(pa_data_c2, form.lm = formula(fm1.2), m = 5, seed = 7, plotit = F) # 0.017
fm2cv2 <- CVlm(pa_data_c2, form.lm = formula(fm2.2), m = 5, seed = 7, plotit = F) # 0.018

fm1.fit2 <- lm(pvote ~ pp + pv + pd_new, data = pa_data_c2) 
summary(fm1.fit2) # SELECTED MODEL (68%)

fm2.fit2 <- lm(pvote ~ ma + ms + pp + pv + pd_new + pnh, data = pa_data_c2) 
summary(fm2.fit2) (#69%)

autoplot(fm1.fit2, which = 1:6, ncol = 2, label.size = 3)

m3_r <- residuals(fm1.fit2)
plot(pa_data_c2$pvote, m3_r) # missing linear related model in data collection

############################## MULTICOLLINEARITY DIAGNOSTICS #############################
# Will just do this for the 3rd iteration of the model
library(olsrr)
cor(pa_data_c2$pp, pa_data_c2$pvote) # 0.41
cor(pa_data_c2$pv, pa_data_c2$pvote) # 0.67
cor(pa_data_c2$pd_new, pa_data_c2$pvote) # 0.51

ols_coll_diag(fm1.fit2) 
# No evidence of SEVERE multicollinearity (NO VIF > 10 & all tolerances > 0.1)
# looks like we have 2 sets of multicollinear data: (pp, pd_new), (pp, pv, pd_new)

################### Principal Component Analysis ###########################
# PCA serves as a way to deal with multicollinearity
# Creates components that are orthogonal, which means no multicollinearity between predictors
library(pls)

pcr_model <- pcr(pvote ~ ., data = pa_data_c2, 
                 scale = T, center=TRUE, validation = "CV")

summary(pcr_model) # Variance explained by each pc

library(factoextra)
pcr_model2 <- prcomp(pa_data_c4, scale = TRUE)
print(pcr_model2)
summary(pcr_model2)
eig.val <- get_eigenvalue(pcr_model2)
eig.val
fviz_eig(pcr_model2, col.var="blue")

# Determining number of PC's to keep in the model (Threshold at 90% of variation explained)
par(mfrow=c(1,3))
validationplot(pcr_model)
validationplot(pcr_model, val.type="MSEP")
validationplot(pcr_model, val.type = "R2")

#### Looks like we want to keep 7 principal components
pcr_model_f <- pcr(pvote ~ ., data = pa_data_c2,
                   scale = TRUE, center = TRUE, ncomp = 5)

# Getting the model coefficients (have already been transformed back)
pca_coef <- coef(pcr_model_f, ncomp = 5, intercept = TRUE)
pca_coef

# Now we need to get results
# Fitted values
pcr_model_f_fitted <- pcr_model_f$fitted.values[,,5]
pcr_model_f_fitted

# Residuals
pcr_model_f_residuals <- pcr_model_f$residuals[,,5]
pcr_model_f_residuals

# Plotting fitted vs. actual
par(mfrow=c(1,1))
plot(pcr_model_f_fitted, pa_data_c2$pvote) # Looks good

# R^2 for this model
cor(pcr_model_f_fitted, pa_data_c2$pvote)^2 # No better when using pca (68%)

summary(pcr_model_f)

# Create diagnostic plots
par(mfrow=c(2,2))
# Residuals vs Fitted
plot(pcr_model_f_fitted, pcr_model_f_residuals,
     xlab="Fitted Values", ylab="Residuals",
     main="Residuals vs Fitted")
abline(h=0, col="red", lty=2) # Looks good

# Normal Q-Q plot
qqnorm(pcr_model_f_residuals)
qqline(pcr_model_f_residuals) # Looks good

# Scale-Location plot
plot(pcr_model_f_fitted, sqrt(abs(pcr_model_f_residuals)),
     xlab="Fitted Values", ylab="√|Standardized Residuals|",
     main="Scale-Location")

# Residuals vs Order (independence)
plot(pcr_model_f_residuals, type="b",
     xlab="Observation Order", ylab="Residuals",
     main="Residuals vs Order")

