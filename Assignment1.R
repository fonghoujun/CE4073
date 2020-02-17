# ======================
# CE4073 Assignment 1
# Fong Hou Jun U1721741L
# ======================



rmse <- function(error)
{
  sqrt(mean(error^2))
}




install.packages("dplyr")
library(dplyr)







navalDataPred <- read.csv("assign1_NavalPred.csv", header = TRUE)




# Loading the DataSet
navalData <- read.csv("assign1_NavalData.csv", header = TRUE)

# =======================================
# Basic exploration of the loaded dataset
# =======================================

dim(navalData)        # Dimensions of the loaded dataset
names(navalData)      # Labels of the columns in csv file
str(navalData)        # Structure of the loaded dataset
head(navalData)       # First 5 rows of the loaded dataset
tail(navalData)       # Last 5 rows of the loaded dataset 
summary(navalData)    # Summary statistics for all variables in loaded dataset

# ================================
# Advanced exploration of the data
# ================================

# Explore the correations between all 16 variables against Y1 and Y2
cor(navalData)
x <- c(1,2,3,4,5,6,7,8,10,11,13,14,15,16,17,18)
navalData_2 <- navalData[,x]
cor(navalData_2)
navalData_3 <- navalData_2[-9]
cor(navalData_3)
# As seen from the correlation table, we can make 2 observations.
# X6 and X7 are identical, so we can remove either of them.
# X9 and X12 are not correlated with the other variables as it shows NA value.
# Upon closer inspection, its values contain only 288 and 1 respectively.
# No matter how the other input variables change, the values of X9 and X12 will not
# affect them. Thus, we can remove these 2 as well.

# Shows a heatmap of the variables and how correlated they are to each other
library(corrplot)
library(RColorBrewer)
corrplot.mixed(cor(navalData_2), upper.col = brewer.pal(n=10, name ="Spectral"),
               lower.col = "blue", upper = "pie", number.cex = .75, number.digits = 3, na.label = "NA")

# In the heatmap, most of the variables are very closely correlated to each other, especially
# X1 and X2, as they are the darkest

# Plot out 2d scatterplots of all pairs of variables
pairs(navalData, pch = 19, col = "blue")

# As seen from the 2d scatterplot, we can see that there is a very strong linear pattern for 
# most of the variables.

# =======================================
# Basic linear modeling for predicting Y1
# =======================================

# Building the first model with all variables against Y1
lmFit_Y1 <- lm(Y1 ~ . - Y2, data = navalData)
summary(lmFit_Y1)
# From the summary of this first linear model, we can see that X7, X9 and X12 are NA to the model,
# and we will remove these variables from the model.

lmFit_Y1_V2 <- update(lmFit_Y1, ~ . - X7 - X9 - X12, data = navalData)
summary(lmFit_Y1_V2)
# After removing the 3 variables, the adjusted R-squared does not change. Thus, removing the 3 variables did not
# in any way affect the results.  From the graph, it seems that X4 is the variable 
# that will affect model the least, and so we will remove this as well.

lmFit_Y1_V3 <- update(lmFit_Y1_V2, ~ . - X4, data = navalData)
summary(lmFit_Y1_V3)
# Upon removing X4, X7, X9 and X12, the summary of the linear model shows that the remaining variables
# are all very significant variables which will affect the result of the prediction. From here,
# I have started to introduct non-linear variables as well.

# ==========================================
# Advanced linear modeling for predicting Y1
# ==========================================

# When looking at the correlation between Y1 and the variables, we can see that X10 has the highest correlation
# with Y1. Therefore, I introduced the square of X10 into the formula. I also introduced X8 and X15 as they are the 
# next 2 variables with the highest correlation.
lmFit_Y1_V4 <- update(lmFit_Y1_V3, ~ . + I(X10 ^ 2), data = navalData)
summary(lmFit_Y1_V4)

lmFit_Y1_V5 <- update(lmFit_Y1_V4, ~ . + I(X8^2), data = navalData)
summary(lmFit_Y1_V5)

lmFit_Y1_V6 <- update(lmFit_Y1_V5, ~ . + I(X15^2), data = navalData)
summary(lmFit_Y1_V6)
# After adding in the square of these 3 variables, the adjusted R-squared increased significantly, from 0.8396 to 0.9345

# From here, I decided to add all possible variations of the 3 variables. Namely, X8:X10, X8:X15 and X10:X15.
lmFit_Y1_V7 <- update(lmFit_Y1_V6, ~ . + X10:X8, data = navalData)
summary(lmFit_Y1_V7)

lmFit_Y1_V8 <- update(lmFit_Y1_V7, ~ . + X10:X15, data = navalData)
summary(lmFit_Y1_V8)

lmFit_Y1_V9 <- update(lmFit_Y1_V8, ~ . + X8:X15, data = navalData)
summary(lmFit_Y1_V9)
plot(lmFit_Y1_V9)

# After adding these variations, I get a total adjusted R-squared of 0.9434, from 0.8396. As i believe the model is good,
# I will proceed to make predictions now.


lmFit_Y1_V9$residuals                 # Residuals or prediction errors
sum(lmFit_Y1_V9$residuals^2)          # Sum of squares of errors
summary(lmFit_Y1_V9$residuals)        # Statistical summary of residuals

coef(summary(lmFit_Y1_V9))            # Standard errors of coefficients
confint(lmFit_Y1_V9)                  # Confidence intervals of coefficients
var(lmFit_Y1_V9$residuals)            # Variance of residuals

# Prediction of model without removing outliers from dataset
predict(lmFit_Y1_V9, newdata = navalData, interval = "confidence", level = 0.95)
predict(lmFit_Y1_V9, newdata = navalData, interval = "prediction", level = 0.95)


# Removing outliers from the dataset, using cooks distance of 2 standard deviation
cd <- cooks.distance(lmFit_Y1_V9)
navalData.clean <- navalData[abs(cd) < 4/nrow(navalData), ]
# To check the number of rows remaining, type in nrow(navalData.clean)

# Fitting the best model with new "outlier-free" data
lmFit_Y1_V10 <- lm(formula(lmFit_Y1_V9), data = navalData.clean)
summary(lmFit_Y1_V10)

lmFit_Y1_V10$residuals                # Residuals or prediction errors
sum(lmFit_Y1_V10$residuals^2)         # Sum of squares of errors
summary(lmFit_Y1_V10$residuals)       # Statistical summary of residuals

coef(summary(lmFit_Y1_V10))           # Standard errors of coefficients
confint(lmFit_Y1_V10)                 # Confidence intervals of coefficients
var(lmFit_Y1_V10$residuals)           # Variance of residuals

# Prediction of model with new dataset without outliers
predict(lmFit_Y1_V10, newdata = navalData.clean, interval = "confidence", level = 0.95)
predict(lmFit_Y1_V10, newdata = navalData.clean, interval = "prediction", level = 0.95)
# After removing the outliers, the vairance of residuals improved slightly.

predict(lmFit_Y1, newdata = navalData, interval = "confidence", level = 0.95)
predict(lmFit_Y1, newdata = navalData, interval = "prediction", level = 0.95)

Y1sum <- sum((lmFit_Y1_V10$residuals)^2)/nrow(navalData.clean)
Y1root <- sqrt(Y1sum)
Y1root


Y1sum2 <- sum((lmFit_Y1$residuals)^2)/nrow(navalData)
Y1root2 <- sqrt(Y1sum2)
Y1root2


# =======================================
# Basic linear modeling for predicting Y2
# =======================================

# Building the first model with all variables against Y2
lmFit_Y2 <- lm(Y2 ~ . - Y1, data = navalData)
summary(lmFit_Y2)
# From the summary of this first linear model, we can see that X7, X9 and X12 are NA to the model,
# and we will remove these variables from the model.

lmFit_Y2_V1 <- update(lmFit_Y2, ~ . - X7 - X9 - X12, data = navalData)
summary(lmFit_Y2_V1)
# After removing the 3 variables, the adjusted R-squared does not change. Thus, removing the 3 variables did not
# in any way affect the results.  From the graph, it seems that X2 is the variable 
# that will affect model the least, and so we will remove this as well.

lmFit_Y2_V2 <- update(lmFit_Y2_V1, ~ . - X2, data = navalData)
summary(lmFit_Y2_V2)

lmFit_Y2_V3 <- update(lmFit_Y2_V2, ~ . + I(X8^2), data = navalData)
summary(lmFit_Y2_V3)

lmFit_Y2_V4 <- update(lmFit_Y2_V3, ~ . + I(X15^2), data = navalData)
summary(lmFit_Y2_V4)

lmFit_Y2_V5 <- update(lmFit_Y2_V4, ~ . + I(X13^2), data = navalData)
summary(lmFit_Y2_V5)

lmFit_Y2_V6 <- update(lmFit_Y2_V5, ~ . + I(X16^2) - I(X8^2), data = navalData)
summary(lmFit_Y2_V6)

lmFit_Y2_V7 <- update(lmFit_Y2_V6, ~ . + X8:X15 + X8:X13 + X8:X16 + X15:X13 + X15:X16 + X13:X16, data = navalData)
summary(lmFit_Y2_V7)

lmFit_Y2_V8 <- update(lmFit_Y2_V7, ~ . - X16 - I(X8^2) - X8:X16 - X13:X16, data = navalData)
summary(lmFit_Y2_V8)
# Adjusted R-squared == 0.8289

lmFit_Y2_V5$residuals
sum(lmFit_Y2_V5$residuals^2)
summary(lmFit_Y2_V5$residuals)

coef(summary(lmFit_Y2_V5))
confint(lmFit_Y2_V5)
var(lmFit_Y2_V5$residuals)

predict(lmFit_Y2_V5, newdata = navalData, interval = "confidence", level = 0.95)
predict(lmFit_Y2_V5, newdata = navalData, interval = "prediction", level = 0.95)

cd <- cooks.distance(lmFit_Y2_V7)

navalData.cleanY2 <- navalData[abs(cd) < 4/nrow(navalData), ]
nrow(navalData.cleanY2)

lmFit_Y2_V8 <- lm(formula(lmFit_Y2_V7), data = navalData.cleanY2)
summary(lmFit_Y2_V8)

lmFit_Y2_V9 <- update(lmFit_Y2_V8, ~ . - X13:X16, data = navalData)
summary(lmFit_Y2_V9)

cd <- cooks.distance(lmFit_Y2_V9)

navalDataY2.clean <- navalData[abs(cd) < 4/nrow(navalData), ]

lmFitY2 <- lm(formula(lmFit_Y2_V9), data = navalDataY2.clean)
summary(lmFitY2)




coef(summary(lmFit_Y2_V6))
confint(lmFit_Y2_V6)
var(lmFit_Y2_V6$residuals)

predict(lmFit_Y2_V6, newdata = navalData, interval = "confidence", level = 0.95)
predict(lmFit_Y2_V6, newdata = navalData, interval = "prediction", level = 0.95)

Y1sum2 <- sum((lmFit_Y2_V9$residuals)^2)/nrow(navalDataY2.clean)
Y1root2 <- sqrt(Y1sum2)
Y1root2

Y1sum2 <- sum((lmFit_Y2$residuals)^2)/nrow(navalData)
Y1root2 <- sqrt(Y1sum2)
Y1root2
