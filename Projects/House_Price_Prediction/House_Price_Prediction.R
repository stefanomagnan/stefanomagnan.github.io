#Kaggle in-class competition: https://www.kaggle.com/c/pine-real-state/overview

#The purpose of this competition is to predict the sales price of houses based on different characteristics
#of the house using regression techniques.

#Set the folder path (to do it only once)
folder_path <- ""; #define the path where data is located

#Load train data
train <- read.table(file.path(folder_path, "train.csv"), sep = ",", header = TRUE);
train

#Load submission format
submission <- read.table(file.path(folder_path, "sample_submission.csv"), sep = ",", header = TRUE);
submission

#Load test data 
test <- read.table(file.path(folder_path, "test.csv"), sep = ",", header = TRUE);
test[nrow(test),]
dim(test)
summary(train)

#First model . linear regression with 2 variables
mod1 <- lm(price ~ living_m2 + house_quality_index, data = train)

#Analyse statistical significance of variables 
summary(mod1)

par(mfrow = c(2,2))
plot(mod1) 

#Calculate the predictions on test data
predictions_test_1 <- predict(mod1, newdata = test);

submission_test_1 <- data.frame(id = submission_1, price = predictions_test)

submission_test_1

#Export predisctions in csv
write.csv(submission_test_1, "attempt_1.csv", row.names = FALSE, quote = FALSE)
help("write.csv")

#Second model - linear regression with almost all numerical variables
mod2 <- lm(price ~ living_vs_neighbors + baths + bedrooms + floors + age_since_construction + age_since_renovation + bearing + dist + living_m2 + house_quality_index, data = train)

#Analyse statistical significance of variables 
summary(mod2)
plot(mod2)

#Calculate the predictions on test data
predictions_test_2 <- predict(mod2, newdata = test)

submission_2 <- submission[,1]
submission_test_2 <- data.frame(id = submission_2, price = predictions_test_2)
submission_test_2

#Export predisctions in csv
write.csv(submission_test_2, "attempt_2.csv", row.names = FALSE, quote = FALSE)
help("write.csv")


#Load of train and test data with feature transformation (numerical encoding of categorical variables and log transformation of numerical variables) - see Excel file data_prep.
train_mod <- read.table(file.path(folder_path, "train_mod.csv"), sep = ",", header = TRUE);
train_mod

test_mod <- read.table(file.path(folder_path, "test_mod.csv"), sep = ",", header = TRUE);
test_mod

summary(train_mod)

#Third model - linear regression with v ariables that are most correlated with the dependent variable 
mod3 <- lm(price ~ bearing + bedrooms + living_m2 + lot_m2_ln + living_vs_neighbors + viewsToPOI_num + view_quality_num, data = train_mod)

#Analyse statistical significance of variables 
summary(mod3)

plot(mod3)

#Calculate predictions on test data
predictions_test_3 <- predict(mod3, newdata = test_mod)

submission_3 <- submission[,1]
submission_test_3 <- data.frame(id = submission_3, price = predictions_test_3)
submission_test_3

#Export predictions in csv
write.csv(submission_test_3, "attempt_3.csv", row.names = FALSE, quote = FALSE)

#Fourth model - linear regression using the log of the price as dependent variable
mod4 <- lm(price_ln ~ floors + Basement_num + bearing + bedrooms + living_m2 + lot_m2_ln + living_vs_neighbors + viewsToPOI_num + view_quality_num, data = train_mod)

#Analyse statistical significance of variables 
summary(mod4)
plot(mod4)

#Calculate the predictions
predictions_test_ln_4 <- predict(mod4, newdata = test_mod)

#Reverse the log transformation
predictions_test_4 <- exp(predictions_test_ln_4)

predictions_test_4

submission_4 <- submission[,1]
submission_test_4 <- data.frame(id = submission_4, price = predictions_test_4)
submission_test_4

#Export predictions in csv
write.csv(submission_test_4, "attempt_4.csv", row.names = FALSE, quote = FALSE)

library(glmnet)

x <- model.matrix(price_ln ~ floors + Basement_num + bearing + bedrooms + living_m2 + lot_m2_ln + living_vs_neighbors + viewsToPOI_num + view_quality_num, data = train_mod)[,-1]
x_test <- model.matrix(baths ~ floors + Basement_num + bearing + bedrooms + living_m2 + lot_m2_ln + living_vs_neighbors + viewsToPOI_num + view_quality_num, data = test_mod)[,-1]

help("model.matrix")
y <- train_mod$price_ln

grid <- 10^seq(10,-2, length = 100)

#Fifth model - ridge regression model using hyperparameter search and using the log of price as dependent variable
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)

#Use of cross validation to find the best parameter
cv_mod <- cv.glmnet(x, y, alpha = 0)
plot(cv_mod)
min_lambda <- cv_mod$lambda.min

#Calculate the predictions
predictions_test_ln_5 <- predict(ridge.mod, s = min_lambda, newx = x_test)
predictions_test_5 <- as.numeric(predictions_test_ln_5)

#Reverse the log transformation
predictions_test_5 <- exp(predictions_test_5 )
predictions_test_5

submission_5 <- submission[,1]
submission_test_5 <- data.frame(id = submission_5, price = predictions_test_5)
submission_test_5

#Export predictions in csv
write.csv(submission_test_5, "attempt_5.csv", row.names = FALSE, quote = FALSE)


library(leaps)

#Sixth model - linear regression with best subset selection
mod6 <- regsubsets(price_ln ~ dist + house_quality_index + house_state_index_num + age_since_construction + age_since_renovation + floors + Basement_num + bearing + bedrooms + baths + living_m2 + lot_m2_ln + living_vs_neighbors + lot_vs_neighbors + viewsToPOI_num + view_quality_num, data = train_mod, nvmax = 16)
mod6_summary <- summary(mod6)
which.max(mod6_summary$rsq)
which.min(mod6_summary$cp)
coef(mod6, 16)

#Applying linear regression to best variables selected
mod6 <- lm(price_ln ~ dist + house_quality_index + house_state_index_num + age_since_construction + age_since_renovation + floors + Basement_num + bearing + bedrooms + baths + living_m2 + lot_m2_ln + living_vs_neighbors + lot_vs_neighbors + viewsToPOI_num + view_quality_num, data = train_mod)

#Analyse statistical significance of variables 
summary(mod6)

#Calculating predictions
predictions_test_ln_6 <- predict(mod6, newdata = test_mod)

#Undo log transformation
predictions_test_6 <- exp(predictions_test_ln_6)

predictions_test_6

submission_6 <- submission[,1]
submission_test_6 <- data.frame(id = submission_6, price = predictions_test_6)
submission_test_6

#Export predictions in csv
write.csv(submission_test_6, "attempt_6.csv", row.names = FALSE, quote = FALSE)

