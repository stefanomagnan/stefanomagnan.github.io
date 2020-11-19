#### R Group Project - Group E ####


#The data comes from the following Kaggle competition:
#https://www.kaggle.com/c/ams-2014-solar-energy-prediction-contest/overview/evaluation

#The objective of the competition is to use regression techniques to predict the solar energy production
#for 98 stations in Oklahoma, USA. For this purpose first an EDA of the data is performed. Following the EDA,
#various statistical techniques are used to generate the predictions for the 98 stations.
#The error metric used to evaluate the predictions is the Mean Absolute Error.


############################ Load data ######################################
folder_path <- "" #define custom path where data is located

solar_data <- data.frame(readRDS(file.path(folder_path, "solar_dataset.RData")));

dim(solar_data);
head(solar_data);

additional_data <- data.frame(readRDS(file.path(folder_path,"additional_variables.RData"))); 
additional_data

library(data.table);
station_info <- fread(file.path(folder_path, "station_info.csv"));
dim(station_info);
head(station_info);


######### [] General codes & key variables ##########
key_vars <- data.frame(solar_data[,2:99]);
na_section <- data.frame(solar_data[5114:6909, 2:99]);
key_exist <- data.frame(solar_data[2:5113,2:99]);
solar_data_clean <- data.frame(solar_data[1:5113,]); #clean dataset where all NAs from y variables are removed (full dataset for the analysis)

set.seed(1);
#division train and test for the solar_data_clean
train_index <- sample(1:nrow(solar_data_clean), 0.7*nrow(solar_data_clean))
train <- data.frame(solar_data_clean[train_index,]) #train set used for the modelling
test  <- data.frame(solar_data_clean[-train_index,]) #test set using for the modelling


head(key_vars);
head(na_section);
head(key_exist);

##########################################

class(solar_data);
summary(solar_data) 
dim(solar_data) #dimensions of the data frame 
solar_data[,c(1:10)] 
colnames(solar_data) 
summary(solar_data[,c(1:100)]) #if you print more than 100 rows you cannot see them in the r console 
View(summary(solar_data)) #unless you do this (new panel) 
solar_data[c(5113:6909),c(1:10)] #NAs values that have to be predicted 

cor(solar_data[2:5113,2:99]) #correlation matrix for the 98 stations only for non NAs rows 

#Histograms of the variables 

attach(solar_data) 

hist(ACME, col = "blue", breaks = 40) 
hist(ADAX, col = "blue", breaks = 40) 
hist(ALTU, col = "blue", breaks = 40) 
hist(APAC, col = "blue", breaks = 40) 
hist(ARNE, col = "blue", breaks = 40) 
hist(BEAV, col = "blue", breaks = 40) 
hist(BESS, col = "blue", breaks = 40) 
hist(BIXB, col = "blue", breaks = 40) 
hist(BLAC, col = "blue", breaks = 40) 
hist(BOIS, col = "blue", breaks = 40) 

#and so on... 

#Boxplots of the variables 

help("boxplot") 

boxplot(ACME, main = "ACME") 
boxplot(ADAX, main = "ADAX") 
boxplot(ALTU, main = "ALTU") 
boxplot(APAC, main = "APAC") 
boxplot(ARNE, main = "ARNE") 
boxplot(BEAV, main = "BEAV") 
boxplot(BESS, main = "BESS") 
boxplot(BIXB, main = "BIXB") 
boxplot(BLAC, main = "BLAC") 
boxplot(BOIS, main = "BOIS") 

################### E ** D ** A ##################

solar_data[c(5113:6909),c(1:10)]

# View a sum of missing values
sapply(solar_data, function(x){sum(is.na(x))}); 

######## [ ] GLOBAL EDA ##########

######## [ ] Means Map (scatterplot) ##########

means <- sapply(key_vars, function(x){mean(x, na.rm = TRUE)});
plot(means, col = 'blue');

######## [ ] Map ##########

#install.packages("leaflet")
library(leaflet)
#map of the stations
map <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lat=station$nlat, lng=station$elon)
map 



######## [ ] Time Series visualization ##########
          
library(lubridate)
          
Date_New <- ymd(solar_data[,1]) #trasform the first column in date format
solar_data_date <- data.frame( solar_data, Date_New = Date_New)
          
#loop to create 2 plots and a summary for each variable of the original dataset
          
for(i in 2:5){ #can create time series for all 98 stations, but chose 4 as example
  print(paste("Description of the", colnames(solar_data)[i], "variable.", sep = " ")) #title
  print(summary(solar_data[,i])) #summary of the variable
  plot(solar_data_date[,ncol(solar_data_date)], solar_data_date[,i], type = "l", 
    main = paste("Time plot of", colnames(solar_data)[i], "variable", sep = " "), 
    xlab = "Year", 
    ylab = colnames(solar_data)[i]) #time plot
}
          
          
          ######## MODELLING ###########
          
          #analysis on full dataset using linear regression
          l <- list() #create an empty list where to store the models
          #FOR LOOP that calculates a model for each of 98 y variables using all PCA variables
          for(i in 2:98){ #starting from 2 because first y variable is the 2nd col
            y <- solar_data_clean[,i] #extract the y variable from the dataset
            data_loop <- solar_data_clean[,-c(1:99)] #delate all y variables from the dataset
            data_loop <- cbind(y, data_loop) #add only the y variable used in this model to the dataset
            model <- lm(y ~ ., data = data_loop) #calculate the model
            l <- c(l, list(model)) #store the model in the list
          }
          
          summary(l[[1]]) #access model number 1         
          
          
          results <- data.frame() #create the data frame where results are stored
          
          solar_data <- as.data.frame(solar_data)
          class(solar_data)
          solar_data_clean <- as.data.frame(solar_data_clean)
          class(solar_data_clean)
          
          solar_data_eval <- as.data.frame(solar_data[5114:6909,]) #dataset used to calculate predictions that will be uploaded on kaggle
          eval_df_lr <- data.frame(Date = solar_data_eval[,1]) #create data frame where predictions will be stored
          #to be run again before every time you run the loop (reset the df)
          class(eval_df_lr)
          head(eval_df_lr)
          l <- list() #create an empty list where to store the models, to be run before every time the loop is run
          
          #linear regression
          for(i in 2:99){
            y <- test[,i] #extract the y variable from the test dataset
            data_loop_test <- test[,-c(1:99)] #delete all y variables from the test dataset
            data_loop_test <- cbind(y, data_loop_test) #add only the y variable used in this model to the test dataset, complete test dataset with one y variable and all x variables
            y <- train[,i] #extract the y variable from the train dataset
            data_loop_train <- train[,-c(1:99)] #delete all y variables from the train dataset
            data_loop_train <- cbind(y, data_loop_train) #add only the y variable used in this model to the train dataset, complete train dataset with one y variable and all x variables
            model <- lm(y ~ ., data = data_loop_train) #calculate the model using train data
            l <- c(l, list(model)) #store the model in the list
            predictions_train <- predict(model, newdata = data_loop_train); #calculate predictions values using train data
            predictions_test <- predict(model, newdata = data_loop_test); #calculate predictions values using test data
            errors_train <- predictions_train - data_loop_train$y #calculate errors for train set
            errors_test <- predictions_test - data_loop_test$y #calculate errors for test set
            mse_train <- round(mean(errors_train^2), 2);
            mae_train <- round(mean(abs(errors_train)), 2);
            mse_test <- round(mean(errors_test^2), 2);
            mae_test <- round(mean(abs(errors_test)), 2);
            results <- rbind(results,data.frame(mse_train = mse_train, mae_train = mae_train, mse_test = mse_test, mae_test = mae_test)); #store the results in a data.frame
            predictions_eval <- predict(model, newdata = solar_data_eval) #calculate the predicted value for NA values (to be uploaded on kaggle)
            eval_df_lr <- cbind(eval_df_lr, predictions_eval) #store the predicted values inside a data frame
            colnames(eval_df_lr)[i] <- colnames(solar_data_eval)[i] #assign corresponding col name
          }
          
          library(MASS)
          #linear regression with stepwise
          for(i in 2:3){
            y <- test[,i] #extract the y variable from the test dataset
            data_loop_test <- test[,-c(1:99)] #delete all y variables from the test dataset
            data_loop_test <- cbind(y, data_loop_test) #add only the y variable used in this model to the test dataset, complete test dataset with one y variable and all x variables
            y <- train[,i] #extract the y variable from the train dataset
            data_loop_train <- train[,-c(1:99)] #delete all y variables from the train dataset
            data_loop_train <- cbind(y, data_loop_train) #add only the y variable used in this model to the train dataset, complete train dataset with one y variable and all x variables
            model <- lm(y ~ ., data = data_loop_train) #calculate the model using train data
            step.model <- stepAIC(model, direction = "both", 
                                  trace = FALSE)
            #l <- c(l, list(model)) #store the model in the list
            predictions_train <- predict(step.model, newdata = data_loop_train); #calculate predictions values using train data
            predictions_test <- predict(step.model, newdata = data_loop_test); #calculate predictions values using test data
            errors_train <- predictions_train - data_loop_train$y #calculate errors for train set
            errors_test <- predictions_test - data_loop_test$y #calculate errors for test set
            mse_train <- round(mean(errors_train^2), 2);
            mae_train <- round(mean(abs(errors_train)), 2);
            mse_test <- round(mean(errors_test^2), 2);
            mae_test <- round(mean(abs(errors_test)), 2);
            results <- rbind(results,
                             data.frame(mse_train = mse_train, mae_train = mae_train,
                                        mse_test = mse_test, mae_test = mae_test)); #store the results in a data.frame
            predictions_eval <- predict(step.model, newdata = solar_data_eval) #calculate the predicted value for NA values (to be uploaded on kaggle)
            eval_df_lr <- cbind(eval_df_lr, predictions_eval) #store the predicted values inside a data frame
            colnames(eval_df_lr)[i] <- colnames(solar_data_eval)[i] #assign corresponding col name
          }          
          
          #stepwise only for one model 
          
          y <- test[,2] #extract the y variable from the test dataset
          data_loop_test <- test[,-c(1:99)] #delete all y variables from the test dataset
          data_loop_test <- cbind(y, data_loop_test) #add only the y variable used in this model to the test dataset, complete test dataset with one y variable and all x variables
          y <- train[,2] #extract the y variable from the train dataset
          data_loop_train <- train[,-c(1:99)] #delete all y variables from the train dataset
          data_loop_train <- cbind(y, data_loop_train) #add only the y variable used in this model to the train dataset, complete train dataset with one y variable and all x variables
          model <- lm(y ~ ., data = data_loop_train) #calculate the model using train data
          
          step.model <- stepAIC(model, direction = "backward", 
                                trace = FALSE)
          
          
          dim(eval_df_lr)
          eval_df_lr
          results
          
          
          ### LASSO Regression Analysis
          library(glmnet)
          result_lasso <- c()
          grid <- 10^seq(10, -2, length = 100)
          
          solar_data_eval <- as.data.frame(solar_data[5114:6909,]) #dataset used to calculate predictions that will be uploaded on kaggle
          solar_data_eval_mat <- model.matrix(~., data = solar_data_eval[,100:456])[,-1] #transformed into matrix
          #to be run before running the loop each time
          result_lasso <- c()
          eval_df_lasso <- data.frame(Date = solar_data_eval[,1]) #create data frame where predictions will be stored (start from df that has only the date)
          
          
          set.seed(1)
          for(i in 2:99){
            lasso_set <- solar_data_clean[, c(100:456)] #only PC variables
            y <- solar_data_clean[,i] 
            lasso_set <- data.frame(y = y, lasso_set)
            x <- model.matrix(y ~., data = lasso_set)[,-1]
            lasso_cv <- cv.glmnet(x[train_index,], y[train_index], alpha = 1, lambda = grid, standardize = TRUE, nfolds = 10) #fit the model and find nest l?ambda (hyperpar)
            lambda_cv <- lasso_cv$lambda.min #store best lambda
            model_cv <- glmnet(x[train_index,], y[train_index], alpha = 1, lambda = lambda_cv, standardize = TRUE) #fit model usins best lambda
            lasso_pred <- pred_lasso <- predict(model_cv, x[-train_index,]) #calculate test pred
            mae_test <- mean(abs(lasso_pred-y[-train_index])) #calculate mae test
            result_lasso <- c(result_lasso, mae_test)
            lasso_eval <- predict(model_cv, solar_data_eval_mat) #calculate predictions
            eval_df_lasso <- cbind(eval_df_lasso, lasso_eval) #store the predicted values inside a data frame
            colnames(eval_df_lasso)[i] <- colnames(solar_data_eval)[i] #assign corresponding col name
          }
          