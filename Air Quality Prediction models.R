# Importing the required libraries
library(hydroGOF)
library(MLmetrics)
library(caTools)
library(e1071)
library(plotrix)

########## Creating the necessary functions ##########
# Creating a function to measure r square
r_square = function(predicted, actualdata){
  r_sq = 1 - (sum((actualdata - predicted) ^ 2) / sum((actualdata - mean(actualdata)) ^ 2))
  return(r_sq)
}

# Creating a function to measure adjusted r square
adj_r_square = function(r_squared, no_pred, sample_size){
  adj = 1 - (((1 - r_squared) * (sample_size - 1)) / (sample_size - no_pred - 1))
  return(adj)
}

# Creating a function that measures the evaluation metrics of the model
SVR_EV_metrics = function(traindata, testdata, formula_x, formula_y, test_act, train_act, test_pred_set){
  
  # Creating all the SVR models to evaluate the accuracy between them
  SVR_model_eps_linear = svm(x = formula_x, y = formula_y, data = traindata ,type = 'eps-regression', kernel = 'linear')
  SVR_model_eps_poly = svm(x = formula_x, y = formula_y, data =  traindata, type = 'eps-regression', kernel = 'polynomial')
  SVR_model_eps_rbf = svm(x = formula_x, y = formula_y, data = traindata, type = 'eps-regression', kernel = 'radial')
  SVR_model_eps_sig = svm(x = formula_x, y = formula_y, data = traindata, type = 'eps-regression', kernel = 'sigmoid')
  SVR_model_nu_linear = svm(x = formula_x, y = formula_y, data = traindata, type = 'nu-regression', kernel = 'linear')
  SVR_model_nu_poly = svm(x = formula_x, y = formula_y, data = traindata, type = 'nu-regression', kernel = 'polynomial')
  SVR_model_nu_rbf = svm(x = formula_x, y = formula_y, data = traindata, type = 'nu-regression', kernel = 'radial')
  SVR_model_nu_sig = svm(x = formula_x, y = formula_y, data = traindata, type = 'nu-regression', kernel = 'sigmoid')
  
  # Creating a dataframe to evaluate the results
  
  SVR_model_df = as.data.frame(cbind( c('eps linear', 'eps polynomial', 'eps radial', 'eps sigmoid',
                                        'nu linear', 'nu polynomial', 'nu radial', 'nu sigmoid'),
                                      c(r_square(predicted = fitted(SVR_model_eps_linear), actualdata = train_act),
                                        r_square(predicted = fitted(SVR_model_eps_poly), actualdata = train_act),
                                        r_square(predicted = fitted(SVR_model_eps_rbf), actualdata = train_act),
                                        r_square(predicted = fitted(SVR_model_eps_sig), actualdata = train_act),
                                        r_square(predicted = fitted(SVR_model_nu_linear), actualdata = train_act),
                                        r_square(predicted = fitted(SVR_model_nu_poly), actualdata = train_act),
                                        r_square(predicted = fitted(SVR_model_nu_rbf), actualdata = train_act),
                                        r_square(predicted = fitted(SVR_model_nu_sig), actualdata = train_act)),
                                      c(r_square(predicted = predict(SVR_model_eps_linear, newdata = test_pred_set), actualdata = test_act),
                                        r_square(predicted = predict(SVR_model_eps_poly, newdata = test_pred_set), actualdata = test_act),
                                        r_square(predicted = predict(SVR_model_eps_rbf, newdata = test_pred_set), actualdata = test_act),
                                        r_square(predicted = predict(SVR_model_eps_sig, newdata = test_pred_set), actualdata = test_act), 
                                        r_square(predicted = predict(SVR_model_nu_linear, newdata = test_pred_set), actualdata = test_act),
                                        r_square(predicted = predict(SVR_model_nu_poly, newdata = test_pred_set), actualdata = test_act),
                                        r_square(predicted = predict(SVR_model_nu_rbf, newdata = test_pred_set), actualdata = test_act),
                                        r_square(predicted = predict(SVR_model_nu_sig, newdata = test_pred_set), actualdata = test_act)),
                                      c(adj_r_square(r_squared = r_square(predicted = fitted(SVR_model_eps_linear), actualdata = train_act), no_pred = length(traindata) - 1, sample_size = nrow(traindata)),
                                        adj_r_square(r_squared = r_square(predicted = fitted(SVR_model_eps_poly), actualdata = train_act), no_pred = length(traindata) - 1, sample_size = nrow(traindata)),
                                        adj_r_square(r_squared = r_square(predicted = fitted(SVR_model_eps_rbf), actualdata = train_act), no_pred = length(traindata) - 1, sample_size = nrow(traindata)),
                                        adj_r_square(r_squared = r_square(predicted = fitted(SVR_model_eps_sig), actualdata = train_act), no_pred = length(traindata) - 1, sample_size = nrow(traindata)), 
                                        adj_r_square(r_squared = r_square(predicted = fitted(SVR_model_nu_linear), actualdata = train_act), no_pred = length(traindata) - 1, sample_size = nrow(traindata)),
                                        adj_r_square(r_squared = r_square(predicted = fitted(SVR_model_nu_poly), actualdata = train_act), no_pred = length(traindata) - 1, sample_size = nrow(traindata)),
                                        adj_r_square(r_squared = r_square(predicted = fitted(SVR_model_nu_rbf), actualdata = train_act), no_pred = length(traindata) - 1, sample_size = nrow(traindata)),
                                        adj_r_square(r_squared = r_square(predicted = fitted(SVR_model_nu_sig), actualdata = train_act), no_pred = length(traindata) - 1, sample_size = nrow(traindata))),
                                      c(adj_r_square(r_squared = r_square(predicted = predict(SVR_model_eps_linear, newdata = test_pred_set), actualdata = test_act), no_pred = length(testdata) - 1, sample_size = nrow(testdata)),
                                        adj_r_square(r_squared = r_square(predicted = predict(SVR_model_eps_poly, newdata = test_pred_set), actualdata = test_act), no_pred = length(testdata) - 1, sample_size = nrow(testdata)),
                                        adj_r_square(r_squared = r_square(predicted = predict(SVR_model_eps_rbf, newdata = test_pred_set), actualdata = test_act), no_pred = length(testdata) - 1, sample_size = nrow(testdata)),
                                        adj_r_square(r_squared = r_square(predicted = predict(SVR_model_eps_sig, newdata = test_pred_set), actualdata = test_act), no_pred = length(testdata) - 1, sample_size = nrow(testdata)), 
                                        adj_r_square(r_squared = r_square(predicted = predict(SVR_model_nu_linear, newdata = test_pred_set), actualdata = test_act), no_pred = length(testdata) - 1, sample_size = nrow(testdata)),
                                        adj_r_square(r_squared = r_square(predicted = predict(SVR_model_nu_poly, newdata = test_pred_set), actualdata = test_act), no_pred = length(testdata) - 1, sample_size = nrow(testdata)),
                                        adj_r_square(r_squared = r_square(predicted = predict(SVR_model_nu_rbf, newdata = test_pred_set), actualdata = test_act), no_pred = length(testdata) - 1, sample_size = nrow(testdata)),
                                        adj_r_square(r_squared = r_square(predicted = predict(SVR_model_nu_sig, newdata = test_pred_set), actualdata = test_act), no_pred = length(testdata) - 1, sample_size = nrow(testdata))), 
                                      c(rmse(predict(SVR_model_eps_linear, newdata = test_pred_set), test_act),
                                        rmse(predict(SVR_model_eps_poly, newdata = test_pred_set), test_act),
                                        rmse(predict(SVR_model_eps_rbf, newdata = test_pred_set), test_act),
                                        rmse(predict(SVR_model_eps_sig, newdata = test_pred_set), test_act), 
                                        rmse(predict(SVR_model_nu_linear, newdata = test_pred_set), test_act),
                                        rmse(predict(SVR_model_nu_poly, newdata = test_pred_set), test_act),
                                        rmse(predict(SVR_model_nu_rbf, newdata = test_pred_set), test_act),
                                        rmse(predict(SVR_model_nu_sig, newdata = test_pred_set), test_act)),
                                      c(MAPE(predict(SVR_model_eps_linear, newdata = test_pred_set), test_act),
                                        MAPE(predict(SVR_model_eps_poly, newdata = test_pred_set), test_act),
                                        MAPE(predict(SVR_model_eps_rbf, newdata = test_pred_set), test_act),
                                        MAPE(predict(SVR_model_eps_sig, newdata = test_pred_set), test_act), 
                                        MAPE(predict(SVR_model_nu_linear, newdata = test_pred_set), test_act),
                                        MAPE(predict(SVR_model_nu_poly, newdata = test_pred_set), test_act),
                                        MAPE(predict(SVR_model_nu_rbf, newdata = test_pred_set), test_act),
                                        MAPE(predict(SVR_model_nu_sig, newdata = test_pred_set), test_act)),
                                      c(std.error(residuals(SVR_model_eps_linear)), std.error(residuals(SVR_model_eps_poly)),
                                        std.error(residuals(SVR_model_eps_rbf)), std.error(residuals(SVR_model_eps_sig)),
                                        std.error(residuals(SVR_model_nu_linear)), std.error(residuals(SVR_model_nu_poly)),
                                        std.error(residuals(SVR_model_nu_rbf)), std.error(residuals(SVR_model_nu_sig)))))
  assign('SVR_model_df', SVR_model_df, envir = .GlobalEnv)
  
}

############ Starting the Prediction model ############

# Importing the clean dataset
dataset = read.csv('AirQuality_clean.csv', stringsAsFactors = FALSE, header = FALSE)

# Renaming the dataset columns 
dataset_columns = c('d', 'm', 'y', 'h', 'co', 'co_s', 'NMHC', 'c6h6',
                    'NMHC_s', 'nox', 'nox_s', 'no2', 'no2_s',
                    'o3_s', 'temp', 'RH', 'AH', 'Monday', 'Tuesday',
                    'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
colnames(dataset) = dataset_columns

# Seperating the dataset with respect to the pollutants
co_dataset = dataset[, c(1:6, 9, 11, 13:24)]
NMHC_dataset = dataset[, c(1:4, 6, 7, 9, 11, 13:24)]
nox_dataset = dataset[, c(1:4, 6, 9, 10, 11, 13:24)]
no2_dataset = dataset[, c(1:4, 6, 9, 11, 12, 13:24)]
c6h6_dataset = dataset[, c(1:4, 6, 8, 9, 11, 13:24)]


##### Datasets Splitting #####

# Splitting the Carbon Monoxide dataset to training and test set
set.seed(123)
split_co_dataset = sample.split(co_dataset$co, SplitRatio = 0.8)
co_train_set = subset(co_dataset, split_co_dataset == TRUE)
co_test_set = subset(co_dataset, split_co_dataset == FALSE)
# Splitting the NMHC dataset to training and test set
set.seed(123)
split_NMHC_dataset = sample.split(NMHC_dataset$NMHC, SplitRatio = 0.8)
NMHC_train_set = subset(NMHC_dataset, split_NMHC_dataset == TRUE)
NMHC_test_set = subset(NMHC_dataset, split_NMHC_dataset == FALSE)
# Splitting the nox dataset to training and test set
set.seed(123)
split_nox_dataset = sample.split(nox_dataset$nox, SplitRatio = 0.8)
nox_train_set = subset(nox_dataset, split_nox_dataset == TRUE)
nox_test_set = subset(nox_dataset, split_nox_dataset == FALSE)
# Splitting the no2 dataset to training and test set
set.seed(123)
split_no2_dataset = sample.split(no2_dataset$no2, SplitRatio = 0.8)
no2_train_set = subset(no2_dataset, split_no2_dataset == TRUE)
no2_test_set = subset(no2_dataset, split_no2_dataset == FALSE)

# Splitting the c6h6 dataset to training and test set
set.seed(123)
split_c6h6_dataset = sample.split(c6h6_dataset$c6h6, SplitRatio = 0.8)
c6h6_train_set = subset(c6h6_dataset, split_c6h6_dataset == TRUE)
c6h6_test_set = subset(c6h6_dataset, split_c6h6_dataset == FALSE)

##### Linear regression Models for all the datasets #####

##### Carbon Monoxide
# Fitting the Linear regression model to the Carbon Monoxide dataset
co_regressor_lm = lm(formula = co ~ ., data = co_train_set)


# Finding the best SVR model for Carbon Monoxide
co_SVR_model_eval = SVR_EV_metrics(traindata = co_train_set, testdata = co_test_set, formula_x = co_train_set[, -5], 
                                   formula_y = co_train_set[, 5], test_act = co_test_set$co, train_act = co_train_set$co, 
                                   test_pred_set = co_test_set[, -5])

# Finalizing the model eval dataframe
co_lm_df = as.data.frame(cbind('linear regression', r_square(predicted = fitted(co_regressor_lm), actualdata = co_train_set[, 5]),
                               r_square(predicted = predict(co_regressor_lm, newdata = co_test_set[, -5]), actualdata = co_test_set[, 5]),
                               adj_r_square(r_squared = r_square(predicted = fitted(co_regressor_lm), actualdata = co_train_set[, 5]), no_pred = length(co_train_set) - 1, sample_size = nrow(co_train_set)),
                               adj_r_square(r_squared = r_square(predicted = predict(co_regressor_lm, newdata = co_test_set[, -5]), actualdata = co_test_set[, 5]), no_pred = length(co_test_set) - 1, sample_size = nrow(co_test_set)),
                               rmse(predict(co_regressor_lm, newdata = co_test_set[, -5]), co_test_set$co),
                               MAPE(predict(co_regressor_lm, newdata = co_test_set[, -5]), co_test_set$co),
                               std.error(residuals(co_regressor_lm))))

colnames(co_lm_df) = c('Model', 'Train_R2', 'Test_R2', 'Train_ADJ_R2', 'Test_ADJ_R2', 'RMSE', 'MAPE', 'Residual_std_error')
colnames(co_SVR_model_eval) = c('Model', 'Train_R2', 'Test_R2', 'Train_ADJ_R2', 'Test_ADJ_R2', 'RMSE', 'MAPE', 'Residual_std_error')
co_regression_models_df = rbind(co_lm_df, co_SVR_model_eval)


##### NMHC
# Fitting the Linear Regression model to the NMHC dataset
NMHC_regressor_lm = lm(formula = NMHC ~ ., data = NMHC_train_set)

# Finding the best SVR model for NMHC
NMHC_SVR_model_eval = SVR_EV_metrics(traindata = NMHC_train_set, testdata = NMHC_test_set, 
                                     formula_x = NMHC_train_set[, -6], formula_y = NMHC_train_set[, 6], 
                                     test_act = NMHC_test_set$NMHC, train_act = NMHC_train_set$NMHC, test_pred_set = NMHC_test_set[, -6])

# Finalizing the model eval data frame
NMHC_lm_df = as.data.frame(cbind('linear regression', r_square(predicted = fitted(NMHC_regressor_lm), actualdata = NMHC_train_set$NMHC),
                                 r_square(predicted = predict(NMHC_regressor_lm, newdata = NMHC_test_set[, -6]), actualdata = NMHC_test_set$NMHC),
                                 adj_r_square(r_squared = r_square(predicted = fitted(NMHC_regressor_lm), actualdata = NMHC_train_set$NMHC), no_pred = length(NMHC_train_set) - 1, sample_size = nrow(NMHC_train_set)),
                                 adj_r_square(r_squared = r_square(predicted = predict(NMHC_regressor_lm, newdata = NMHC_test_set[, -6]), actualdata = NMHC_test_set$NMHC), no_pred = length(NMHC_test_set) - 1, sample_size = nrow(NMHC_test_set)),
                                 rmse(predict(NMHC_regressor_lm, newdata = NMHC_test_set[, -6]), NMHC_test_set$NMHC),
                                 MAPE(predict(NMHC_regressor_lm, newdata = NMHC_test_set[, -6]), NMHC_test_set$NMHC),
                                 std.error(residuals(NMHC_regressor_lm))))

colnames(NMHC_lm_df) = c('Model', 'Train_R2', 'Test_R2', 'Train_ADJ_R2', 'Test_ADJ_R2', 'RMSE', 'MAPE', 'Residual_std_error')
colnames(NMHC_SVR_model_eval) = c('Model', 'Train_R2', 'Test_R2', 'Train_ADJ_R2', 'Test_ADJ_R2', 'RMSE', 'MAPE', 'Residual_std_error')
NMHC_regression_models_df = rbind(NMHC_lm_df, NMHC_SVR_model_eval)


##### nox
# Fitting the Linear Regression model to the nox dataset
nox_regressor_lm = lm(formula = nox ~ ., data = nox_train_set)

# Finding the best SVR model for nox dataset
nox_SVR_model_eval = SVR_EV_metrics(traindata = nox_train_set, testdata = nox_test_set, 
                                    formula_x = nox_train_set[, -7], formula_y = nox_train_set[, 7], 
                                    test_act = nox_test_set$nox, train_act = nox_train_set$nox, 
                                    test_pred_set = nox_test_set[, -7])

# Finalizing the model eval data frame
nox_lm_df = cbind('linear regression', r_square(predicted = fitted(nox_regressor_lm), actualdata = nox_train_set$nox),
                  r_square(predicted = predict(nox_regressor_lm, newdata = nox_test_set[, -7]), actualdata = nox_test_set$nox),
                  adj_r_square(r_squared = r_square(predicted = fitted(nox_regressor_lm), actualdata = nox_train_set$nox), no_pred = length(nox_train_set) - 1, sample_size = nrow(nox_train_set)),
                  adj_r_square(r_squared = r_square(predicted = predict(nox_regressor_lm, newdata = nox_test_set[, -7]), actualdata = nox_test_set$nox), no_pred = length(nox_test_set) - 1, sample_size = nrow(nox_test_set)),
                  rmse(predict(nox_regressor_lm, newdata = nox_test_set[, -7]), nox_test_set$nox),
                  MAPE(predict(nox_regressor_lm, newdata = nox_test_set[, -7]), nox_test_set$nox),
                  std.error(residuals(nox_regressor_lm)))

colnames(nox_lm_df) = c('Model', 'Train_R2', 'Test_R2', 'Train_ADJ_R2', 'Test_ADJ_R2', 'RMSE', 'MAPE', 'Residual_std_error')
colnames(nox_SVR_model_eval) = c('Model', 'Train_R2', 'Test_R2', 'Train_ADJ_R2', 'Test_ADJ_R2', 'RMSE', 'MAPE', 'Residual_std_error')
nox_regression_models_df = rbind(nox_lm_df, nox_SVR_model_eval)


##### no2
# Fitting the Linear Regression model to the no2 dataset
no2_regressor_lm = lm(formula = no2 ~ ., data = no2_train_set)

# Finding the best SVR model for the no2 dataset
no2_SVR_model_eval = SVR_EV_metrics(traindata = no2_train_set, testdata = no2_test_set, 
                                    formula_x = no2_train_set[, -8], formula_y = no2_train_set[, 8], 
                                    test_act = no2_test_set$no2, train_act = no2_train_set$no2,
                                    test_pred_set = no2_test_set[, -8])

# Finalizing the model eval data frame
no2_lm_df = cbind('linear regression', r_square(predicted = fitted(no2_regressor_lm), actualdata = no2_train_set$no2),
                  r_square(predicted = predict(no2_regressor_lm, newdata = no2_test_set[, -8]), actualdata = no2_test_set$no2),
                  adj_r_square(r_squared = r_square(predicted = fitted(no2_regressor_lm), actualdata = no2_train_set$no2), no_pred = length(no2_train_set) - 1, sample_size = nrow(no2_train_set)),
                  adj_r_square(r_squared = r_square(predicted = predict(no2_regressor_lm, newdata = no2_test_set[, -8]), actualdata = no2_test_set$no2), no_pred = length(no2_test_set) - 1, sample_size = nrow(no2_test_set)),
                  rmse(predict(no2_regressor_lm, newdata = no2_test_set[, -8]), no2_test_set$no2),
                  MAPE(predict(no2_regressor_lm, newdata = no2_test_set[, -8]), no2_test_set$no2),
                  std.error(residuals(no2_regressor_lm)))

colnames(no2_lm_df) = c('Model', 'Train_R2', 'Test_R2', 'Train_ADJ_R2', 'Test_ADJ_R2', 'RMSE', 'MAPE', 'Residual_std_error')
colnames(no2_SVR_model_eval) = c('Model', 'Train_R2', 'Test_R2', 'Train_ADJ_R2', 'Test_ADJ_R2', 'RMSE', 'MAPE', 'Residual_std_error')
no2_regression_models_df = rbind(no2_lm_df, no2_SVR_model_eval)

##### c6h6
# Fitting the Linear Regression model to the c6h6 dataset
c6h6_regressor_lm = lm(formula = c6h6 ~ ., data = c6h6_train_set)

# Finding the best SVR model for the c6h6 dataset
c6h6_SVR_model_eval = SVR_EV_metrics(traindata = c6h6_train_set, testdata = c6h6_test_set, 
                                     formula_x = c6h6_train_set[, -6], formula_y = c6h6_train_set[, 6], 
                                     test_act = c6h6_test_set$c6h6, train_act = c6h6_train_set$c6h6, 
                                     test_pred_set = c6h6_test_set[, -6])

# Finalizing the model eval data frame
c6h6_lm_df = cbind('linear regression', r_square(predicted = fitted(c6h6_regressor_lm), actualdata = c6h6_train_set$c6h6),
                   r_square(predicted = predict(c6h6_regressor_lm, newdata = c6h6_test_set[, -6]), actualdata = c6h6_test_set$c6h6),
                   adj_r_square(r_squared = r_square(predicted = fitted(c6h6_regressor_lm), actualdata = c6h6_train_set$c6h6), no_pred = length(c6h6_train_set) - 1, sample_size = nrow(c6h6_train_set)),
                   adj_r_square(r_squared = r_square(predicted = predict(c6h6_regressor_lm, newdata = c6h6_test_set[, -6]), actualdata = c6h6_test_set$c6h6), no_pred = length(c6h6_test_set) - 1, sample_size = nrow(c6h6_test_set)),
                   rmse(predict(c6h6_regressor_lm, newdata = c6h6_test_set[, -6]), c6h6_test_set$c6h6),
                   MAPE(predict(c6h6_regressor_lm, newdata = c6h6_test_set[, -6]), c6h6_test_set$c6h6),
                   std.error(residuals(c6h6_regressor_lm)))

colnames(c6h6_lm_df) = c('Model', 'Train_R2', 'Test_R2', 'Train_ADJ_R2', 'Test_ADJ_R2', 'RMSE', 'MAPE', 'Residual_std_error')
colnames(c6h6_SVR_model_eval) = c('Model', 'Train_R2', 'Test_R2', 'Train_ADJ_R2', 'Test_ADJ_R2', 'RMSE', 'MAPE', 'Residual_std_error')
c6h6_regression_models_df = rbind(c6h6_lm_df, c6h6_SVR_model_eval)

write.csv(co_regression_models_df, 'CO_RModels.csv')
write.csv(NMHC_regression_models_df, 'NMHC_RModels.csv')
write.csv(nox_regression_models_df, 'NOx_RModels.csv')
write.csv(no2_regression_models_df, 'NO2_RModels.csv')
write.csv(c6h6_regression_models_df, 'C6H6_RModels.csv')

