library(dplyr)
library(mltools)
library(data.table)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(zoo)
library(ggplot2)
library(ParamHelpers)
library(mlr)
library(parallel)
library(parallelMap)


# Read in data ------------------------------------------------------------
df <- read.csv("other_data/df.csv")

# Select and rename variables --------------------------------------------------------
# select only potentially relevant variables
df_select <- df %>% select(matches(" event|t[1,2]jobsa|t[1,2]jobstr|Age|t1cope|Education|emotions$|found|gender|t1wofa|Industry|meansev|occ|Co-own|preocc|solved|t1threat|Buisness age"))


# One Hot Encoding and standardization  --------------------------------------
# select categorical columns
cat_data <- df_select %>% select(matches("Severest|Education|t1found|t1gender|t1occ|ownership|t1preocc|Industry"))
df_select[,names(cat_data)] <- lapply(df_select[,names(cat_data)] , factor)

# standardize numerical variables
df_select <- df_select %>% mutate_if(is.numeric, ~scale(., center = TRUE, scale = TRUE))

df_select <- setDT(df_select)
df_select <- one_hot(df_select, cols = "auto")

df_select <- df_select %>% as.data.frame(.)

# Remove missing on most rows ---------------------------------------------
## Remove columns with more than 20% NA
df_select <- df_select %>% select_if(~mean(is.na(.)) < 0.2)
colnames(df_select)[colSums(is.na(df_select)) > 0] #show NA columns 

## replace catgeorical NA with mode
calc_mode <- function(x){
  
  # List the distinct / unique values
  distinct_values <- unique(x)
  
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}

df_select <- df_select %>% 
  mutate(`Problem.solved` = if_else(is.na(`Problem.solved`), 
                                    calc_mode(`Problem.solved`), 
                                    `Problem.solved`))

## replace numerical NA with mean
df_select <- na.aggregate(df_select)


# Crate training and test datasets ----------------------------------------
## 75% of the sample size
smp_size <- floor(0.75 * nrow(df_select))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df_select)), size = smp_size)

train <- df_select[train_ind, ]
test <- df_select[-train_ind, ] 

# JOB SATISFACTION --------------------------------------------------------

y_t1jobsa_train <- train['t1jobsa'] 
y_t1jobsa_train <- as.vector(y_t1jobsa_train[['t1jobsa']])

X_train <- subset(train, select=-c(t1jobsa, t1jobstr)) %>% as.matrix(.) # remove job strain as well, not very informative as a predictor! (Multicollinearity)

y_t1jobsa_test <- test['t1jobsa'] 
y_t1jobsa_test <- as.vector(y_t1jobsa_test[['t1jobsa']])

X_test <- subset(test, select=-c(t1jobsa)) %>% as.matrix(.)


## Crate xgb data type -----------------------------------------------------
train_XGB <- xgb.DMatrix(label = y_t1jobsa_train, data = as.matrix(X_train))

## Random grid ---------------------------------------------------------
# #create tasks
X_train_df <- as.data.frame(cbind(X_train, y_t1jobsa_train))
X_test_df <- as.data.frame(cbind(X_test, y_t1jobsa_test))

traintask <- makeRegrTask(data = X_train_df,target = "y_t1jobsa_train")
testtask <- makeRegrTask(data = X_test_df,target = "y_t1jobsa_test")

#create learner
lrn <- makeLearner("regr.xgboost", predict.type = "response")
lrn$par.vals <- list(objective="reg:squarederror", eval_metric="error", nrounds=100L, eta=0.1)

#set parameter space
params <- makeParamSet(makeIntegerParam("max_depth", lower = 3L,upper = 10L),
                       makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
                       makeNumericParam("subsample",lower = 0.5,upper = 1), 
                       makeNumericParam("colsample_bytree",lower = 0.5,upper = 1),
                       makeNumericParam("eta",lower = 0.01, upper = 0.1)
                       )

#set resampling strategy
rdesc <- makeResampleDesc("CV", iters=2L) 

#search strategy: build 10 models with different parameters, random search
ctrl <- makeTuneControlRandom(maxit = 40L)

#set parallel backend
parallelStartSocket(cpus = detectCores())

#parameter tuning
mytune <- tuneParams(learner = lrn, task = traintask, par.set = params, resampling =  rdesc, control = ctrl, show.info = T)


# Grid search -------------------------------------------------------------

# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
  nrounds = c(1000, 1500),
  eta = c(0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8, 10),
  colsample_bytree = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
  min_child_weight = c(1 , 2, 3, 4, 5),
  gamma = c(0.1, 0.2),
  subsample = 1
)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 2,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  allowParallel = TRUE
)

# train the model for each parameter combination in the grid,
#   using CV to evaluate
xgb_train_1 = caret::train(
  X_train,
  y_t1jobsa_train,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"
)




# scatter plot of the AUC against max_depth and eta
ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) +
  geom_point() +
  theme_bw() +
  scale_size_continuous(guide = "none")

## Building model ----------------------------------------------------------

xgb <- xgboost(X_train,
               label = y_t1jobsa_train,
               nrounds = 1000,
               objective = "reg:squarederror",
               early_stopping_rounds = 10,
               max_depth = 9,
               min_child_weight=2.26,
               subsample=0.837,
               colsample_bytree=0.952,
               eta=0.0525)


## Predict test ------------------------------------------------------------
# predict values in test set
y_pred <- predict(xgb_train_1, data.matrix(X_test))


## Measure model performance -----------------------------------------------
mse = mean((y_t1jobsa_test - y_pred)^2)
mae = caret::MAE(y_t1jobsa_test, y_pred)
rmse = caret::RMSE(y_t1jobsa_test, y_pred)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

# Compare to simple mean model
y_pred_mean <- mean(y_t1jobsa_test)
mse_mean = mean((y_t1jobsa_test - y_pred_mean)^2)
mae_mean = caret::MAE(y_t1jobsa_test, y_pred_mean)
rmse_mean = caret::RMSE(y_t1jobsa_test, y_pred_mean)

cat("MSE mean model: ", mse_mean, "MAE mean model: ", mae_mean, " RMSE mean model: ", rmse_mean)


## Plot model prediction ---------------------------------------------------

options(repr.plot.width=8, repr.plot.height=4)
# Plot predictions vs test data
my_data <- as.data.frame(cbind(y_t1jobsa_test, y_pred))

ggplot(my_data,aes(y_pred, y_t1jobsa_test)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Linear Regression ') + ggtitle("Extreme Gradient Boosting: Prediction vs Test Data") +
  xlab("Predecited Job satisfaction ") + ylab("Observed Job satisfaction") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))


## Inspect tree ------------------------------------------------------------
model <- xgb.dump(xgb, with_stats = T)
model[1:10] #This statement prints top 10 nodes of the model

# Get the feature real names
names <- dimnames(data.matrix(X_train))[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb)
# Nice graph
xgb.plot.importance(importance_matrix[1:10,])


## Improve model with only relevant features -------------------------------

df_select <- df %>% select(matches("t1jobsa|emotions$|meansev|Buisness age"))

# standardize numerical variables
df_select <- df_select %>% mutate_if(is.numeric, ~scale(., center = TRUE, scale = TRUE))
df_select <- setDT(df_select)
df_select <- one_hot(df_select, cols = "auto")
df_select <- df_select %>% as.data.frame(.)

## Remove columns with more than 20% NA
df_select <- df_select %>% select_if(~mean(is.na(.)) < 0.2)
colnames(df_select)[colSums(is.na(df_select)) > 0] #show NA columns 

## replace numerical NA with mean
df_select <- na.aggregate(df_select)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(df_select))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df_select)), size = smp_size)

train <- df_select[train_ind, ]
test <- df_select[-train_ind, ]

y_t1jobsa_train <- train['t1jobsa'] 
y_t1jobsa_train <- as.vector(y_t1jobsa_train[['t1jobsa']])

X_train <- subset(train, select=-c(t1jobsa)) %>% as.matrix(.)

y_t1jobsa_test <- test['t1jobsa'] 
y_t1jobsa_test <- as.vector(y_t1jobsa_test[['t1jobsa']])

X_test <- subset(test, select=-c(t1jobsa)) %>% as.matrix(.)

# Crate xgb data type 
train_XGB <- xgb.DMatrix(label = y_t1jobsa_train, data = as.matrix(X_train))

# Build hypergrid 
# #create tasks
X_train_df <- as.data.frame(cbind(X_train, y_t1jobsa_train))
X_test_df <- as.data.frame(cbind(X_test, y_t1jobsa_test))

traintask <- makeRegrTask(data = X_train_df,target = "y_t1jobsa_train")
testtask <- makeRegrTask(data = X_test_df,target = "y_t1jobsa_test")

#create learner
lrn <- makeLearner("regr.xgboost", predict.type = "response")
lrn$par.vals <- list(objective="reg:squarederror", eval_metric="error", nrounds=100L, eta=0.1)

#set parameter space
params <- makeParamSet(makeIntegerParam("max_depth", lower = 3L,upper = 10L),
                       makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
                       makeNumericParam("subsample",lower = 0.5,upper = 1), 
                       makeNumericParam("colsample_bytree",lower = 0.5,upper = 1),
                       makeNumericParam("eta",lower = 0.01, upper = 0.1)
)

#set resampling strategy
rdesc <- makeResampleDesc("CV", iters=5L) 

#search strategy: build 10 models with different parameters, random search
ctrl <- makeTuneControlRandom(maxit = 40L)

#set parallel backend
parallelStartSocket(cpus = detectCores())

#parameter tuning
mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, par.set = params, control = ctrl, show.info = T)

# Building model 
xgb <- xgboost(X_train,
               label = y_t1jobsa_train,
               nrounds = 1000,
               objective = "reg:squarederror",
               early_stopping_rounds = 10,
               max_depth = 4,
               min_child_weight=6.26,
               subsample=0.635,
               colsample_bytree=0.615,
               eta=0.0722,
               mse.test.mean=0.8631808
)

# Predict test 
# predict values in test set
y_pred <- predict(xgb, data.matrix(X_test))


# Measure model performance 
mse = mean((y_t1jobsa_test - y_pred)^2)
mae = caret::MAE(y_t1jobsa_test, y_pred)
rmse = caret::RMSE(y_t1jobsa_test, y_pred)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)


# JOB STRAIN --------------------------------------------------------------

## Crate training and test datasets ----------------------------------------
## set the seed to make your partition reproducible
set.seed(123)

y_t1jobstr_train <- train['t1jobstr']
y_t1jobstr_train <- as.vector(y_t1jobstr_train[['t1jobstr']])

X_train <- subset(train, select=-c(t1jobstr)) %>% as.matrix(.)

y_t1jobstr_test <- test['t1jobstr']
y_t1jobstr_test <- as.vector(y_t1jobstr_test[['t1jobstr']])

X_test <- subset(test, select=-c(t1jobstr)) %>% as.matrix(.)

## Crate xgb data type -----------------------------------------------------
train_XGB <- xgb.DMatrix(label = y_t1jobstr_train, data = as.matrix(X_train))

## Build hypergrid ---------------------------------------------------------
# #create tasks
X_train_df <- as.data.frame(cbind(X_train, y_t1jobstr_train))
X_test_df <- as.data.frame(cbind(X_test, y_t1jobstr_test))

traintask <- makeRegrTask(data = X_train_df,target = "y_t1jobstr_train")
testtask <- makeRegrTask(data = X_test_df,target = "y_t1jobstr_test")

#create learner
lrn <- makeLearner("regr.xgboost", predict.type = "response")
lrn$par.vals <- list(objective="reg:squarederror", eval_metric="error", nrounds=100L, eta=0.1)

#set parameter space
params <- makeParamSet(makeIntegerParam("max_depth", lower = 1L,upper = 10L),
                       makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
                       makeNumericParam("subsample",lower = 0.5,upper = 1), 
                       makeNumericParam("colsample_bytree",lower = 0.5,upper = 1),
                       makeNumericParam("eta",lower = 0.01, upper = 0.1)
)

#set resampling strategy
rdesc <- makeResampleDesc("CV", iters=2L) 

#search strategy: build 40 models with different parameters, random search
ctrl <- makeTuneControlRandom(maxit = 40L)

#set parallel backend
parallelStartSocket(cpus = detectCores())

#parameter tuning
mytune <- tuneParams(learner = lrn, task = traintask, par.set = params, resampling =  rdesc, control = ctrl, show.info = T)


## Building model ----------------------------------------------------------
xgb <- xgboost(X_train,
               label = y_t1jobsa_train,
               nrounds = 1000,
               objective = "reg:squarederror",
               early_stopping_rounds = 10,
               max_depth = 2,
               min_child_weight=1.3,
               subsample=0.90,
               colsample_bytree=0.517,
               eta=0.0405
)


## Predict test ------------------------------------------------------------
# predict values in test set
y_pred <- predict(xgb, data.matrix(X_test))


## Measure model performance -----------------------------------------------
mse = mean((y_t1jobstr_test - y_pred)^2)
mae = caret::MAE(y_t1jobstr_test, y_pred)
rmse = caret::RMSE(y_t1jobstr_test, y_pred)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

# Compare to simple mean model
y_pred_mean <- mean(y_t1jobstr_test)
mse_mean = mean((y_t1jobstr_test - y_pred_mean)^2)
mae_mean = caret::MAE(y_t1jobstr_test, y_pred_mean)
rmse_mean = caret::RMSE(y_t1jobstr_test, y_pred_mean)

cat("MSE mean model: ", mse_mean, "MAE mean model: ", mae_mean, " RMSE mean model: ", rmse_mean)

## Plot model prediction ---------------------------------------------------

options(repr.plot.width=8, repr.plot.height=4)
## Plot predictions vs test data
my_data <- as.data.frame(cbind(y_t1jobsa_test, y_pred))

ggplot(my_data,aes(y_pred, y_t1jobsa_test)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Linear Regression ') + ggtitle("Extreme Gradient Boosting: Prediction vs Test Data") +
  xlab("Predecited Job satisfaction ") + ylab("Observed Job satisfaction") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))


## Inspect tree ------------------------------------------------------------
model <- xgb.dump(xgb, with_stats = T)
model[1:10] #This statement prints top 10 nodes of the model

# Get the feature real names
names <- dimnames(data.matrix(X_train))[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb)
# Nice graph
xgb.plot.importance(importance_matrix[1:10,])





