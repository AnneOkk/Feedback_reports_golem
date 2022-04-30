library(dplyr)
library(mltools)
library(data.table)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(zoo)
library(ggplot2)


# Select variables --------------------------------------------------------
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
  mutate(`Problem solved` = if_else(is.na(`Problem solved`), 
                                    calc_mode(`Problem solved`), 
                                    `Problem solved`))

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

y_t1jobsa_train <- train['t1jobsa'] 
y_t1jobsa_train <- as.vector(y_t1jobsa_train[['t1jobsa']])

y_t1jobstr_train <- train['t1jobstr']
y_t1jobstr_train <- as.vector(y_t1jobstr_train[['t1jobsa']])

y_solved_train <- train['Problem solved']
y_solved_train <- as.vector(y_solved[['t1jobsa']])

X_train <- subset(train, select=-c(t1jobsa, t1jobstr, `Problem solved`)) %>% as.matrix(.)

y_t1jobsa_test <- test['t1jobsa'] 
y_t1jobsa_test <- as.vector(y_t1jobsa_test[['t1jobsa']])

y_t1jobstr_test <- test['t1jobstr']
y_t1jobstr_test <- as.vector(y_t1jobstr_test[['t1jobsa']])

y_solved <- test['Problem solved']
y_solved <- as.vector(y_solved[['t1jobsa']])

X_test <- subset(test, select=-c(t1jobsa, t1jobstr, `Problem solved`)) %>% as.matrix(.)



# Crate xgb data type -----------------------------------------------------
train_XGB <- xgb.DMatrix(label = y_t1jobsa_train, data = as.matrix(X_train))

# Building model ----------------------------------------------------------

xgb <- xgboost(X_train,
               label = y_t1jobsa_train,
               nrounds = 1000,
               objective = "reg:squarederror",
               early_stopping_rounds = 3,
               max_depth = 6,
               eta = .25
)


# Predict test ------------------------------------------------------------
# predict values in test set
y_pred <- predict(xgb, data.matrix(X_test))


# Measure model performance -----------------------------------------------
mse = mean((y_t1jobsa_test - y_pred)^2)
mae = caret::MAE(y_t1jobsa_test, y_pred)
rmse = caret::RMSE(y_t1jobsa_test, y_pred)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)


# Plot model prediction ---------------------------------------------------

options(repr.plot.width=8, repr.plot.height=4)
# Plot predictions vs test data
my_data <- as.data.frame(cbind(y_t1jobsa_test, y_pred))

ggplot(my_data,aes(y_pred, y_t1jobsa_test)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Linear Regression ') + ggtitle("Extreme Gradient Boosting: Prediction vs Test Data") +
  xlab("Predecited Job satisfaction ") + ylab("Observed Job satisfaction") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))


# Inspect tree ------------------------------------------------------------
model <- xgb.dump(xgb, with_stats = T)
model[1:10] #This statement prints top 10 nodes of the model

# Get the feature real names
names <- dimnames(data.matrix(X_train))[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb)
# Nice graph
xgb.plot.importance(importance_matrix[1:10,])



