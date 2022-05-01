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