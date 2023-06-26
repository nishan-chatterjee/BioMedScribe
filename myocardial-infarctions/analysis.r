# set a seed for making tasks reproducible
set.seed(42)

# set the working directory
setwd(paste0(getwd(), "/MI"))

# set the output directory
outdir <- paste0(getwd(), "/Outputs")

# read the files and save the data to a table
mi <- read.table(file = "Myocardial infarction complications Database.csv",
                 sep = ",", header = TRUE, na.strings = NA)

# let's look at what our inputs and outputs are
# inputs in this case correspond to our patient records
# outputs correspond to the moment of complication reports
# the id's have been preserved

# inputs: patient records
inputs <- mi[1:112]

# let's look at the patient records in more detail
library(summarytools)
# since we focus on our first research question, i.e. the time of admission 
# to the hospital first, we exclude the features that aren't relevant
first_complication <- inputs[, -c(93, 94, 95, 100, 101, 102, 103, 104, 105)]
dfSummary(first_complication, style = "grid", graph.magnif = 0.75, 
          valid.col = FALSE)

# let's look at the missing values in more detail
library(tidyverse)

# let's look at missing data column wise and row wise to see if we can eliminate
# some of them to remove missing data
library(cowplot)

# for the rows
missing_each_rows <- data.frame(x = 1:1700, missing = as.integer(rowSums(is.na(first_complication))))

plot1 <- ggplot(data = missing_each_rows, aes(x = missing, color = "darkred")) + 
  geom_histogram(bins = 30, fill = "white", show.legend = FALSE, size = 1.1)
  
plot2 <- ggplot(data = missing_each_rows, aes(x = missing, color = "darkred")) + 
  geom_line(data = missing_each_rows, aes(x = x, y = missing), color = "steelblue", size = 1.1)

plot_grid(plot1, plot2, labels = "AUTO")

# for the columns
missing_each_cols <- data.frame(x = 1:103, missing = as.integer(colSums(is.na(first_complication))))

plot3 <- ggplot(data = missing_each_cols, aes(x = missing, color = "darkred")) + 
  geom_histogram(bins = 30, fill = "white", show.legend = FALSE, size = 1.1)

plot4 <- ggplot(data = missing_each_cols, aes(x = missing, color = "darkred")) + 
  geom_line(data = missing_each_cols, aes(x = x, y = missing), color = "steelblue", size = 1.1)

plot_grid(plot3, plot4, labels = "AUTO")

# another way to visualize this would be by storing the count of the missing values
missing_each_cols_new <- data.frame(colname = names(first_complication),
                                total = as.integer(colSums(is.na(first_complication))))

# sort them by most missing value columns appearing first
missing_each_cols_new <- missing_each_cols_new %>% arrange(desc(total))

plot5 <- ggplot(missing_each_cols_new, aes(x = colname, y = total, group = 1)) +
  geom_line(color = "steelblue") + geom_point(color = "darkred") +
  geom_hline(yintercept = quantile(missing_each_cols_new$total)) +
  labs(x = "Columns", y = "Sparsity") + ggtitle("Sparsity of patient features") +
  theme_minimal() + theme(axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5))

plot_grid(plot5, labels = "AUTO")

# we see here that the features are sometimes a bit sparse
nrow(missing_each_cols_new %>% filter(total == 0))
nrow(missing_each_cols_new %>% filter(total <= 10))

# here we can decide a threshold for the number of features we want for initial
# analysis by choosing the features which aren't missing for a lot of people

dense_features <- missing_each_cols_new %>% filter(total <= 10)
dense_cols <- which(colnames(first_complication) %in% dense_features$colname)
dense_mi <- first_complication[, dense_cols]

# drop all the rows containing the empty values
na_rows <- which(is.na(dense_mi), arr.ind = TRUE)[, 1]
dense_mi_drop <- dense_mi[-na_rows, ]

# now let's look at the summary of the data
dfSummary(dense_mi_drop, style = "grid", graph.magnif = 0.75, valid.col = FALSE)

# otherwise, we can fill the NA values by filling them with mean/median/mode values

# but since most of these variables are not continuous, we need to round them up
# to make our models noise free


# visualization into a box-plot
library(rstatix)
library(ggpubr)
ggsummarystats(missing_each_rows, ggfunc = ggboxplot, add = "jitter")

# outputs: moment of complications
outputs <- mi[, c(1, 113:124)]

# let's look at the moment of complications in more detail
summarytools::descr(outputs)

# since we're dealing with binary outcomes, 
# we're not concerned with the mean median mode
# instead we look at skewness and kurtosis of the data
# skewness helps reflect on the data symmetry/asymmetry
# kurtosis provides an unambiguous interpretation of the tail extremity

# we also look at the summary statistics of the frequency of each observation
# in our moment of complications
dfSummary(outputs, style = "grid", graph.magnif = 0.75, valid.col = FALSE)

# we can narrow down to the complications we can explain more easily
# we do this by looking at complications with non-extreme distributions first
# Chronic Heart Failure (ZSH) seems to have the biggest polarization
# another important moment of complication is the lethal outcome (LET_IS)
# which is a multiclass classification task

###################
# let's look at a few GLMs for these two moments of complications

# Chronic Heart Failure (ZSH)
predict_ZSN <- cbind(ZSN = outputs[-na_rows, ]$ZSN, dense_mi_drop)
fit_ZSN <- glm(formula = ZSN ~ ., data = predict_ZSN, family = "binomial")

# lasso, ridge, and elastic nets
library(dplyr)
# test-train-val split
split_ZSN <- sample(seq(1,3), size = nrow(predict_ZSN), replace = TRUE, prob = c(0.6, 0.2, 0.2))
train_ZSN <- predict_ZSN[split_ZSN == 1, ]
test_ZSN <- predict_ZSN[split_ZSN == 2, ]
val_ZSN <- predict_ZSN[split_ZSN == 3, ]

# create predictor matrices
train_ZSN.X <- as.matrix(within(train_ZSN, rm(ZSN, ID)))
test_ZSN.X <- as.matrix(within(test_ZSN, rm(ZSN, ID)))
val_ZSN.X <- as.matrix(within(val_ZSN, rm(ZSN, ID)))

train_ZSN.y <- train_ZSN$ZSN
test_ZSN.y <- test_ZSN$ZSN
val_ZSN.y <- val_ZSN$ZSN

# cross-validate to tune lambda for ridge and lasso
library(glmnet)
cvridge_ZSN <- cv.glmnet(train_ZSN.X, train_ZSN.y, family = "binomial", 
                         alpha = 0, nlambda = 20, type.measure = "auc")
cvlasso_ZSN <- cv.glmnet(train_ZSN.X, train_ZSN.y, family = "binomial", 
                         alpha = 1, nlambda = 20, type.measure = "auc")

# fit models with final lambda
ridgemod_ZSN <- glmnet(train_ZSN.X, train_ZSN.y, family = "binomial",
                       alpha = 0, lambda = cvridge_ZSN$lambda.1se)
lassomod_ZSN <- glmnet(train_ZSN.X, train_ZSN.y, family = "binomial",
                       alpha = 1, lambda = cvridge_ZSN$lambda.1se)

# pre-standardizing the predictor matrix for elastic net
# this is done so that glmnet wrapped in caret doesn't create problems while preprocessing
train_ZSN.stdX <- scale(train_ZSN.X)

# set up the training controls
library(caret)
library(plyr)
train_control_ZSN <- trainControl(method = "repeatedcv", number = 1,
                                  repeats = 1, search = "random",
                                  classProbs = TRUE, 
                                  summaryFunction = twoClassSummary,
                                  verboseIter = TRUE)

# factorizing before training
train_ZSN.y <- revalue(as.character(train_ZSN.y), c('1'="yes", '0'="no"))
# dropping the NA column
train_ZSN.stdX <- subset(train_ZSN.stdX, select = -c(fibr_ter_08))

# training the model
elastic_grid_ZSN <- train(train_ZSN.stdX, factor(train_ZSN.y), method = "glmnet",
                          tuneLength = 25, trControl = train_control_ZSN,
                          metric = "ROC", family = "multinomial", standardize = FALSE)
# the above didn't work and I couldn't figure out why

# trying out other models
library(randomForest)
library(e1071)

# vanilla
fit_vanilla_ZSN <- train(train_ZSN.stdX, factor(train_ZSN.y))
print(fit_vanilla_ZSN)
plot(fit_vanilla_ZSN)

# other prediction models
# CART
fit_rpart_ZSN <- train(x = train_ZSN.stdX, y = factor(train_ZSN.y), method = "rpart")
print(fit_rpart_ZSN)
plot(fit_rpart_ZSN)
# Stochastic Gradient Boosting
library(gbm)
fit_gbm_ZSN <- train(x = train_ZSN.stdX, y = factor(train_ZSN.y), 
                     method = "gbm", verbose = FALSE)
print(fit_gbm_ZSN)
plot(fit_gbm_ZSN)
# resampling methods
# random forest
fit_control_ZSN <- trainControl(method = "repeatedcv", number = 10, repeats = 15)
fit_rf_ZSN <- train(x = train_ZSN.stdX, y = factor(train_ZSN.y), method = "rf",
                    trControl = fit_control_ZSN)
print(fit_rf_ZSN)
plot(fit_rf_ZSN)
# cubist
library(Cubist)
cubist_tune_grid_ZSN <- expand.grid(committees = 1:3, neighbours = c(1, 5))
fit_cubist_ZSN <- train(x = train_ZSN.stdX, y = factor(train_ZSN.y), method = "cubist",
                       trControl = fit_control_ZSN, tuneGrid = cubist_tune_grid_ZSN)
print(fit_cubist_ZSN)
plot(fit_cubist_ZSN)
# cubist shows to be a wrong linear model for classification

# comparing models
predictions_ZSN <- extractPrediction(list(fit_rf_ZSN, fit_rpart_ZSN, fit_gbm_ZSN),
                                 testX = train_ZSN.stdX, testY = factor(train_ZSN.y))
summary(predictions_ZSN)
plotObsVsPred(predictions_ZSN)

# performance measures
library(devtools)
library(ithir)
predictions_ZSN %>% group_by(model, dataType) %>% do(goof(observed = .$obs, 
                                                          predicted = .$pred))

library(dplyr)
predictions_ZSN %>% group_by(model, dataType) %>% dplyr::summarise(
  rmse = RMSE(pred = pred, obs = obs),
  rsq = R2(pred = pred, obs = obs)
)
# also encountered problems with these

###########################

# replicating for lethal outcome prediction

# Lethal Outcome (LET_IS)
predict_LET_IS <- cbind(LET_IS = outputs[-na_rows, ]$LET_IS, dense_mi_drop)
fit_LET_IS <- lm(formula = LET_IS ~ ., data = predict_LET_IS, family = "binomial")

# lasso, ridge, and elastic nets

# test-train-val split
split_LET_IS <- sample(seq(1,3), size = nrow(predict_LET_IS), replace = TRUE, prob = c(0.6, 0.2, 0.2))
train_LET_IS <- predict_LET_IS[split_LET_IS == 1, ]
test_LET_IS <- predict_LET_IS[split_LET_IS == 2, ]
val_LET_IS <- predict_LET_IS[split_LET_IS == 3, ]

# create predictor matrices
train_LET_IS.X <- as.matrix(within(train_LET_IS, rm(LET_IS, ID)))
test_LET_IS.X <- as.matrix(within(test_LET_IS, rm(LET_IS, ID)))
val_LET_IS.X <- as.matrix(within(val_LET_IS, rm(LET_IS, ID)))

train_LET_IS.y <- train_LET_IS$LET_IS
test_LET_IS.y <- test_LET_IS$LET_IS
val_LET_IS.y <- val_LET_IS$LET_IS

# cross-validate to tune lambda for ridge and lasso
library(glmnet)
cvridge_LET_IS <- cv.glmnet(train_LET_IS.X, train_LET_IS.y, family = "multinomial", 
                         alpha = 0, nlambda = 20, type.measure = "auc")
cvlasso_LET_IS <- cv.glmnet(train_LET_IS.X, train_LET_IS.y, family = "multinomial", 
                         alpha = 1, nlambda = 20, type.measure = "auc")

# fit models with final lambda
ridgemod_LET_IS <- glmnet(train_LET_IS.X, train_LET_IS.y, family = "multinomial",
                       alpha = 0, lambda = cvridge_LET_IS$lambda.1se)
lassomod_LET_IS <- glmnet(train_LET_IS.X, train_LET_IS.y, family = "multinomial",
                       alpha = 1, lambda = cvridge_LET_IS$lambda.1se)

# pre-standardizing the predictor matrix for elastic net
# this is done so that glmnet wrapped in caret doesn't create problems while preprocessing
train_LET_IS.stdX <- scale(train_LET_IS.X)

# set up the training controls
train_control_LET_IS <- trainControl(method = "repeatedcv", number = 1,
                                  repeats = 1, search = "random",
                                  classProbs = TRUE, 
                                  summaryFunction = multiClassSummary,
                                  verboseIter = TRUE)

# factorizing before training
train_LET_IS.y <- revalue(as.character(train_LET_IS.y), c('0'="Zero",
                                                    '1'="One",
                                                    '2'="Two",
                                                    '3'="Three",
                                                    '4'="Four",
                                                    '5'="Five",
                                                    '6'="Six",
                                                    '7'="Seven"))

# training the model
library(MLmetrics)
elastic_grid_LET_IS <- train(train_LET_IS.stdX, factor(train_LET_IS.y), 
                             method = "glmnet", tuneLength = 25, 
                             trControl = train_control_LET_IS, metric = "ROC", 
                             family = "multinomial", standardize = FALSE)
# the above didn't work like the other one and I couldn't figure out why
# gave warnings

# trying out other models

# vanilla
fit_vanilla_LET_IS <- train(train_LET_IS.stdX, factor(train_LET_IS.y))
print(fit_vanilla_LET_IS)
plot(fit_vanilla_LET_IS)

# other prediction models
# CART
fit_rpart_LET_IS <- train(x = train_LET_IS.stdX, y = factor(train_LET_IS.y), method = "rpart")
print(fit_rpart_LET_IS)
plot(fit_rpart_LET_IS)
# Stochastic Gradient Boosting
fit_gbm_LET_IS <- train(x = train_LET_IS.stdX, y = factor(train_LET_IS.y), 
                     method = "gbm", verbose = FALSE)
print(fit_gbm_LET_IS)
plot(fit_gbm_LET_IS)
# resampling methods
# random forest
fit_control_LET_IS <- trainControl(method = "repeatedcv", number = 10, repeats = 15)
fit_rf_LET_IS <- train(x = train_LET_IS.stdX, y = factor(train_LET_IS.y), method = "rf",
                    trControl = fit_control_LET_IS)
print(fit_rf_LET_IS)
plot(fit_rf_LET_IS)
# cubist
cubist_tune_grid_LET_IS <- expand.grid(committees = 1:3, neighbours = c(1, 5))
fit_cubist_LET_IS <- train(x = train_LET_IS.stdX, y = factor(train_LET_IS.y), method = "cubist",
                        trControl = fit_control_LET_IS, tuneGrid = cubist_tune_grid_LET_IS)
print(fit_cubist_LET_IS)
plot(fit_cubist_LET_IS)

# comparing models
predictions_LET_IS <- extractPrediction(list(fit_rf_LET_IS, fit_cubist_LET_IS, fit_rpart_LET_IS, fit_gbm_LET_IS),
                                     testX = train_LET_IS.stdX, testY = factor(train_LET_IS.y))
summary(predictions_LET_IS)
plotObsVsPred(predictions_LET_IS)

# performance measures
predictions_LET_IS %>% group_by(model, dataType) %>% do(goof(observed = .$obs, 
                                                          predicted = .$pred))
