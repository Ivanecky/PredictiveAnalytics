#------------------------------------------------------------------------------------------
# LOAD LIBRARIES
#------------------------------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(ggplot2)
library(caret)
library(rpart)
library(maptree)
library(forcats)
library(gridExtra)
library(ROCit)
library(ROCR)
library(gains)
library(party)
library(partykit)

#------------------------------------------------------------------------------------------
# LOAD DATA
#------------------------------------------------------------------------------------------
df <- fread("FinalModelData.csv")

#------------------------------------------------------------------------------------------
# DATA SETUP
#------------------------------------------------------------------------------------------
# Setup variable structure
attach(df)

df$DebtDimId <- as.factor(df$DebtDimId)
df$ExternalStatus <- as.factor(df$ExternalStatus)
#df$Bad <- as.factor(df$Bad)

#------------------------------------------------------------------------------------------
# DATA SPLITTING
#------------------------------------------------------------------------------------------
# Shuffle data randomly
#df <- df[sample(nrow(df)),]

# Set sample size of 70%
sz <- floor(0.7 * nrow(df))

# Seed makes splitting reproducible
set.seed(7)

# Define split point
train_ind <- sample(seq_len(nrow(df)), size = sz)

#------------------------------------------------------------------------------------------
# MODEL BUILD - TREE
#------------------------------------------------------------------------------------------
# Develop a model using a basic tree in rpart
tree1 <- rpart(Bad ~ `Days Deliq` + `Net Behavior Fees Billed During Cycle` + pay_ratio + bal_ratio +
               PropOverLimit + `Months On Book` + `Net Purchases During Cycle`,
               data = df[train_ind, ])

# Plot the tree
draw.tree (tree1, cex=1.1,
           nodeinfo=TRUE,
           cases="obs",
           digits=1, print.levels=TRUE,
           new=TRUE)

# Print and plot CP
printcp(tree1)
plotcp(tree1)

# Prune the tree based on CP
ptree <- prune(tree1, cp = 0.01)

# Plot the pruned tree
draw.tree (ptree, cex=1.1,
           nodeinfo=TRUE,
           cases="obs",
           digits=1, print.levels=TRUE,
           new=TRUE)

#------------------------------------------------------------------------------------------
# GENERATE PROBABILITY PREDICTIONS FOR TRAINING & TESTING USING TREE
#------------------------------------------------------------------------------------------
# Generate training predictions
trainProbPreds <- predict(ptree, df[train_ind, ])

# Generate testing predictions
testProbPreds <- predict(ptree, df[-train_ind])

# Add variables to data frame
df$probBad <- 0
df[train_ind, ]$probBad <- trainProbPreds
df[-train_ind, ]$probBad <- testProbPreds

#------------------------------------------------------------------------------------------
# SPLIT DATA INTO TRAIN / TEST SETS
#------------------------------------------------------------------------------------------
# Split using index
train <- df[train_ind, ]
test <- df[-train_ind, ]

#------------------------------------------------------------------------------------------
# ROC CURVE OF DATA - TREE
#------------------------------------------------------------------------------------------
m <- measureit(score = test$probBad, class = test$Bad,
               measure = c("ACC", "SENS", "SPEC", "FSCR"))

mymetrics <- as.data.frame(cbind(Cutoff = m$Cutoff, Depth = m$Depth,
                                 Accuracy = m$ACC, Sensitivity = m$SENS,
                                 Specificity = m$SPEC, `F-Score` = m$FSCR))

rocit_object_empirical <- rocit(score = test$probBad,
                                class = test$Bad)
summary(rocit_object_empirical)

# ROC plot
plot(rocit_object_empirical)

# Gainstable
gainstable <- gainstable(rocit_object_empirical, ngroup = 10)
print(gainstable, maxdigit = 2)
plot(gainstable, type = 2)

# Binormal vs Empirical ROC
plot(rocit_object_empirical, col = c(1,"gray50"), legend = FALSE, YIndex = FALSE)
rocit_bin <- rocit(score = df$probBad, class = df$Bad, method = "bin")
lines(rocit_bin$TPR~rocit_bin$FPR, col = 2, lwd = 2)
legend("bottomright", col = c(1,2),
       c("Empirical ROC", "Binormal ROC"), lwd = 2)

# Confidence interval for ROC
ci.roc <- ciROC(rocit_object_empirical, level = 0.9)
plot(ci.roc)

# KS plot
ksplot(rocit_object_empirical)

# Write gains table to csv
temp <- as.data.frame(cbind(gainstable$Bucket, gainstable$Obs, gainstable$CObs,
                            gainstable$Depth, gainstable$Resp, gainstable$CResp,
                            gainstable$RespRate, gainstable$CRespRate, gainstable$CCapRate,
                            gainstable$Lift, gainstable$CLift))

names(temp) <- c("Bucket", "Obs", "CObs", "Depth", "Resp", "CResp", "RespRate",
                 "CRespRate", "CCapRate", "Lift", "CLift")

write.csv(temp, file = "TreeGainsTable.csv")

#------------------------------------------------------------------------------------------
# MODEL BUILD - LOGISTIC REGRESSION
#------------------------------------------------------------------------------------------
# Create logisitic model
logMod1 <- glm(Bad ~ `Days Deliq` + `Net Behavior Fees Billed During Cycle` + pay_ratio + bal_ratio +
               PropOverLimit + `External Status` + `Months On Book` + `Net Purchases During Cycle`,
               family=binomial(link='logit'),
               data = df[train_ind, ])

# Summary of logistic model
summary(logMod1)

# Create new log model with only significant parameters
logMod2 <- glm(Bad ~ `Days Deliq` + `Net Behavior Fees Billed During Cycle` + PropOverLimit +
               `Months On Book` + `Net Purchases During Cycle`,
               family=binomial(link='logit'),
               data=df[train_ind, ])

# Summary of new model
summary(logMod2)

#------------------------------------------------------------------------------------------
# GENERATE PREDICTIONS USING LOGISTIC MODEL
#------------------------------------------------------------------------------------------
# Generate predictions for training and testing data
logTrainProb <- predict( logMod2, df[train_ind], type = "response" )
logTestProb <- predict( logMod2, df[-train_ind], type = "response" )

df$probBadLog <- 0
df[train_ind, ]$probBadLog <- logTrainProb
df[-train_ind, ]$probBadLog <- logTestProb

write.csv(df, file = "scored_data.csv")

#------------------------------------------------------------------------------------------
# ROC CURVE OF DATA - LOGISITIC TESTING 
#------------------------------------------------------------------------------------------
m <- measureit(score = logTestProb, class = df[-train_ind, ]$Bad,
               measure = c("ACC", "SENS", "SPEC", "FSCR"))

mymetrics <- as.data.frame(cbind(Cutoff = m$Cutoff, Depth = m$Depth,
                                 Accuracy = m$ACC, Sensitivity = m$SENS,
                                 Specificity = m$SPEC, `F-Score` = m$FSCR))

rocit_object_empirical <- rocit(score = logTestProb,
                                class = df[-train_ind, ]$Bad)
summary(rocit_object_empirical)

# ROC plot
plot(rocit_object_empirical)

# Gainstable
gainstable <- gainstable(rocit_object_empirical, ngroup = 10)
print(gainstable, maxdigit = 2)
plot(gainstable, type = 2)

# Binomal vs Empirical ROC
plot(rocit_object_empirical, col = c(1,"gray50"), legend = FALSE, YIndex = FALSE)
rocit_bin <- rocit(score = logTestProb, class = df[-train_ind, ]$Bad, method = "bin")
lines(rocit_bin$TPR~rocit_bin$FPR, col = 2, lwd = 2)
legend("bottomright", col = c(1,2),
       c("Empirical ROC", "Binormal ROC"), lwd = 2)

# KS plot
ksplot(rocit_object_empirical)

# Write gains table to csv
temp <- as.data.frame(cbind(gainstable$Bucket, gainstable$Obs, gainstable$CObs,
                            gainstable$Depth, gainstable$Resp, gainstable$CResp,
                            gainstable$RespRate, gainstable$CRespRate, gainstable$CCapRate,
                            gainstable$Lift, gainstable$CLift))

names(temp) <- c("Bucket", "Obs", "CObs", "Depth", "Resp", "CResp", "RespRate",
                 "CRespRate", "CCapRate", "Lift", "CLift")

write.csv(temp, file = "LogGainsTable.csv")

#------------------------------------------------------------------------------------------
# ROC CURVE OF DATA - LOGISITIC TESTING 
#------------------------------------------------------------------------------------------
m <- measureit(score = logTrainProb, class = df[train_ind, ]$Bad,
               measure = c("ACC", "SENS", "SPEC", "FSCR"))

mymetrics <- as.data.frame(cbind(Cutoff = m$Cutoff, Depth = m$Depth,
                                 Accuracy = m$ACC, Sensitivity = m$SENS,
                                 Specificity = m$SPEC, `F-Score` = m$FSCR))

rocit_object_empirical <- rocit(score = logTrainProb,
                                class = df[train_ind, ]$Bad)
summary(rocit_object_empirical)

# ROC plot
plot(rocit_object_empirical)

# Gainstable
gainstable <- gainstable(rocit_object_empirical, ngroup = 10)
print(gainstable, maxdigit = 2)
plot(gainstable, type = 2)

# Binomal vs Empirical ROC
plot(rocit_object_empirical, col = c(1,"gray50"), legend = FALSE, YIndex = FALSE)
rocit_bin <- rocit(score = logTrainProb, class = df[train_ind, ]$Bad, method = "bin")
lines(rocit_bin$TPR~rocit_bin$FPR, col = 2, lwd = 2)
legend("bottomright", col = c(1,2),
       c("Empirical ROC", "Binormal ROC"), lwd = 2)

# KS plot
ksplot(rocit_object_empirical)

# Write gains table to csv
temp <- as.data.frame(cbind(gainstable$Bucket, gainstable$Obs, gainstable$CObs,
                            gainstable$Depth, gainstable$Resp, gainstable$CResp,
                            gainstable$RespRate, gainstable$CRespRate, gainstable$CCapRate,
                            gainstable$Lift, gainstable$CLift))

names(temp) <- c("Bucket", "Obs", "CObs", "Depth", "Resp", "CResp", "RespRate",
                 "CRespRate", "CCapRate", "Lift", "CLift")

write.csv(temp, file = "LogGainsTableTraining.csv")

#------------------------------------------------------------------------------------------
# MAKE PREDICTIONS FOR LOGISTIC MODEL
#------------------------------------------------------------------------------------------
# Attach logProb to train and test
train$logProb <- logTrainProb
test$logProb <- logTestProb

# Order training and testing data by probability of bad.
train <- train[order(-logProb),] 
test <- test[order(-logProb),]

# New var to hold predicted bad
train$predBad <- 0
test$predBad <- 0

# Assign pred bad based on cutoff 
train[1:2788, ]$predBad <- 1
test[1:1193, ]$predBad <- 1

# Convert pred bad to factor levels
train$predBad <- as.factor(train$predBad)
test$predBad <- as.factor(test$predBad)
train$Bad <- as.factor(train$Bad)
test$Bad <- as.factor(test$Bad)

# Tables of accuracy
table(train$predBad, train$Bad)
table(test$predBad, test$Bad)


#------------------------------------------------------------------------------------------
# WRITE TO CSV
#------------------------------------------------------------------------------------------
# Write to csv
write.csv(test[, c(1, 5, 12, 13)], file = "Test.csv")
write.csv(train[, c(1, 5, 12, 13)], file = "Train.csv")


#------------------------------------------------------------------------------------------
# ROCR APPROACH
#------------------------------------------------------------------------------------------
# Generate predictions using ROCR
predsTrain <- prediction(logTrainProb, df[train_ind, ]$Bad)
predsTest <- prediction(logTestProb, df[-train_ind, ]$Bad)

# Create performance objects
roc.perf.train = performance(predsTrain, measure = "tpr", x.measure = "fpr")
roc.perf.test = performance(predsTest, measure = "tpr", x.measure = "fpr")

# Plot ROC curve - Train
plot(roc.perf.train)
abline(0,1)

# Plot ROC curve - Test
plot(roc.perf.test)
abline(0,1)

# Get and plot accuracy of predictions
acc.perf.train = performance(predsTrain, measure = "acc")
plot(acc.perf.train)

acc.perf.test = performance(predsTest, measure = "acc")
plot(acc.perf.test)

# Get cutoff point of max accuracy
ind = which.max( slot(acc.perf.train, "y.values")[[1]] )
acc = slot(acc.perf.train, "y.values")[[1]][ind]
cutoff = slot(acc.perf.train, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))

# Get AUC value
auc.perf.train = performance(predsTrain, measure = "auc")
auc.perf.train@y.values

# Get cutoff point of max accuracy
ind = which.max( slot(acc.perf.test, "y.values")[[1]] )
acc = slot(acc.perf.test, "y.values")[[1]][ind]
cutoff = slot(acc.perf.test, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))

# Get AUC value
auc.perf.test = performance(predsTest, measure = "auc")
auc.perf.test@y.values

# Get average Net Profit for good and bad customers
badNetAvg <- mean(df[which(df$Bad == '1'), ]$NetProfit, na.rm = T)
goodNetAvg <- mean(df[which(df$Bad == '0'), ]$NetProfit, na.rm = T)


