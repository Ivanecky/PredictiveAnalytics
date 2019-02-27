#-----------------------------------------------------
# TREE ASSIGNMENT PART 1
#-----------------------------------------------------

#-----------------------------------------------------
# LOAD LIBRARIES
#-----------------------------------------------------

library(tidyverse)
library(ggplot2)
library(rpart)
library(party)
library(datasets)
library(maptree)
library(C50)
library(partykit)
library(gridExtra)

#-----------------------------------------------------
# DATA SETUP
#-----------------------------------------------------

# Load iris data set
data(iris)
attach(iris)

# Shuffle data frame
df = iris[sample(nrow(iris), nrow(iris)), ]

# Split into training and testing data set
# Create split index
split_ind = floor(0.8*nrow(df))
# Split data
train = df[1:split_ind, ]
test = df[(split_ind+1):nrow(df), ]

#-----------------------------------------------------
# PART 0 - EXPLORATORY DATA ANALYSIS
#-----------------------------------------------------

# Histogram of each petal & sepal divided by species
ggplot(iris, aes(y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  facet_wrap(~Species) +
  ggtitle("Distribution of Sepal Length by Species") +
  labs(x = "Sepal Length", y = "Frequency") -> p1

ggplot(iris, aes(y = Sepal.Width, fill = Species)) +
  geom_boxplot() +
  facet_wrap(~Species) +
  ggtitle("Distribution of Sepal Width by Species") +
  labs(x = "Sepal Length", y = "Frequency") -> p2

ggplot(iris, aes(y = Petal.Length, fill = Species)) +
  geom_boxplot() +
  facet_wrap(~Species) +
  ggtitle("Distribution of Petal Length by Species") +
  labs(x = "Sepal Length", y = "Frequency") -> p3

ggplot(iris, aes(y = Petal.Width, fill = Species)) +
  geom_boxplot() +
  facet_wrap(~Species) +
  ggtitle("Distribution of Petal Width by Species") +
  labs(x = "Sepal Length", y = "Frequency") -> p4

# Arrange into grid
grid.arrange( p1, p2, p3, p4, nrow = 2, ncol = 2 )

#-----------------------------------------------------
#FUNCTION FOR CALCULATING PREDICTION ACCURACY
#-----------------------------------------------------

prediction.acc <- function(preds){
  # Check prediction accuracy
  # Count number correct
  correct = 0
  
  # Iterate through data
  for (i in 1:nrow(test)){
    if( test[i,]$Species == preds[i])
      correct = correct + 1;
  }
  
  # Calculate accuracy
  pred.acc <- (correct/nrow(test))*100
  
  # Print accuracy
  print(paste("Accuracy = ", round(pred.acc, 2)))
}

#-----------------------------------------------------
# PART 1 - CREATE A TREE USING RPART
#-----------------------------------------------------

# Create tree using rpart - specify anova for regression
tree1 <- rpart(train$Species ~ ., data = train, method = 'class')

# Plot tree
draw.tree (tree1, cex=1.1, 
           nodeinfo=TRUE,
           cases="obs",
           digits=1, print.levels=TRUE,
           new=TRUE)

#-----------------------------------------------------
# PART 2 - CREATE A TREE USING PARTY
#-----------------------------------------------------

# Create party tree
tree2 <- ctree(train$Species ~ ., data = train)

# Plot tree
plot(tree2)

#-----------------------------------------------------
# PART 3 - CREATE A TREE USING A NEW LIBRARY
#-----------------------------------------------------

# Create tree using C50
tree3 <- C5.0(train$Species ~ ., data = train, trials = 5)

# Plot tree
plot(tree3)

#-----------------------------------------------------
# PART 4 - MODEL TESTING
#-----------------------------------------------------
# Model Testing
# Predict for testing data

# rpart
# Generate predictions
preds <- predict(tree1, test, type = 'class')

# Accuracy
prediction.acc(preds)

# party
# Generate predictions
preds2 <- predict(tree2, test)

# Accuracy
prediction.acc(preds2)

# C50
# Generate predictions
preds3 <- predict(tree3, test)

# Accuracy
prediction.acc(preds3)

# Loop through and test
for (i in 1:4){
  # Resample data - keep same sample size of 30
  test <- iris[sample(1:nrow(iris), 30, replace=FALSE),]
  
  # rpart
  # Generate predictions
  preds <- predict(tree1, test, type = 'class')
  
  # Accuracy
  prediction.acc(preds)
  
  # party
  # Generate predictions
  preds2 <- predict(tree2, test)
  
  # Accuracy
  prediction.acc(preds2)
  
  # C50
  # Generate predictions
  preds3 <- predict(tree3, test)
  
  # Accuracy
  prediction.acc(preds3)
  
}
