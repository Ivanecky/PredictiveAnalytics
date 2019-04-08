#------------------------------------------------------------------------------------------
# LOAD LIBRARIES
#------------------------------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(ggplot2)
library(caret)
library(lubridate)
library(GGally)

#------------------------------------------------------------------------------------------
# LOAD DATA
#------------------------------------------------------------------------------------------
df <- fread("RetentionDataRaw.csv")

#------------------------------------------------------------------------------------------
# DATA MANIPULATION
#------------------------------------------------------------------------------------------
# Print data structure
str(df)

# Attach data
attach(df)

# Convert variables to factors
df$`External Status` <- as.factor(df$`External Status`)
df$ClosureReason <- as.factor(df$ClosureReason)
df$DebtDimId <- as.factor(df$DebtDimId)
#df$`Credit Limit` <- as.factor(df$`Credit Limit`)

# Remove dollar signs from data to use as numeric
df$`Over limit Amount` = as.numeric(gsub("\\$", "", df$`Over limit Amount`))
df$`Credit Limit` = as.numeric(gsub("\\$", "", df$`Credit Limit`))
df$`Opening Balance` = as.numeric(gsub("\\$", "", df$`Opening Balance`))
df$`Ending Balance` = as.numeric(gsub("\\$", "", df$`Ending Balance`))
df$`Actual Min Pay Due` = as.numeric(gsub("\\$", "", df$`Actual Min Pay Due`))
df$`Total Min Pay Due` = as.numeric(gsub("\\$", "", df$`Total Min Pay Due`))
df$`Net Payments During Cycle` = as.numeric(gsub("\\$", "", df$`Net Payments During Cycle`))
df$`Net Purchases During Cycle` = as.numeric(gsub("\\$", "", df$`Net Purchases During Cycle`))
df$`Net Cash Advances During Cycle` = as.numeric(gsub("\\$", "", df$`Net Cash Advances During Cycle`))
df$`Net Premier Fees Billed During Cycle` = as.numeric(gsub("\\$", "", df$`Net Premier Fees Billed During Cycle`))
df$`Net Behavior Fees Billed During Cycle` = as.numeric(gsub("\\$", "", df$`Net Behavior Fees Billed During Cycle`))
df$`Net Concessions Billed During Cycle` = as.numeric(gsub("\\$", "", df$`Net Concessions Billed During Cycle`))

# Convert customer score to numeric
df$`Good Customer Score` <- as.numeric(df$`Good Customer Score`)

# Remove empty rows at end of file
df <- df[!is.na(df$DebtDimId), ]

#------------------------------------------------------------------------------------------
# EXPLORATORY DATA ANALYSIS
#------------------------------------------------------------------------------------------
# Get number of individuals in data set - 9997 people in the data
nlevels(df$DebtDimId)

# Count NA in data by column
nas <-sapply(df, function(y) sum(length(which(is.na(y)))))
nas <- data.frame(nas)

# Count total NA in data
sum(is.na(df))

# Correlation Matrix
res <- cor(na.omit(df[, c(6, 8:20, 24:26)]))
round(res, 2)
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 75)

# Get top reasons for closure
# Count factor level freq
closure <- as.data.frame(table(df$ClosureReason))
# Change names
names(closure) <- c("Reason", "Count")
# Reorder from most to least
closure <- closure[order(-closure$Count), ]
# Only plot top 10 not including null or blank
closure <- closure[3:13, ]

# Plot top ten closure reasons
ggplot(closure, aes(x = closure$Reason, y = closure$Count, fill = closure$Reason)) +
  geom_bar(stat = 'identity') +
  ggtitle("Barplot of Top 10 Closure Reasons") +
  labs(x = "Closure Reason", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24)) +
  guides(fill=FALSE)

# Counts of External Status
ggplot(df, aes(df$`External Status`, fill = df$`External Status`)) +
  geom_bar(stat = 'count') +
  ggtitle("Counts of External Status") +
  labs(x = "External Status", y = "Count")

# Histogram of Months on Books by External Status Discluding Good Customers
temp <- df[which(!df$`External Status` == ""), ]
ggplot(temp, aes(temp$`Months On Book`, fill = temp$`External Status`)) +
  geom_histogram() +
  ggtitle("Histogram of Months on Books") +
  labs(x = "External Status", y = "Count") +
  facet_wrap(temp$`External Status` ~ .) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24))

# Histogram of Months on Book for Good Customers
temp <- df[which(df$`External Status` == ""), ]
ggplot(temp, aes(temp$`Months On Book`)) +
  geom_histogram(fill = "lightblue", color = "blue") +
  ggtitle("Histogram of Months on Books for Good Customers") +
  labs(x = "Months on Books", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24))

# Histogram of Days Delq - Overall Data
ggplot(df, aes(df$`Days Deliq`)) +
  geom_histogram(bins = 9, fill = "lightblue", color = "blue") +
  ggtitle("Histogram of Days Deliquent") +
  labs(x = "Days Deliquent", y = "Count")

# Histogram of Days Delq - By External Status
ggplot(df, aes(df$`Days Deliq`, fill = df$`External Status`)) +
  geom_histogram(bins = 9) +
  ggtitle("Histogram of Days Deliquent") +
  labs(x = "Days Deliquent", y = "Count") +
  facet_wrap(df$`External Status` ~ .)

# Create df for those with > 0 days deliq
dd <- df[which(df$`Days Deliq` > 0),]

# Boxplot for DD > 0 - By External Status
ggplot(dd, aes(y = dd$`Days Deliq`, fill = dd$`External Status`)) +
  geom_boxplot() +
  facet_wrap(dd$`External Status` ~ .) +
  ggtitle("Days Deliquent Above Zero by External Status") +
  labs(y = "Days Deliquent") +
  theme(axis.text.x = element_blank(), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24))

# Barplot of Days Deliquent by External Status 
ggplot(dd, aes(dd$`External Status`, fill = dd$`External Status`)) +
  geom_bar(stat = 'count') +
  ggtitle("Barplot of Days Deliquent") +
  labs(x = "Days Deliquent", y = "Count")

# Histogram of Good Customer Score
ggplot(df, aes(df$`Good Customer Score`)) +
  geom_histogram(fill = "lightblue", color = "blue") +
  ggtitle("Distribution of Good Customer Score") +
  labs(x = "Good Customer Score", y = "Count")

# Histogram of Behavior Score
ggplot(df, aes(df$`Behavior Score`)) +
  geom_histogram(fill = "lightblue", color = "blue", bins = 30) +
  ggtitle("Distribution of Behavior Score") +
  labs(x = "Behavior Score", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24))

# Scatterplot of credit limit vs months on books
ggplot(df, aes(df$`Months On Book`, df$`Good Customer Score`)) +
  geom_point() +
  ggtitle("Month on Books vs Credit Limit") +
  labs(x = "Months on Books", y = "Credit Limit")

# Get all those over credit limit
summary(df$`Over limit Amount`)

temp <- df[which(df$`Over limit Amount` > 20), ]

ggplot(temp, aes(temp$`Over limit Amount`, fill = temp$`External Status`)) +
  geom_histogram(bins = 20) +
  ggtitle("Distribution of Amount Over Limit") +
  labs(x = "Amount Over Limit", y = "Count") +
  facet_wrap(temp$`External Status` ~ .)

# Plot histograms of each 'bad' external status
temp <- df[which(df$`External Status` == 'I'), ]

# I
ggplot(temp, aes(temp$`Over limit Amount`)) +
  geom_histogram(fill = "lightblue", color = "blue") +
  ggtitle("Distribution of Amount Over Limit for Interest Prohibited") +
  labs(x = "Amount Over Limit", y = "Count")

temp <- df[which(df$`External Status` == 'E'), ]

# E
ggplot(temp, aes(temp$`Over limit Amount`)) +
  geom_histogram(fill = "lightblue", color = "blue") +
  ggtitle("Distribution of Amount Over Limit for Revoked") +
  labs(x = "Amount Over Limit", y = "Count")

temp <- df[which(df$`External Status` == 'F'), ]

# F
ggplot(temp, aes(temp$`Over limit Amount`)) +
  geom_histogram(fill = "lightblue", color = "blue") +
  ggtitle("Distribution of Amount Over Limit for Frozen") +
  labs(x = "Amount Over Limit", y = "Count")

# Behavior Score by Exernal Status
ggplot(df, aes(df$`Behavior Score`, fill = df$`External Status`)) +
  geom_histogram(bins = 20) +
  ggtitle("Distribution of Behavior Score") +
  labs(x = "Behavior Score", y = "Count") +
  facet_wrap(df$`External Status` ~ .)

# Scatterplot of FICO vs Months on Books
# All data
ggplot(df, aes(df$`Months On Book`, df$`Quarterly Fico Score`, color = df$`External Status`)) +
  geom_point() +
  ggtitle("Months on Books vs Quarterly FICO Score") +
  labs(x = "Months on Books", y = "FICO Score") +
  theme(plot.title = element_text(size=26))

# Only data greater than 0
temp <- df[which(df$`Quarterly Fico Score` > 0), ]
ggplot(temp, aes(temp$`Months On Book`, temp$`Quarterly Fico Score`, color = temp$`External Status`)) +
  geom_point() +
  ggtitle("Months on Books vs Quarterly FICO Score") +
  labs(x = "Months on Books", y = "FICO Score") +
  theme(plot.title = element_text(size=26))
