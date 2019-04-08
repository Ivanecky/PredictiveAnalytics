#------------------------------------------------------------------------------------------
# LOAD LIBRARIES
#------------------------------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(ggplot2)
library(caret)
library(lubridate)
library(rpart)
library(maptree)
library(forcats)
library(gridExtra)

#------------------------------------------------------------------------------------------
# LOAD DATA
#------------------------------------------------------------------------------------------
df <- fread("RetentionDataRaw.csv")

#------------------------------------------------------------------------------------------
# DATA SETUP
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

# Convert dates to Date Type
df$`Open Date` <- mdy(df$`Open Date`)
df$`Last Statement Date` <- mdy(df$`Last Statement Date`)
df$`Cycle Date` <- mdy(df$`Cycle Date`)
df$`Month End Date` <- mdy(df$`Month End Date`)
df$`Last Payment Date` <- mdy(df$`Last Payment Date`)

# Convert customer score to numeric
df$`Good Customer Score` <- as.numeric(df$`Good Customer Score`)

# Remove empty rows at end of file
df <- df[!is.na(df$DebtDimId), ]

#------------------------------------------------------------------------------------------
# FUNCTIONS
#------------------------------------------------------------------------------------------
# Function to plot distribution by days deliq
deliq_dist <- function(x){
  # Create temporary data frame
  temp <- df[which(df$`Days Deliq` > x), ]
  
  # Print barplot of external status
  ggplot(temp, aes(temp$`External Status`, fill = temp$`External Status`)) +
    geom_bar(stat = "count") +
    ggtitle(paste("Distribution of Days Over ="), x) +
    labs(x = "External Status", y = "Count")
}

#------------------------------------------------------------------------------------------
# DAYS DELINQUENT - INCREMENTAL
#------------------------------------------------------------------------------------------
# Print plots of distributions in 30 day intervals
deliq_dist(30) -> a
deliq_dist(60) -> b
deliq_dist(90) -> c
deliq_dist(120) -> d

# Create grid of plots & plot
grid.arrange( a, b, c, d, nrow = 2, ncol = 2 )

# Cumulative distribution of days deliq - all data
ggplot(df, aes(df$`Days Deliq`, colour = df$`External Status`)) + 
  stat_ecdf(geom = 'step') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24)) +
  ggtitle("Cumulative Distribution of Days Delinquent by Status Level") +
  labs(x = "Days Delinquent", y = "Cumulative Distribution")

#------------------------------------------------------------------------------------------
# DAYS DELINQUENT - ZERO
#------------------------------------------------------------------------------------------
# Subset data
temp <- df[`Days Deliq` == 0]

# Plot barplot
ggplot(temp, aes(temp$`External Status`, fill = temp$`External Status`)) +
  geom_bar(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24)) +
  ggtitle("Distribution of Status for Zero Days Delinquent") +
  labs(x = "External Status", y = "Count")

#------------------------------------------------------------------------------------------
# CONDENSE DATA TO ONE ROW PER CUSTOMER
#------------------------------------------------------------------------------------------
# Have to specify certain IDs which do not have a Row Num = 1
mod.df <- df[`Row Num` == 1 | (DebtDimId == "10336095" & `Row Num` == 2) | (DebtDimId == "18492458" & `Row Num` == 2) |
                 (DebtDimId == "18757462" & `Row Num` == 2) | (DebtDimId == "19487137" & `Row Num` == 2)
               | (DebtDimId == "21170332" & `Row Num` == 2)]

# Remove duplicate rows
mod.df <- mod.df[!duplicated(mod.df$DebtDimId), ]

#------------------------------------------------------------------------------------------
# CLASSIFY CUSTOMERS AS BAD / NOT BAD
#------------------------------------------------------------------------------------------
# Customers who have an account status not open or are at least 90 days delinquent are to be considered bad customers
# Create vector to hold classifications
Bad.vec <- vector()
# Create vectors to hold days delinquent and status level
DD.vec <- df$`Days Deliq`
Status.vec <- df$`External Status`

# Loop through and assign Bad
for ( i in 1:length(DD.vec) )
{
  # Check if a non-blank status exists
  if( Status.vec[i] != "" )
  {
    # Assign as Bad
    Bad.vec[i] <- 1
  }
  # OR check if >= 90 days delinquent
  else if ( DD.vec[i] >= 90 )
  {
    # Assign as Bad
    Bad.vec[i] <- 1
  }
  # If neither met, assign as 0
  else
  {
    Bad.vec[i] <- 0
  }
}

# Create variable in data frame and assign values using vector holding values
df$Bad <- Bad.vec

# Aggregate data and take max of Bad for each customer. That way if they ever go bad we bin them as such.
agg_df <- aggregate(df$Bad, by=list(df$DebtDimId), max)

# Assign Bad column in condensed data using aggregated data
mod.df$Bad <- agg_df$x

# Convert Bad to factor variable
mod.df$Bad <- as.factor(mod.df$Bad)

#------------------------------------------------------------------------------------------
# PLOT BAD VS GOOD COUNTS
#------------------------------------------------------------------------------------------
# Simple barplot
ggplot(mod.df, aes(mod.df$Bad, fill = mod.df$Bad)) +
  geom_bar(stat = "count") +
  ggtitle("Distribution of Good vs Bad Customers") +
  labs(x = "Bad", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=34, face = "bold"),
        axis.title=element_text(size=34,face="bold"), plot.title = element_text(size=44))

# Get Bad distribution
table(mod.df$Bad)

#------------------------------------------------------------------------------------------
# CREATE NETPURCHASESTOTAL AND NETPROFIT
#------------------------------------------------------------------------------------------
# Aggregate data
agg_df <- aggregate(df$`Net Purchases During Cycle`, by=list(df$DebtDimId), sum)

# Create variable for NetPurchasesTotal
mod.df$NetPurchasesTotal <- agg_df$x

# Aggregate profit good and bad
agg_profit_good <- aggregate(df$`Net Payments During Cycle`, by=list(df$DebtDimId), sum)
agg_profit_bad <- aggregate(df$`Ending Balance`, by=list(df$DebtDimId), sum)

# Create vectors with good and bad profits
good_profit <- agg_profit_good$x
bad_profit <- (agg_profit_bad$x)*(-1)

# Add profit values to clean_data
mod.df$NetProfit <- 0

# Vectors to hold data
bad_col <- mod.df$Bad
prof_vec <- mod.df$NetProfit

for( i in 1:length(bad_col) )
{
  # If customer is not bad, assign good profit
  if( bad_col[i] == 0 )
  {
    prof_vec[i] <- good_profit[i]
  }
  # Else, customer is bad and assign bad profit
  else
  {
    prof_vec[i] <- bad_profit[i]
  }
}

# Reassign NetProfit 
mod.df$NetProfit <- prof_vec

#------------------------------------------------------------------------------------------
# DISTRIBUTION OF NETPURCHASESTOTAL
#------------------------------------------------------------------------------------------
# Simple boxplot
ggplot(mod.df, aes(y = mod.df$NetPurchasesTotal, fill = mod.df$Bad)) +
  geom_boxplot() +
  ggtitle("Distribution of Net Purchases Total") +
  labs(y = "Total Net Purchases") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=34, face = "bold"),
        axis.title=element_text(size=34,face="bold"), plot.title = element_text(size=44), legend.title=element_text(size=30), 
        legend.text=element_text(size=30)) +
  coord_flip()

# Get summary of good versus bad
summary(mod.df[Bad == 1]$NetPurchasesTotal)
summary(mod.df[Bad == 0]$NetPurchasesTotal)

#------------------------------------------------------------------------------------------
# DISTRIBUTION OF NETPROFIT
#------------------------------------------------------------------------------------------
# Simple boxplot
ggplot(mod.df, aes(y = mod.df$NetProfit, fill = mod.df$Bad)) +
  geom_boxplot() +
  ggtitle("Distribution of Net Profit") +
  labs(y = "Net Profit") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24)) +
  coord_flip()

# Get summary of good versus bad
summary(mod.df[Bad == 1]$NetProfit)
summary(mod.df[Bad == 0]$NetProfit)

#------------------------------------------------------------------------------------------
# BEHAVIOR SCORE
#------------------------------------------------------------------------------------------
# Simple barplot
ggplot(mod.df, aes(y = mod.df$`Behavior Score`, fill = mod.df$Bad)) +
  geom_boxplot() +
  ggtitle("Distribution of Behavior Score") +
  labs(y = "Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24)) +
  coord_flip()

ggplot(mod.df, aes(mod.df$`Behavior Score`, fill = mod.df$Bad)) +
  geom_histogram(binwidth = 5) +
  facet_grid(mod.df$Bad ~ .) +
  ggtitle("Distribution of Behavior Score") +
  labs(x = "Behavior Score", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24))

#------------------------------------------------------------------------------------------
# FICO SCORE
#------------------------------------------------------------------------------------------
# Simple barplot
ggplot(mod.df, aes(y = mod.df$`Quarterly Fico Score`, fill = mod.df$Bad)) +
  geom_boxplot() +
  ggtitle("Distribution of FICO Score") +
  labs(y = "FICO Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24)) +
  coord_flip()

ggplot(mod.df, aes(mod.df$`Quarterly Fico Score`, fill = mod.df$Bad)) +
  geom_histogram(binwidth = 5) +
  facet_grid(mod.df$Bad ~ .) +
  ggtitle("Distribution of FICO Score") +
  labs(x = "Score", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24))

#------------------------------------------------------------------------------------------
# GOOD CUSTOMER SCORE
#------------------------------------------------------------------------------------------
# Simple barplot
ggplot(mod.df, aes(y = mod.df$`Good Customer Score`, fill = mod.df$Bad)) +
  geom_boxplot() +
  ggtitle("Distribution of Good Customer Score") +
  labs(y = "Good Customer Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24)) +
  coord_flip()

ggplot(mod.df, aes(mod.df$`Good Customer Score`, fill = mod.df$Bad)) +
  geom_histogram(binwidth = 25) +
  facet_grid(mod.df$Bad ~ .) +
  ggtitle("Distribution of Good Customer Score") +
  labs(x = "Score", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24))

#------------------------------------------------------------------------------------------
# MONTHS ON BOOKS
#------------------------------------------------------------------------------------------
# Simple barplot
ggplot(mod.df, aes(y = mod.df$`Months On Book`, fill = mod.df$Bad)) +
  geom_boxplot() +
  ggtitle("Distribution of Months on Books") +
  labs(y = "Months on Books") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24)) +
  coord_flip()

ggplot(mod.df, aes(mod.df$`Months On Book`, fill = mod.df$Bad)) +
  geom_histogram(binwidth = 5) +
  facet_grid(mod.df$Bad ~ .) +
  ggtitle("Distribution of Months on Books") +
  labs(x = "Months on Books", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24))

#------------------------------------------------------------------------------------------
# OVER LIMIT AMOUNT
#------------------------------------------------------------------------------------------
# Simple barplot
ggplot(mod.df, aes(y = mod.df$`Over limit Amount`, fill = mod.df$Bad)) +
  geom_boxplot() +
  ggtitle("Distribution of Over Limit Amount") +
  labs(y = "Over Limit Amount") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24)) +
  coord_flip()

ggplot(mod.df, aes(mod.df$`Over limit Amount`, fill = mod.df$Bad)) +
  geom_histogram(binwidth = 5) +
  facet_grid(mod.df$Bad ~ .) +
  ggtitle("Distribution of Over Limit Amount") +
  labs(x = "Over Limit Amount", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24))

ggplot(mod.df, aes(mod.df$`Over limit Amount`, colour = mod.df$Bad)) + 
  stat_ecdf(geom = 'step') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24)) +
  ggtitle("Cumulative Distribution of Payment Ratio by External Status") +
  labs(x = "Ratio of Payments Made to Minimum Owed", y = "Cumulative Distribution")

#------------------------------------------------------------------------------------------
# CREATE PAY RATIO VARIABLE
#------------------------------------------------------------------------------------------
# Create ratio of net payments to minimum owed
mod.df$pay_ratio <- mod.df$`Net Payments During Cycle`/ mod.df$`Total Min Pay Due`

# Plot
ggplot(mod.df, aes(mod.df$pay_ratio, colour = mod.df$Bad)) + 
  stat_ecdf(geom = 'step', aes(size = 1.25)) +
  coord_cartesian(xlim = c(0,2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=34, face = "bold"),
        axis.title=element_text(size=34,face="bold"), plot.title = element_text(size=44), legend.title=element_text(size=30), 
        legend.text=element_text(size=30)) +
  ggtitle("Cumulative Distribution of Payment Ratio") +
  labs(x = "Ratio of Payments Made to Minimum Owed", y = "Cumulative Distribution")

#------------------------------------------------------------------------------------------
# CREATE BALANCE RATIO VARIABLE
#------------------------------------------------------------------------------------------
# Create ratio of net purchases to net payments variable
mod.df$bal_ratio <- mod.df$`Credit Limit`/ (mod.df$`Opening Balance` + 0.00000000000001)

# Plot
ggplot(mod.df, aes(mod.df$bal_ratio, colour = mod.df$Bad)) + 
  stat_ecdf(geom = 'step', aes(size=1.25)) +
  coord_cartesian(xlim = c(0,3)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=34, face = "bold"),
        axis.title=element_text(size=34,face="bold"), plot.title = element_text(size=44), legend.title=element_text(size=30), 
        legend.text=element_text(size=30)) +
  ggtitle("Cumulative Distribution of Balance Ratio") +
  labs(x = "Ratio of Credit Limit to Opening Balance", y = "Cumulative Distribution")

#------------------------------------------------------------------------------------------
# CREATE PROPORTION OVER LIMIT VARIABLE
#------------------------------------------------------------------------------------------
# Create variable for proportion over the limit
mod.df$PropOverLimit <- mod.df$`Over limit Amount` / mod.df$`Credit Limit`

# Plot
ggplot(mod.df, aes(mod.df$PropOverLimit, colour = mod.df$Bad)) + 
  stat_ecdf(geom = 'step', aes(size = 1.25)) +
  coord_cartesian(xlim = c(0,1.2))  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=34, face = "bold"),
        axis.title=element_text(size=34,face="bold"), plot.title = element_text(size=44), legend.title=element_text(size=30), 
        legend.text=element_text(size=30)) +
  ggtitle("Cumulative Distribution of Proportion Over Limit") +
  labs(x = "Proportion Over Limit", y = "Cumulative Distribution")

#------------------------------------------------------------------------------------------
# CREDIT LIMIT
#------------------------------------------------------------------------------------------
# Simple barplot
ggplot(mod.df, aes(y = mod.df$`Credit Limit`, fill = mod.df$Bad)) +
  geom_boxplot() +
  ggtitle("Distribution of Credit Limit") +
  labs(y = "Credit Limit") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24)) +
  coord_flip()

ggplot(mod.df, aes(mod.df$`Credit Limit`, fill = mod.df$Bad)) +
  geom_histogram(binwidth = 50) +
  facet_grid(mod.df$Bad ~ .) +
  ggtitle("Distribution of Credit Limit") +
  labs(x = "Credit Limit", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24))

ggplot(mod.df, aes(mod.df$`Credit Limit`, colour = mod.df$Bad)) + 
  stat_ecdf(geom = 'step') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24)) +
  ggtitle("Cumulative Distribution of Credit Limit") +
  labs(x = "Credit Limit", y = "Cumulative Distribution")

ggplot(mod.df, aes(mod.df$`Months On Book`, mod.df$`Credit Limit`, colour = mod.df$Bad)) +
  geom_count()

#------------------------------------------------------------------------------------------
# LIMIT RATIO
#------------------------------------------------------------------------------------------
# Create variable
mod.df$LimitRatio <- mod.df$`Credit Limit` / mod.df$`Months On Book`

# Simple barplot
ggplot(mod.df, aes(y = mod.df$LimitRatio, fill = mod.df$Bad)) +
  geom_boxplot() +
  ggtitle("Distribution of Limit Ratio") +
  labs(y = "Limit Ratio") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24)) +
  coord_flip()

ggplot(mod.df, aes(mod.df$LimitRatio, fill = mod.df$Bad)) +
  geom_histogram(binwidth = 50) +
  facet_grid(mod.df$Bad ~ .) +
  ggtitle("Distribution of Limit Ratio") +
  labs(x = "Limit Ratio", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24))

ggplot(mod.df, aes(mod.df$LimitRatio, colour = mod.df$Bad)) + 
  stat_ecdf(geom = 'step') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24)) +
  ggtitle("Cumulative Distribution of Limit Raitio") +
  labs(x = "Limit Ratio", y = "Cumulative Distribution")

#------------------------------------------------------------------------------------------
# NET BEHAVIOR FEES
#------------------------------------------------------------------------------------------
# Simple barplot
ggplot(mod.df, aes(y = mod.df$`Net Behavior Fees Billed During Cycle`, fill = mod.df$Bad)) +
  geom_boxplot() +
  ggtitle("Distribution of Behavior Fees") +
  labs(y = "Behavior Fees")  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=34, face = "bold"),
        axis.title=element_text(size=34,face="bold"), plot.title = element_text(size=44), legend.title=element_text(size=30), 
        legend.text=element_text(size=30)) +
  coord_flip()

# Summary of Behavior Fees by Good / Bad
# Get summary of good versus bad
summary(mod.df[Bad == 1]$`Net Behavior Fees Billed During Cycle`)
summary(mod.df[Bad == 0]$`Net Behavior Fees Billed During Cycle`)

ggplot(mod.df, aes(mod.df$`Net Behavior Fees Billed During Cycle`, fill = mod.df$Bad)) +
  geom_histogram(binwidth = 2.5) +
  facet_grid(mod.df$Bad ~ .) +
  ggtitle("Distribution of Fees") +
  labs(x = "Fees", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24))

ggplot(mod.df, aes(mod.df$`Net Behavior Fees Billed During Cycle`, colour = mod.df$Bad)) + 
  stat_ecdf(geom = 'step') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=24)) +
  ggtitle("Cumulative Distribution of Fees") +
  labs(x = "Fees", y = "Cumulative Distribution")

#------------------------------------------------------------------------------------------
# CREATE DATA FILE WITH ONE ROW PER CUSTOMER AND ONLY INCLUDE NECESSARY VARIABLES
#------------------------------------------------------------------------------------------
# Subset clean_data columns
final.df <- mod.df[, c(1, 6:8, 16, 19, 27:32)]

# Save final data to CSV file
write.csv(final.df, file = "FinalModelData.csv")
