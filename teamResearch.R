## Setting the working directory to where the dataset is!

getwd()
setwd("G:/team-research/ds&code")

## Importing the Dataset!
dataset <- read.csv("./Bitcoin CZ.csv") 

## Display the first 2 rows of the dataset!
head(dataset, 2)

## Running some descriptive Stats on our dependent and independent variables!

print("The data covers a time range or interval of:")
min_timestamp <- min(dataset$Date)
max_timestamp <- max(dataset$Date)

cat("Minimum timestamp:", min_timestamp, "\n")
cat("Maximum timestamp:", max_timestamp, "\n")

print('This shows we are dealing with 3 years of Historical Data!!')

## Total number of Records!!
total_records <- nrow(dataset)

# Printing the total number of records
cat("Total number of records in the dataset:", total_records, "\n")

print('Our dependent Variable is Close(price) for any particular day')

print('Our Independent Variable is Open(price) for any particular day')


## Lets deal with null Values first!

## Lets check a Volume value of zero that indicates periods of no trading activity, 
## which might be useless still we will keep these records as we are only concerned 
## with the open and close!!
zero_count<- sum(dataset$Volume==0)

cat("Number of Days with no trading activity are: ",zero_count)


# Count rows where 'Open' has "null"
null_open_count <- sum(dataset$Open == "null")

# Count rows where 'Close' has "null"
null_close_count <- sum(dataset$Close == "null")

# Print the results
cat("Rows with 'null' in 'Open':", null_open_count, "\n")
cat("Rows with 'null' in 'Close':", null_close_count, "\n")

# As significant amount of rows have these null values we 
# use Replace Missing Values with Surrounding Non-Null Average.
install.packages("zoo")
library(zoo)

# Replace "null" with NA
dataset[dataset == "null"] <- NA

# Replace NA values with surrounding average in 'Open' column
dataset$Open <- na.approx(dataset$Open, na.rm = FALSE)

# Replace NA values with surrounding average in 'Close' column
dataset$Close <- na.approx(dataset$Close, na.rm = FALSE)

# Checking the null count again 
null_open_count_again <- sum(dataset$Open == "null")
null_open_count_again
null_close_count_again <- sum(dataset$Close == "null")
null_close_count_again

# Descriptive stats about our dependent and independent variables:
summary(dataset$Open)
summary(dataset$Close)

 # Histogram for the dependent variable to see data spread!
hist(dataset$Close, 
     main = "Distribution of Bitcoin Open Prices (2019-2022)", 
     xlab = "Close Prices (Price of 1 Satoshi: 1btc price/100,000,000)", 
     col = "red", 
     border = "white", 
     probability = TRUE, 
     breaks = 50)  # Adjust this number for finer bins


# Fit an exponential decay curve to the histogram
# Define the exponential function (decay)
decay_function <- function(x, a, b) {
  a * exp(-b * x)
}

# Fit the model (using nonlinear least squares fitting)
fit <- nls(density ~ decay_function(x, a, b), 
           data = data.frame(x = hist(dataset$Close, plot = FALSE)$mids,
                             density = hist(dataset$Close, plot = FALSE)$density),
           start = list(a = 1, b = 0.001))  

# Extract the fitted parameters
params <- coef(fit)

# Add the exponential decay line to the histogram
curve(params["a"] * exp(-params["b"] * x), 
      col = "black", 
      add = TRUE,
      lwd=2)