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
