df <- read.csv("C:/Users/sakal/Documents/ISMF/Sem 6/ST 3082-Statistical Learning I/EDA project/train_cleaned_1.csv")
summary(df)
colSums(is.na(df))
# Check for duplicate rows in the dataset
duplicates <- duplicated(df)

# View duplicate rows (if any)
duplicate_rows <- df[duplicates, ]

# Count the number of duplicate rows
num_duplicates <- sum(duplicates)

# Print results
if (num_duplicates > 0) {
  print(paste("Number of duplicate rows:", num_duplicates))
  print("Duplicate rows:")
  print(duplicate_rows)
} else {
  print("No duplicate rows found in the dataset.")
}
