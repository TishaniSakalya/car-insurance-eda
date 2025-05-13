
#Load dataset
df <- read.csv("C:/Users/sakal/Documents/ISMF/Sem 6/ST 3082-Statistical Learning I/EDA project/df_cat.csv")
View(df)

# Function to find rare categories in categorical variables
find_rare_categories <- function(df_cat, threshold = 0.01) {
  rare_categories <- list()
  
  for (col in colnames(df_cat)) {
    category_counts <- table(df_cat[[col]])  # Count occurrences
    category_percent <- prop.table(category_counts)  # Convert to proportions
    
    # Filter categories below the threshold
    rare <- names(category_percent[category_percent < threshold])
    
    if (length(rare) > 0) {
      rare_categories[[col]] <- rare
    }
  }
  
  return(rare_categories)
}

# Apply function to your categorical dataframe
rare_cats <- find_rare_categories(df_cat)

# Print results
print(rare_cats)
