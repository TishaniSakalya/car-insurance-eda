
#Load dataset
df <- read.csv("C:/Users/sakal/Documents/ISMF/Sem 6/ST 3082-Statistical Learning I/EDA project/df_numeric.csv")
View(df)

#Univariate

# Function to detect outliers using IQR
detect_outliers_IQR <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- which(x < lower_bound | x > upper_bound)
  return(outliers)
}

# Apply the function to each variable
outliers_list <- lapply(df, detect_outliers_IQR)

# Print outliers for each variable
outliers_list

# Display outliers and count for each variable
for (variable in names(outliers_list)) {
  outliers <- outliers_list[[variable]]
  
  if (length(outliers) > 0) {
    cat(paste("\nOutliers for", variable, ":\n"))
    print(outliers)
    cat(paste("\nNumber of outliers for", variable, ":", length(outliers), "\n"))
  } else {
    cat(paste("\nNo outliers detected for", variable, "\n"))
  }
}

# Load necessary libraries
library(ggplot2)

# Create a new data frame to plot only age_of_car and age_of_policyholder
df_outliers <- data.frame(
  Variable = rep(c("age_of_car", "age_of_policyholder"), times = c(length(outliers_list$age_of_car), length(outliers_list$age_of_policyholder))),
  Value = c(df$age_of_car[outliers_list$age_of_car], df$age_of_policyholder[outliers_list$age_of_policyholder]),
  Outlier = c(rep(TRUE, length(outliers_list$age_of_car)), rep(TRUE, length(outliers_list$age_of_policyholder)))
)

# Plotting outliers for "age_of_car" and "age_of_policyholder"
ggplot(df_outliers, aes(x = Variable, y = Value, color = Outlier)) +
  geom_boxplot(outlier.shape = NA, fill = "lightgray", color = "black") +  # Boxplot without default outliers
  geom_jitter(width = 0.1, size = 1, aes(color = Outlier)) +  # Add jitter points for outliers
  scale_color_manual(values = c("red")) +  # Red for outliers
  labs(title = "Outliers for Age of Car and Age of Policyholder", x = "Variable", y = "Value", color = "Outlier") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since it's clear from color


#Multivariate

#install.packages("isotree")
library(isotree)

# Apply Isolation Forest
iso_forest <- isolation.forest(df, ntrees = 100)  # Adjust ntrees for better performance

# Get anomaly scores (outlier score: the higher the score, the more anomalous)
anomaly_scores <- predict(iso_forest, df, type = "score")

# Add anomaly scores to the data
df$anomaly_score <- anomaly_scores

# Define a threshold for outliers (e.g., top 5% of anomaly scores are outliers)
threshold <- quantile(anomaly_scores, 0.95)

# Mark the outliers (those with an anomaly score above the threshold)
df$outlier <- ifelse(anomaly_scores > threshold, 1, 0)

# Get total number of outliers
total_outliers <- sum(df$outlier)

# Print the total number of outliers
cat("Total number of outliers: ", total_outliers, "\n")

# Calculate percentage of outliers
total_observations <- nrow(df)
percentage_outliers <- (total_outliers / total_observations) * 100

# Print percentage of outliers
cat("Percentage of outliers: ", percentage_outliers, "%", "\n")

# Load necessary library for plotting
library(ggplot2)

# Plot using ggplot2 (you can select any two variables for the plot)
ggplot(df, aes(x = df[, 1], y = df[, 2], color = factor(outlier))) +
  geom_point() +
  labs(title = "Outlier Detection using Isolation Forest",
       x = colnames(df)[1],
       y = colnames(df)[2],
       color = "Outlier") +
  scale_color_manual(values = c("blue", "red"))  # Outliers in red, inliers in blue

table(df$outlier)  # Should show the count of outliers (1) vs inliers (0)

