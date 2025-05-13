# Load necessary libraries

library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

train_data=read.csv("C:/Users/sakal/Documents/ISMF/Sem 6/ST 3082-Statistical Learning I/EDA project/train_cleaned_1.csv")
View(train_data)
train_data$is_claim <- factor(train_data$is_claim, levels = c(0, 1), labels = c("No", "Yes"))


# Prepare categorical and quantitative data into one dataframe
categorical_vars <- c("area_cluster", "model")
quantitative_vars <- c("policy_tenure", "age_of_car", "age_of_policyholder")

# Categorical Data Preparation
categorical_plots <- lapply(categorical_vars, function(var) {
  train_data[[var]] <- factor(train_data[[var]])  # Ensure factor levels
  summary_data <- train_data %>%
    group_by(!!sym(var), is_claim) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(!!sym(var)) %>%
    mutate(proportion = count / sum(count))  # Calculate proportions
  
  ggplot(summary_data, aes_string(x = var, y = "proportion", fill = "is_claim")) +
    geom_bar(stat = "identity", position = "fill") +
    geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), 
              position = position_fill(vjust = 0.5), size = 3, color = "black") +
    labs(title = paste("Distribution of", var, "by Claim Status"),
         x = var,
         y = "Proportion") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("#EE2E31", "#4DD091"), labels = c("No", "Yes")) +
    theme_minimal()
})

# Quantitative Data Preparation (Updated with smaller text size for IQR stats)
quantitative_plots <- lapply(quantitative_vars, function(var) {
  summary_stats <- train_data %>%
    group_by(is_claim) %>%
    summarise(
      Median = median(.data[[var]], na.rm = TRUE),
      Mean = mean(.data[[var]], na.rm = TRUE),
      IQR_Lower = quantile(.data[[var]], 0.25, na.rm = TRUE),
      IQR_Upper = quantile(.data[[var]], 0.75, na.rm = TRUE)
    )
  
  ggplot(train_data, aes_string(x = "is_claim", y = var, fill = "is_claim")) +
    geom_boxplot() +
    # Add Mean, Median, and IQR values above the plot with even smaller text size
    annotate("text", x = 1, y = max(train_data[[var]], na.rm = TRUE) * 1.1, 
             label = paste0("Median: ", round(summary_stats$Median[1], 2),
                            "\nIQR: [", round(summary_stats$IQR_Lower[1], 2), ", ", 
                            round(summary_stats$IQR_Upper[1], 2), "]"),
             color = "red", size = 1.5, hjust = 0) +  # Even smaller text size
    annotate("text", x = 2, y = max(train_data[[var]], na.rm = TRUE) * 1.1, 
             label = paste0("Median: ", round(summary_stats$Median[2], 2),
                            "\nIQR: [", round(summary_stats$IQR_Lower[2], 2), ", ", 
                            round(summary_stats$IQR_Upper[2], 2), "]"),
             color = "green", size = 1.5, hjust = 0) +  # Even smaller text size
    labs(title = paste("Distribution of", var, "by Claim Status"),
         x = "Claim Status",
         y = var) +
    scale_fill_manual(values = c("red", "green"), labels = c("No", "Yes")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

# Combine all plots into a single layout
library(gridExtra)

grid.arrange(grobs = c(categorical_plots, quantitative_plots), ncol = 2)


# Prepare the data
quantitative_data <- train_data %>%
  select(policy_tenure, age_of_car, age_of_policyholder)  # Select only numeric columns

# Calculate the correlation matrix
cor_matrix <- cor(quantitative_data, use = "complete.obs")  # Use complete cases only

# Reshape the correlation matrix for ggplot2
cor_matrix_melted <- melt(cor_matrix)

# Create a heatmap with values displayed on the heatmap
ggplot(cor_matrix_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix Heatmap", x = "", y = "") +
  coord_fixed() +  # Keep the aspect ratio square
  geom_text(aes(label = round(value, 2)), color = "black", size = 4)  



# Prepare the data
quantitative_data <- train_data %>%
  select(policy_tenure, age_of_car, age_of_policyholder)  # Select only numeric columns

# Calculate the covariance matrix
cov_matrix <- cov(quantitative_data, use = "complete.obs")  # Use complete cases only

# Reshape the covariance matrix for ggplot2
cov_matrix_melted <- melt(cov_matrix)

# Create a heatmap with covariance values displayed
ggplot(cov_matrix_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Covariance Matrix Heatmap", x = "", y = "") +
  coord_fixed() +  # Keep the aspect ratio square
  geom_text(aes(label = round(value, 2)), color = "black", size = 4)  # Add covariance values
