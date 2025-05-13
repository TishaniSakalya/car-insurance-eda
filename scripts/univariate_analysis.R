# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Read the dataset
df <- read.csv("C:/Users/sakal/Documents/ISMF/Sem 6/ST 3082-Statistical Learning I/EDA project/train_cleaned_1.csv")

# 1. Pie Chart for Claim Proportion
claim_counts <- as.data.frame(table(df$is_claim))
colnames(claim_counts) <- c("Claim", "Count")
claim_counts$Percentage <- round(claim_counts$Count / sum(claim_counts$Count) * 100, 1)
claim_counts$Label <- paste0(claim_counts$Percentage, "%")

p1 <- ggplot(claim_counts, aes(x = "", y = Count, fill = factor(Claim))) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("0" = "lightgreen", "1" = "orange")) +
  theme_void() +
  labs(title = "Proportion of Claimants", fill = "is_claim") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "white", size = 5)

p1
# 2. Box Plot for Policy Tenure
p2 <- ggplot(df, aes(y = policy_tenure)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(title = "Box Plot of Policy Tenure", y = "Policy Tenure")

p2

# 3. Box Plot for Age of Policyholder
p3 <- ggplot(df, aes(y = age_of_policyholder)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(title = "Box Plot of Age of Policyholder", y = "Age of Policyholder")

p3

# 4. Bar Chart for Policyholders by Area
area_counts <- as.data.frame(table(df$area))
colnames(area_counts) <- c("Area", "Count")
area_counts$Percentage <- (area_counts$Count / sum(area_counts$Count)) * 100
area_counts$Label <- paste0(round(area_counts$Percentage, 1), "%")

p4 <- ggplot(area_counts, aes(x = reorder(Area, -Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  geom_text(aes(label = Label), vjust = -0.5, size = 2.5) +
  theme_minimal() +
  labs(title = "Distribution of Policyholders by Area", x = "Area", y = "Percentage (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4

# 5. Box Plot for Age of Car
p5 <- ggplot(df, aes(y = age_of_car)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(title = "Box Plot of Age of Car", y = "Age of Car")

p5

# 6. Bar Chart for Policyholders by Model
model_counts <- as.data.frame(table(df$model))
colnames(model_counts) <- c("Model", "Count")
model_counts$Percentage <- (model_counts$Count / sum(model_counts$Count)) * 100
model_counts$Label <- paste0(round(model_counts$Percentage, 1), "%")

p6 <- ggplot(model_counts, aes(x = reorder(Model, -Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  geom_text(aes(label = Label), vjust = -0.5, size = 2.5) +  # Reduce percentage label size
  theme_minimal() +
  labs(title = "Distribution of Policyholders by Model", x = "Model", y = "Percentage (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))  # Keep x-axis text readable

p6

# Arrange all six plots in a grid
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3, nrow =2)
