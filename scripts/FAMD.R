# Install and load required packages (if not already installed)
# install.packages("FactoMineR")
# install.packages("factoextra")
# install.packages("ggplot2")

library(FactoMineR)
library(factoextra)
library(ggplot2)

# Load datasets
df_cat <- read.csv("C:/Users/sakal/Documents/ISMF/Sem 6/ST 3082-Statistical Learning I/EDA project/df_cat.csv")
df_numeric <- read.csv("C:/Users/sakal/Documents/ISMF/Sem 6/ST 3082-Statistical Learning I/EDA project/df_numeric.csv")

# Convert categorical variables to factors
df_cat <- data.frame(lapply(df_cat, as.factor))

# Convert numeric variables to numeric
df_numeric <- data.frame(lapply(df_numeric, as.numeric))

# Check for missing values and remove them **after merging**
df_combined <- cbind(df_cat, df_numeric)
df_combined <- na.omit(df_combined)  # Remove any rows with missing values
View(df_combined)

# Select the desired columns
df_selected <- df_combined[, c(2,3,4,5,6)]

View(df_selected)

# Perform FAMD (Factor Analysis of Mixed Data)
famd_res <- FAMD(df_selected, graph = FALSE)

# === Scree Plot (Eigenvalues) ===
fviz_screeplot(famd_res, addlabels = TRUE, ylim = c(0, 50))

# === Print Eigenvalues ===
eig_values <- famd_res$eig
print(eig_values)

# === Cumulative Variance Explained Plot ===
plot(cumsum(eig_values[, 2]), type = "b", pch = 19,
     xlab = "Number of Factors", ylab = "Cumulative Percentage of Variance",
     main = "Cumulative Variance Explained")
abline(h = 80, col = "red", lty = 2)  # Reference line for 80% explained variance

# === Plot Individuals (Observations) ===
#fviz_famd_ind(famd_res, repel = TRUE)

# === Plot Variables Contribution to Factors ===
fviz_famd_var(famd_res, repel = TRUE)

# === Separate Numerical and Categorical Variables Contributions ===
fviz_famd_var(famd_res, choice = "quanti.var", repel = TRUE)  # Numerical
fviz_famd_var(famd_res, choice = "quali.var", repel = TRUE)   # Categorical

# === Variable Contribution Plot (Colored by Importance) ===
fviz_famd_var(famd_res, col.var = "contrib", gradient.cols = c("blue", "red"))
