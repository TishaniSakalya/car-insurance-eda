# Install and load required packages (if not already installed)
# install.packages("FactoMineR")
# install.packages("factoextra")
# install.packages("ggplot2")

library(FactoMineR)
library(factoextra)
library(ggplot2)

# Load datasets
df_numeric <- read.csv("C:/Users/sakal/Documents/ISMF/Sem 6/ST 3082-Statistical Learning I/EDA project/df_numeric.csv")

# Convert numeric variables to numeric
df_numeric <- data.frame(lapply(df_numeric, as.numeric))
View(df_numeric)

# Select the desired columns
df_selected <- df_numeric[, c(1,2,3)]

View(df_selected)

# Apply PCA
pca_result <- PCA(df_selected, graph = TRUE)

# Scree plot
fviz_screeplot(pca_result, addlabels = TRUE, ylim = c(0, 50))

# Visualize PCA results
fviz_pca_var(pca_result, repel = TRUE)
#fviz_pca_ind(pca_result, repel = TRUE, col.ind = "cos2")
#fviz_pca_biplot(pca_result, repel = TRUE)