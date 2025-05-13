# Install and load required packages (if not already installed)
# install.packages("FactoMineR")
# install.packages("factoextra")
# install.packages("ggplot2")

library(FactoMineR)
library(factoextra)
library(ggplot2)

# Load datasets
df_cat <- read.csv("C:/Users/sakal/Documents/ISMF/Sem 6/ST 3082-Statistical Learning I/EDA project/df_cat.csv")

# Convert categorical variables to factors
df_cat <- data.frame(lapply(df_cat, as.factor))
View(df_cat)

# Select the desired columns
df_selected <- df_cat[, c(2,3)]

View(df_selected)

# Apply MCA
mca_result <- MCA(df_selected, graph = TRUE)

# Scree plot
fviz_screeplot(mca_result, addlabels = TRUE, ylim = c(0, 50))

#fviz_mca_ind(mca_result, repel = TRUE, col.ind = "cos2")
fviz_mca_var(mca_result, repel = TRUE)
#fviz_mca_biplot(mca_result, repel = TRUE)
