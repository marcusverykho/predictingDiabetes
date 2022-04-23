# Load Installed Libraries
library(psych)
library(reshape2)
library(ggplot2)

# Functions
# Get lower triangle of the correlation matrix
get_lower_tri <- function(matrix){
  matrix[upper.tri(matrix)] <- NA
  return(matrix)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(matrix){
  matrix[lower.tri(matrix)]<- NA
  return(matrix)
}

# Reorders correlation matrix using correlation between variables as distance
reorder_matrix <- function(matrix){
  dd = as.dist((1-matrix)/2)
  hc = hclust(dd)
  matrix = matrix[hc$order, hc$order]
}

# Read data set from CSV into data frame
df = read.csv("Cleaned_Diabetes.csv")

# Removes numeric variables from data frame to be used in Tetrachoric Correlation
# 16 & 17 for men and phy health
df2 = df[-c(5, 15, 20, 21, 22)]
matrix = tetrachoric(df2)[[1]]

# Clean the matrix by getting only the upper triangle then format the matrix
upper_tri = get_upper_tri(matrix)
matrix = melt(upper_tri, na.rm = TRUE)

# Create heat map
ggheatmap = ggplot(matrix, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Tetrachoric\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Format heat map
ggheatmap + 
  geom_text(aes(Var2, Var1, label = sprintf("%0.2f", round(value, digits = 2))), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))