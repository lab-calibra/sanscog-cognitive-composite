library(tidyverse)
library(dplyr)

# Select COGNITO
data = read.csv("D:\\Kaggle\\composite-global\\SNI_COG_MRI_MeanImputation.csv")
sub_data = data[,c("EP11BR_GeometricFigures",
               "EP05BRT_VisualAttention",
               "EP04BRT_AuditoryAttention",
               "EP03BR_ReadingSyntaxComprehension",
               "EP17ET01BR_DelayedRecallNames")]

# Perform PCA
scaled_data = scale(sub_data)
pca = prcomp(scaled_data, center = FALSE, scale. = FALSE)
loadings = pca$rotation[,1]

# Save the loading
sink("loadings.txt")
loadings
sink()

# Add the column to the original data
data$Global = rowSums(sweep(scaled_data,2,loadings,"*"))
