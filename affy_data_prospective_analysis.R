# module load R
# R --vanilla
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.14")
BiocManager::install(c("affy","affyPLM","sva","AnnotationDbi","hgu133plus2.db"))

library(affy)
library(affyPLM)
library(sva)
library(AnnotationDbi)
library(hgu133plus2.db)
library(tidyverse)


# Read in CEL files
CEL_data <- ReadAffy(celfile.path='/projectnb/bf528/users/frizzled/project_1/data/samples/')

# RMA Normalized CEL files
rma_data <- rma(CEL_data)

# ------ Quality Assessment ------
# Probe Level linear Models fit to CEL files
CEL_data_PLM <- fitPLM(CEL_data, normalize = TRUE, background = TRUE)

# Relative Log Expression (RLE)
RLE_CEL_data_PLM <- RLE(CEL_data_PLM, type = "stats")

# Normalized Unscaled Standard Error (NUSE)
NUSE_CEL_data_PLM <- NUSE(CEL_data_PLM, type = "stats")

# Plot RLE & NUSE
hist(RLE_CEL_data_PLM[1, ], main='Median RLE', xlab='Score')

hist(NUSE_CEL_data_PLM[1, ], main='Median NUSE', xlab='Score')

# ------ Correct For Batch Effects ------
# Load metadata
proj_metadata <- read.csv('/project/bf528/project_1/doc/proj_metadata.csv')

batch <- proj_metadata$normalizationcombatbatch

mod <- model.matrix(~ proj_metadata$normalizationcombatmod, data = proj_metadata)

# Final normalized/batch corrected data
ComBat_data <- ComBat(exprs(rma_data), batch, mod = mod)

write.csv(ComBat_data, file = "normalized_data.csv")

# ------ Principal component analysis ------
#transpose for scaling
ComBat_data <- t(ComBat_data)

scaled_ComBat_data <- scale(ComBat_data)

#transpose back after scale for plot
scaled_ComBat_data <- t(scaled_ComBat_data)

pca <- prcomp(scaled_ComBat_data, scale = FALSE, center = FALSE)

pca_df <- as.data.frame(pca$rotation)

# summary(pca)  Use to view Proportion of Variance

ggplot(data = pca_df, mapping = aes(x = PC1, y = PC2)) +
  geom_point() + 
  xlab('PC1 - 11.47%') + ylab('PC2 - 8.41%')