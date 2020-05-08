rm(list=ls())
library(FactoMineR)
library(factoextra)

#Read the data
AllWineData <- read.table("AllWineDataPreProcessed.csv", header=TRUE, sep=";")
indnames <- rownames(AllWineData)
varnames <- colnames(AllWineData)

#PCA
pca <-PCA(AllWineData, quali.sup = 13, graph = F, scale = T)

# Extract eigenvalues/variances
get_eig(pca)

# Visualize eigenvalues
pca.eigenvalues <- pca$eig[,c("eigenvalue")]
col.line = "magenta"
plot(pca.eigenvalues,  type = "o", xlab = "Dimensions", ylab = "Eigenvalue", main = "Screeplot"
     , col = "blue")

# Visualize the variance %
fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 30))

# Extract the results for variables
var <- get_pca_var(pca)

# Contribution of variables
head(var$contrib)

# Coordinates of variables
head(var$coord)

# Control variable colors using their contributions
fviz_pca_var(pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T)

# Contributions of variables to PC1
fviz_contrib(pca, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 10)

# Extract the results for individuals
ind <- get_pca_ind(pca)
head((ind$coord))
# Graph of individuals
# 1. Use repel = TRUE to avoid overplotting
# 2. Control automatically the color of individuals using the cos2
# cos2 = the quality of the individuals on the factor map
# Use points only
# I see nothing
fviz_pca_ind(pca, col.ind = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = F)

# Biplot of individuals and variables
fviz_pca_biplot(pca, repel = F)

# individuals // PC killed // Interesting one // Using repel = TRUE no overlapping among name points
fviz_pca_ind(pca,
             label = "none", # hide individual labels
             habillage = AllWineData$quality, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = T, repel = F)
