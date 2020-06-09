rm(list=ls())
library(FactoMineR)
library(factoextra)
library(PCAmixdata)

#Read the data
WineData <- read.table("AllWineDataPreProcessed.csv", header=TRUE, sep=";")
indnames <- rownames(WineData)
varnames <- colnames(WineData)

#PCA
pca <-PCA(WineData[,-13], quali.sup = 12, graph = F, scale = T)

# Extract eigenvalues/variances
get_eig(pca)

# Visualize eigenvalues
pca.eigenvalues <- pca$eig[,c("eigenvalue")]
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

ind.p <- fviz_pca_ind(pca, geom = "point", col.ind = WineData$wine_type)
ggpubr::ggpar(ind.p,
              title = "Individuals - PCA",
              xlab = "PC1", ylab = "PC2",
              legend.title = "Wine type", legend.position = "top",
              ggtheme = theme_gray(), palette = c("#FA2A09", "#EFC000FF")
)

fviz_pca_biplot(pca, 
                col.ind = WineData$wine_type, palette = c("#FA2A09", "#EFC000FF"), 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Wine type")

fviz_pca_biplot(pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = WineData$wine_type, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = c("#F1C40F", "#2ECC71"),
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "Species", color = "Contrib",
                                    alpha = "Contrib")
)

ForBiplotQuality <- WineData
ForBiplotQuality$quality <- as.factor(ForBiplotQuality$quality)

fviz_pca_biplot(pca, 
                col.ind = ForBiplotQuality$quality, palette = c("#2C3E50", "#D35400","#F1C40F", "#1E8449", 
                                                                "#2471A3", "#9B59B6", "#C0392B" ), 
                addEllipses = FALSE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Wine type")

fviz_pca_biplot(pca, 
                 # Individuals
                 geom.ind = "point",
                 fill.ind = ForBiplotQuality$quality, col.ind = "black",
                 pointshape = 21, pointsize = 2,
                 palette = c("#F1C40F", "#2ECC71","#27AE60", "#16A085", "#1ABC9C", "#3498DB", "#2980B9" ),
                 addEllipses = TRUE,
                 # Variables
                 alpha.var ="contrib", col.var = "contrib",
                 gradient.cols = "RdYlBu",
                 
                 legend.title = list(fill = "Species", color = "Contrib",
                                     alpha = "Contrib")
)

#Latent concepts
X.quanti <- splitmix(WineData)$X.quanti[,1:11]
X.quanti.sup <-splitmix(WineData)$X.quanti[,12]
res.pcamix <- PCAmix(X.quanti, NULL, rename.level=TRUE, graph=FALSE, ndim=25)
res.sup <- supvar(res.pcamix, X.quanti.sup = X.quanti.sup, X.quali.sup = WineData[13], rename.level=TRUE)
res.pcarot <- PCArot(res.sup, dim=2, graph=FALSE)
plot(res.sup, choice="cor", coloring.var=TRUE, axes=c(1, 2), leg=TRUE, posleg="topleft", 
     main="Variables before rotation")
plot(res.pcarot, choice="cor", coloring.var=TRUE, axes=c(1, 2), leg=TRUE, posleg="topright", 
     main="Variables - PCA after rotation")
