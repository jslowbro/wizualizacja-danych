getwd()
setwd('/home/ligerd/Wizualizacja danych/PCA_LDA')
df <- read.csv("eeg-eye-state.csv")

#df <- df[1:ncol(df)-1]

#install.packages("GGally")
library(GGally)
ggpairs(df,upper = list(continuous = wrap("cor", size = 3)),
        lower = list(continuous = wrap("points", size = 0.3)))
#Macierz  koralacji 
cor(df, method = "spearman")

pca <- prcomp(df, center = TRUE, scale = TRUE)

#Importance of components
summary(pca)

plot(pca, type = "l", main = "Scree plot")
#instalacja pakitów do grafika 
install.packages('devtools', repos='http://cran.rstudio.com/')

library(devtools)

library(ggbiplot)

ggbiplot(pca, scale = 1)




pcaCharts(pca)
install.packages("pcaExplorer")

biplot(pca,scale=0, cex=.7)


pca.out <-pca
pca.out$rotation <- -pca.out$rotation
pca.out$x <- -pca.out$x
biplot(pca.out,scale=0, cex=.7)
