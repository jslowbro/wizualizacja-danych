getwd()
setwd('/home/ligerd/Wizualizacja danych/PCA_LDA')
df <- read.csv("eeg-eye-state.csv")
#usuwanie cechy class
df <- df[1:ncol(df)-1]

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
biplot(pca,scale=0, cex=.7)


pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow=c(2,2))
  plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="l")
  par(mfrow=c(1,1))
}

pcaCharts(pca)
#Dwa wykresy pokazują to samo ggbiplot jest lepszy według mnie
ggbiplot(pca, scale = 0)
biplot(pca,scale=0, cex=.7)




#ucinanie danych
n<-dim(df)[1]#  dla danych len()
df3<-df[1:(n-14880),]#ucinanie takie głupie ale żeby było widać wykres dalej poprawie
pcatest <- prcomp(df3, center = TRUE, scale = TRUE)
biplot(pcatest,scale=0, cex=.7)