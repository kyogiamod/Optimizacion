library(xtable)
library(cluster)
library(factoextra)

colnames <- c("Area","Perimeter","Compatness","LoK","WoK","Asymmetry","LoKG","Class")
data <- read.table("seeds_dataset.txt", col.names = colnames)

data$Class <- as.factor(data$Class)

print(xtable(cor(data[,1:7]), type = "latex"), file = "correlation")

finalData <- data[,-c(2,4,5)]

siluetas <- c()
for(i in 1:10)
{
    fit <- pam(finalData, diss=FALSE, k=i)
    siluetas[i] <- fit$silinfo$avg.width
}

#Se guardan los puntos que da el metodo
jpeg("img/Siluetas.jpeg")
plot(siluetas, type="b", xlab="Cantidad de clusters", ylab="Valor de la silueta")
dev.off()

#Entonces el mejor cluster es k=3

#Se divide en 3 grupos
clusters <- pam(data, diss = FALSE, k = 3)


jpeg("img/kmeans.jpeg")
plot(fviz_cluster(object = clusters, data = data, show.clust.cent = TRUE, ellipse.type = "t", geom="point") + 
        labs(title = "Resultados clustering K-means") + 
        theme_bw()
)
dev.off()

ggmap(NYCMap) + geom_point(aes(x = Lon[], y = Lat[], colour = as.factor(Borough)),data = data14) +
  ggtitle("NYC Boroughs using KMean")