library(xtable)
library(cluster)
library(factoextra)
library(tictoc)

euclideanDistance <- function(data, mean){
    return ( sqrt( 
        (data[1,1]-mean[1])^2 + 
        (data[1,2]-mean[2])^2 + 
        (data[1,3]-mean[3])^2 + 
        (data[1,4]-mean[4])^2 
                ) 
            )
}

evaluate <- function(Originaldata, clusters){
    #Se separan por clusters
	data <- cbind(Originaldata, clusters$cluster)

	dataCluster <- as.data.frame(data)
	colnames(dataCluster)[5] <- "cluster"

	#colnames(dataCluster) <- c("area","compactness","asymmetry","LoKG", "cluster")
    dataCluster <- split(dataCluster, dataCluster$cluster)

    centroid11 <- summary(dataCluster$"1"$area)[[4]]
    centroid12 <- summary(dataCluster$"1"$compactness)[[4]]
    centroid13 <- summary(dataCluster$"1"$asymmetry)[[4]]
    centroid14 <- summary(dataCluster$"1"$LoKG)[[4]]

    centroid21 <- summary(dataCluster$"2"$area)[[4]]
    centroid22 <- summary(dataCluster$"2"$compactness)[[4]]
    centroid23 <- summary(dataCluster$"2"$asymmetry)[[4]]
    centroid24 <- summary(dataCluster$"2"$LoKG)[[4]]

    centroid31 <- summary(dataCluster$"3"$area)[[4]]
    centroid32 <- summary(dataCluster$"3"$compactness)[[4]]
    centroid33 <- summary(dataCluster$"3"$asymmetry)[[4]]
    centroid34 <- summary(dataCluster$"3"$LoKG)[[4]]

    c1 <- c(centroid11, centroid12, centroid13, centroid14)
    c2 <- c(centroid21, centroid22, centroid23, centroid24)
    c3 <- c(centroid31, centroid32, centroid33, centroid34)

    #Ahora calcular las distancias con sus centroides
    sumC1 <- 0
    sumC2 <- 0
    sumC3 <- 0

    for(pos in 1:nrow(dataCluster$"1")){
        sumC1 <- sumC1 + euclideanDistance(dataCluster$"1"[pos,], c1)
    }
    for(pos in 1:nrow(dataCluster$"2")){
        sumC2 <- sumC2 + euclideanDistance(dataCluster$"2"[pos,], c2)
    }
    for(pos in 1:nrow(dataCluster$"3")){
        sumC3 <- sumC3 + euclideanDistance(dataCluster$"3"[pos,], c3)
    }

    return(sumC1+sumC2+sumC3)
}


tamano <- "small"

colnames <- c("area","perimeter","compactness","LoK","WoK","asymmetry","LoKG","class")
data <- read.table("seedS.txt", col.names = colnames)

#data$Class <- as.factor(data$Class)

finalData <- data[,-c(2,4,5,8)]

#Se divide en 3 grupos y se hace el benchmark de lo que demora
tic()
clusters <- kmeans(finalData, 3)
extime <- toc()



jpeg(paste("img/",tamano,"kmeans.jpeg", sep=""))
plot(fviz_cluster(object = clusters, data = finalData, show.clust.cent = TRUE, ellipse.type = "t", geom="point") + 
        labs(title = "Resultados clustering K-means") + 
        theme_bw()
)
dev.off()


#exect time
extime <- as.data.frame(extime$toc - extime$tic)
names(extime) <- "Execution time: "
write.table(extime, paste("data/", tamano, ".txt", sep=""), append = FALSE, row.names=F, col.names=F)
# value sol
valueSol <- as.data.frame(evaluate(finalData, clusters))
names(valueSol) <- "Valor final de la solucion"
write.table(c(valueSol), paste("data/", tamano, ".txt", sep=""), append = TRUE, row.names=F, col.names=F)
#vector sol
vectorSol <- as.data.frame(clusters$cluster)
names(vectorSol) <- "Vector solucion"
write.table(vectorSol[,1], paste("data/", tamano, ".txt", sep=""), append = TRUE, row.names=F, col.names=F)

