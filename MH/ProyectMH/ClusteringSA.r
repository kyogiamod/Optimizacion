library(xtable)
library(cluster)
library(factoextra)
library(tictoc)

readSeeds <- function(name){
    a <- read.delim(name, header=FALSE, sep="")
    colnames(a) <- c("area","perimeter","compactness","LoK","WoK","asymmetry","LoKG","class")
    return (a)
}

# Funcion : swap
swap <- function(vectorSol,i,j){
    piv <- vectorSol[i]
    vectorSol[i] <- vectorSol[j]
    vectorSol[j] <- piv
    return(vectorSol)
}


#### Funciones ####

euclideanDistance <- function(data, mean){
    return ( sqrt( 
        (data[1,1]-mean[1])^2 + 
        (data[1,2]-mean[2])^2 + 
        (data[1,3]-mean[3])^2 + 
        (data[1,4]-mean[4])^2 
                ) 
            )
}

evaluate <- function(data){
    #Se separan por clusters
    dataCluster <- split(data, data$cluster)

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

getNeighbor <- function(neighborFunc, vectorSol, i, j){
    return (neighborFunc(vectorSol,i,j))
}

probabilityFunction <- function(fs1, fs2, temp){
    return ( exp( (fs1-fs2)/temp ) )
}

acceptSolution <- function(originalSolEvaluated, newSolEvaluated, temp){
    fs1 <- originalSolEvaluated
    fs2 <- newSolEvaluated
    if( fs2 < fs1 ){ 
        return (TRUE) 
    } else {
        pf <- probabilityFunction(fs1, fs2, temp)
        random <- runif(1,0,1)

        if ( random < pf ){
            return (TRUE)
        }
    }
    return (FALSE)
}



################# MAIN ####################
set.seed(0)

dataSeeds <- readSeeds("seedsS.txt")
#Por análisis estadistico previo, se eliminan algunas columnas
data <- dataSeeds[,-c(2,4,5,8)]

## Constantes
Tmax <- 10000
ItMax <- 500
alpha <- 0.5
evaluatedSoles <- c()
bestEvaluatedSol <- c()
dataQuantity <- nrow(data)

# Generate initial solution
vectorSol <- c()
for(i in 1:dataQuantity){
    vectorSol <- c(vectorSol, sample(c(1,2,3), 1))
}
#Se agrega la columna de que cluster pertenece
data <- cbind(data, vectorSol)
names(data)[5] <- "cluster"

evaluatedSol <- evaluate(data)

T = Tmax
It <- 1
ItLocal <- 0
stopCondition <- TRUE
evaluatedSoles <- c(evaluatedSoles, evaluatedSol)
bestEvaluatedSol <- c(bestEvaluatedSol, evaluatedSol)

tic()
while(stopCondition)
{
    #Iteraciones interiores, son 10 antes de modificar la temperatura
    while(ItLocal < 10)
    {
        ItLocal = ItLocal + 1
        #Take 2 samples
        sample1 <- sample(1:dataQuantity, 2)
            #hago swap de un numero empezando desde el principio
            #for(i in 1:length(sample1)){
        #Get neighbor swapping the two values of sample1
        neighbor <- getNeighbor(swap, vectorSol, sample1[1], sample1[2])
        #Evaluate new solution
        data[,5] <- neighbor
        evaluatedNeighbor <- evaluate(data)
        #Si lo acepta, entonces cambia de solución
        if( acceptSolution(evaluatedSol, evaluatedNeighbor, T) ) { 
            vectorSol <- neighbor 
            evaluatedSol <- evaluatedNeighbor
            evaluatedSoles <- c(evaluatedSoles, evaluatedNeighbor)
            if( tail(bestEvaluatedSol, 1) < evaluatedNeighbor) {
                bestEvaluatedSol <- c(bestEvaluatedSol, tail(bestEvaluatedSol, 1))
            } else {
                bestEvaluatedSol <- c(bestEvaluatedSol, evaluatedNeighbor)
            }
            break
        }
            #}
    }
    #Cuando ya completa las iteraciones interiores, se modifican las temperaturas y las iteraciones actuales
    T <- alpha*T
    It <- It + 1
    ItLocal <- 0
    print(It)
    if(It == ItMax) {  stopCondition <- FALSE }
}
exectTime <- toc()
exectTime <- 
#Para este momento ya deberia tener una solución aceptable en la variable sol y todos los obtenidos en soles
jpeg(paste("img/small/valoresFuncionObjetivo-T", Tmax, "-", "alpha", alpha, "0-200", ".jpeg", sep=""))
plot(evaluatedSoles[1:50], main=paste("Funcion objetivo clustering, T",Tmax, "alpha", alpha), xlab="Iteraciones", ylab="Valores")
lines(bestEvaluatedSol[1:50])
dev.off()
#Para este momento ya deberia tener una solución aceptable en la variable sol y todos los obtenidos en soles
jpeg(paste("img/small/valoresFuncionObjetivo-T", Tmax, "-", "alpha", 0.75, "200-end", ".jpeg", sep=""))
plot(evaluatedSoles, main=paste("Funcion objetivo clustering, T=",Tmax, "alpha", alpha), xlab="Iteraciones", ylab="Valores")
lines(bestEvaluatedSol)
dev.off()

# Now there is an optimal saved in vectorSol. We need to separate the data
cluster <- c()
for(i in 1:dataQuantity){
    if(data[i,5] == 1) { cluster <- c(cluster, 3) }
    else if(data[i,5] == 3) { cluster <- c(cluster, 1) }
    else { cluster <- c(cluster, 2)}
}

#data$clustering <- cluster
sa_clusters <- list(data=data[,1:4], cluster=cluster)
jpeg(paste("img/small/clustersMH-S, T=",Tmax, "alpha", alpha, ".jpeg", sep=""))
plot(fviz_cluster(object = sa_clusters, data = data, show.clust.cent = TRUE, ellipse.type = "t", geom="point") + 
        labs(title = paste("Clustering con SA, T", Tmax, "alpha", alpha)) + 
        theme_bw()
)
dev.off()

write.table()