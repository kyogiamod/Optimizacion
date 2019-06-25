library(xtable)
library(cluster)
library(factoextra)

readSeeds <- function(name){
    a <- read.delim(name, header=FALSE, sep="")
    colnames(a) <- c("Area","Perimeter","Compatness","LoK","WoK","Asymmetry","LoKG","Class")
    return (a)
}

# Funcion : swap
swap <- function(matrixSol,i,j){
    if( i > j ) {
        p <- i
        i <- j
        j <- p
    } 
    piv <- matrixSol[i,]
    matrixSol[i,] <- matrixSol[j,]
    matrixSol[j,] <- piv
    return(matrixSol)
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

#matrixSol: matrix of #(cluster) columns, in this case is 3
evaluate <- function(matrixSol, data){
    #Se generan los cluster
    c1IDs <- c()
    c2IDs <- c()
    c3IDs <- c()
    for(i in 1:(length(matrixSol)/3)){
        aux <- matrixSol[i,]
        if(aux[1] == 1) { c1IDs <- c(c1IDs, i) }
        else if(aux[2] == 1) { c2IDs <- c(c2IDs, i) }
        else { c3IDs <- c(c3IDs, i) } 
    }
    centroid11 <- 0
    centroid12 <- 0
    centroid13 <- 0
    centroid14 <- 0

    centroid21 <- 0
    centroid22 <- 0
    centroid23 <- 0
    centroid24 <- 0

    centroid31 <- 0
    centroid32 <- 0
    centroid33 <- 0
    centroid34 <- 0

    for(e in c1IDs) {
        centroid11 <- centroid11 + data[e, 1]
        centroid12 <- centroid12 + data[e, 2]
        centroid13 <- centroid13 + data[e, 3]
        centroid14 <- centroid14 + data[e, 4]
    }

    for(e in c2IDs) {
        centroid21 <- centroid21 + data[e, 1]
        centroid22 <- centroid22 + data[e, 2]
        centroid23 <- centroid23 + data[e, 3]
        centroid24 <- centroid24 + data[e, 4]
    }

    for(e in c3IDs) {
        centroid31 <- centroid31 + data[e, 1]
        centroid32 <- centroid32 + data[e, 2]
        centroid33 <- centroid33 + data[e, 3]
        centroid34 <- centroid34 + data[e, 4]
    }

    centroid11 <- centroid11/length(c1IDs)
    centroid12 <- centroid12/length(c1IDs)
    centroid13 <- centroid13/length(c1IDs)
    centroid14 <- centroid14/length(c1IDs)

    centroid21 <- centroid21/length(c2IDs)
    centroid22 <- centroid22/length(c2IDs)
    centroid23 <- centroid23/length(c2IDs)
    centroid24 <- centroid24/length(c2IDs)

    centroid31 <- centroid31/length(c3IDs)
    centroid32 <- centroid32/length(c3IDs)
    centroid33 <- centroid33/length(c3IDs)
    centroid34 <- centroid34/length(c3IDs)

    c1 <- c(centroid11, centroid12, centroid13, centroid14)
    c2 <- c(centroid21, centroid22, centroid23, centroid24)
    c3 <- c(centroid31, centroid32, centroid33, centroid34)

    #Ahora calcular las distancias con sus centroides
    sumC1 <- 0
    sumC2 <- 0
    sumC3 <- 0

    for(e in c1IDs){
        sumC1 <- sumC1 + euclideanDistance(data[e,], c1)
    }
    for(e in c2IDs){
        sumC2 <- sumC2 + euclideanDistance(data[e,], c2)
    }
    for(e in c3IDs){
        sumC3 <- sumC3 + euclideanDistance(data[e,], c3)
    }

    return(sumC1+sumC2+sumC3)
}

getNeighbor <- function(neighborFunc, matrixSol, i, j){
    return (neighborFunc(matrixSol,i,j))
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

dataSeeds <- readSeeds("seeds210.txt")
#Por análisis estadistico previo, se eliminan algunas columnas
dataSeeds <- dataSeeds[,-c(2,4,5)]

#Se pasa a variable categorica la clase
dataSeeds$Class <- as.factor(dataSeeds$Class)
data <- dataSeeds[,-c(5)]


## Constantes
Tmax <- 10000
ItMax <- 500
evaluatedSoles <- c()
bestEvaluatedSol <- c()
dataQuantity <- 210


# Generate initial solution
vectorSol <- c()
for(i in 1:dataQuantity){
    vectorSol <- c(vectorSol, sample(c(0,0,1)))
}
matrixSol <- matrix(vectorSol, nrow=dataQuantity, ncol=3, byrow=TRUE)
evaluatedSol <- evaluate(matrixSol, data)
T = Tmax
It <- 1
ItLocal <- 1
stopCondition <- TRUE
evaluatedSoles <- c(evaluatedSoles, evaluatedSol)
bestEvaluatedSol <- c(bestEvaluatedSol, evaluatedSol)

while(stopCondition)
{
    #Iteraciones interiores, son 10 antes de modificar la temperatura
    while(ItLocal < 10)
    {
        #Take 2 samples
        sample1 <- sample(1:dataQuantity, 2)
            #hago swap de un numero empezando desde el principio
            #for(i in 1:length(sample1)){
        #Get neighbor swapping the two values of sample1
        neighbor <- getNeighbor(swap, matrixSol, sample1[1], sample1[2])
        #Evaluate new solution
        evaluatedNeighbor <- evaluate(neighbor, data)
        #Si lo acepta, entonces cambia de solución
        if( acceptSolution(evaluatedSol, evaluatedNeighbor, T) ) { 
            matrixSol <- neighbor 
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
        ItLocal = ItLocal + 1
    }
    #Cuando ya completa las iteraciones interiores, se modifican las temperaturas y las iteraciones actuales
    T <- 0.5*T
    It <- It + 1
    ItLocal <- 1
    if(T == 0 || It == ItMax) {  stopCondition <- FALSE }
}
#Para este momento ya deberia tener una solución aceptable en la variable sol y todos los obtenidos en soles
jpeg("img/graph.jpeg")
plot(evaluatedSoles, main="Funcion objetivo clustering", xlab="Iteraciones", ylab="Valores")
lines(bestEvaluatedSol)
dev.off()

# Now there is an optimal saved in matrixSol. We need to separate the data
cluster <- c()
for(i in 1:dataQuantity){
    vector <- matrixSol[i,]
    if(vector[1] == 1) { cluster <- c(cluster, 3) }
    else if(vector[2] == 1) { cluster <- c(cluster, 1) }
    else { cluster <- c(cluster, 2)}
}

#data$clustering <- cluster
sa_clusters <- list(data=data, cluster=cluster)
jpeg("img/clustersMH.jpeg")
plot(fviz_cluster(object = sa_clusters, data = data, show.clust.cent = TRUE, ellipse.type = "t", geom="point") + 
        labs(title = "Clustering con SA") + 
        theme_bw()
)
dev.off()
