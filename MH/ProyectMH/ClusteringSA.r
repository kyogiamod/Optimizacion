library(xtable)

readQAP <- function(name) { 
a <- read.delim(name,header=FALSE, sep ="")
n<-as.integer(a[1,1])
fl<-a[2:(n+1),1:n]
dis<-a[(n+2):(n+n+1),1:n]
d <- list(n=n, f= fl, d = dis)
return(d)
}

readSeeds <- function(name){
    a <- read.delim(name, header=FALSE, sep="")
    colnames(a) <- c("Area","Perimeter","Compatness","LoK","WoK","Asymmetry","LoKG","Class")
    return (a)
}


#sol: vector of #(cluster) columns, this case is 3
evaluate <- function(sol, posCluster){
  return(acum)
}

########## FUNCIONES DE VENCIDAD ###########
# Funcion : swap
swap <- function(sol,i,j)
{
  piv<-sol[i]
  sol[i]<-sol[j]
  sol[j]<-piv
  return(sol)
}

# Function 2: insertion
# s = c(1,2,3,4,5)
# inse <- insertion(s,2,4)
# print(inse)
insertion <- function(sol, i, j){
    if(j < i) 
    {
        a <- i
        i <- j
        j <- a 
    }
    if (j == i) { return (sol) }
    aux1 <- sol[i]
    sol[i] <- sol[j]
    for(pos in i:(j-1)){
        aux2 <- sol[pos+1]
        sol[pos+1] <- aux1
        aux1 <- aux2
    }
    return (sol)
}


# Function 3: Reverse
# s = c(1,2,3,4,5,6)
# rev <- reverse(s, 2, 5)
# print(rev)
reverse <- function(sol, i, j){
    for(pos in 1:(((j-i)/2)+1)){
        aux <- sol[i]
        sol[i] <- sol[(j-pos+1)]
        sol[j-pos+1] <- aux
    }
  return (sol)
}

#### Funciones ####
getNeighbor <- function(neighborFunc, sol, i, j){
    return (neighborFunc(sol,i,j))
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

data <- readSeeds("seeds210.txt")
#Por análisis estadistico previo, se eliminan algunas columnas
data <- data[,-c(2,4,5)]

#Se pasa a variable categorica la clase
data$Class <- as.factor(data$Class)

data <- data[,-c(5)]

#Se hace un aplanamiento de los datos



## Constantes
Tmax <- 1000000
ItMax <- 200
evaluatedSoles <- c()
bestEvaluatedSol <- c()
dataQuantity <- 210


# Generate initial solution
vectorSol <- c()
for(i in 1:dataQuantity){
    vectorSol <- c(vectorSol, sample(c(0,0,1)))
}

matrixSol <- matrix(vectorSol, nrow=dataQuantity, ncol=3, byrow=TRUE)

