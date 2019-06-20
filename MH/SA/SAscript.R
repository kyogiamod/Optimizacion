library(xtable)

readQAP <- function(name) { 
a <- read.delim(name,header=FALSE, sep ="")
n<-as.integer(a[1,1])
fl<-a[2:(n+1),1:n]
dis<-a[(n+2):(n+n+1),1:n]
d <- list(n=n, f= fl, d = dis)
return(d)
}

evaluarQAP <- function(sol, f, d){
  acum <- 0
  for(i in 1:(length(sol))){
    for(j in 1:(length(sol))){
      acum = acum + f[i,j]*d[sol[i],sol[j]]   
    }
  }
  #print(paste(sol))
  return(acum)
}

########## FUNCIONES DE VENCIDAD ###########

# Funcion 1: swap
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

QAP <- readQAP("bur26a.dat")
## Constantes
Tmax <- 1000000
ItMax <- 200
evaluatedSoles <- c()
bestEvaluatedSol <- c()
summaries <- c()
AllTogetherGraph <- c()

# Generate initial solution
for(instancia in 1:11)
{
    sol <- sample(1:QAP$n)
    evaluatedSol <- evaluarQAP(sol, QAP$f, QAP$d)
    T = Tmax
    It <- 1
    ItLocal <- 1
    stopCondition <- TRUE
    evaluatedSoles <- c(evaluatedSoles, evaluatedSol)
    bestEvaluatedSol <- c(bestEvaluatedSol, evaluatedSol)
    while(stopCondition){
        #Iteraciones interiores, son 10 antes de modificar la temperatura
        while(ItLocal < 10){
            #Generate random neighbor de las n posiciones
            sample1 <- sample(1:QAP$n)
            #hago swap de un numero empezando desde el principio
            for(i in 1:length(sample1)){
                #Crea un nuevo vecino que es el swap de la posición entre 1 a 10 y un numero aleatorio de todos los posibles
                neighbor <- getNeighbor(insertion, sol, i, sample1[i])
                #Evalua la nueva funcion
                evaluatedNeighbor <- evaluarQAP(neighbor, QAP$f, QAP$d)
                #Si lo acepta, entonces cambia de solución
                if( acceptSolution(evaluatedSol, evaluatedNeighbor, T) ) { 
                    sol <- neighbor 
                    evaluatedSol <- evaluatedNeighbor
                    evaluatedSoles <- c(evaluatedSoles, evaluatedNeighbor)
                    if( tail(bestEvaluatedSol, 1) < evaluatedNeighbor) {
                        bestEvaluatedSol <- c(bestEvaluatedSol, tail(bestEvaluatedSol, 1))
                    } else {
                        bestEvaluatedSol <- c(bestEvaluatedSol, evaluatedNeighbor)
                    }
                    break
                }
            }
            ItLocal = ItLocal + 1
        }
        #Cuando ya completa las iteraciones interiores, se modifican las temperaturas y las iteraciones actuales
        T <- 0.75*T
        It <- It + 1
        ItLocal <- 1
        if(T < 1 || It == ItMax) {  stopCondition <- FALSE }
        print(paste(instancia, " ", T))
    }
    #Para este momento ya deberia tener una solución aceptable en la variable sol y todos los obtenidos en soles
    jpeg(paste("img/insertion/", "instance", toString(instancia), ".jpeg", sep=""))
    plot(evaluatedSoles, main=paste("Instancia", toString(instancia)), xlab="Iteraciones", ylab="Valores")
    lines(bestEvaluatedSol)
    dev.off()
    
    #Se guarda el summary en la lista de summaries
    summaries <- c(summaries, summary(evaluatedSoles))

    AllTogetherGraph <- c(AllTogetherGraph, evaluatedSoles)
    #Resetear las variables para que sea una nueva instancia
    evaluatedSoles <- c()
    bestEvaluatedSol <- c()
}
jpeg("img/insertion/AllTogetherGraph.jpeg")
plot(AllTogetherGraph, main=paste("Instancia", toString(instancia)), xlab="Iteraciones", ylab="Valores")
dev.off()



#Una vez finalizado, summaries contiene toda la info de las 11 iteraciones. Se pasa a matrix:
summaryMatrix <- matrix(summaries, nrow=11, ncol=6)

dimnames(summaryMatrix) = list(
    c("IT 1","IT 2","IT 3","IT 4","IT 5","IT 6","IT 7","IT 8","IT 9","IT 10","IT 11"), 
    c("Min","1Q","Median","Mean","3Q","Max")
    )

# Se guarda el archivo en formato latex, LOS RESULTADOS ESTAN DIVIDIDOS EN 10K PARA MEJOR VISUALIZACION 
print(xtable(floor(summaryMatrix/10000), type = "latex"), file = "tablaVecindad/Insertion.tex")

write.table(toString(sol), "tablaVecindad/BestSolutionInsertion.txt", sep="\t")