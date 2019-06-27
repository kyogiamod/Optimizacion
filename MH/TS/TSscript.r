#options("width"=160) #Para que no se corte la consola

readQAP <- function(name) { 
    a <- read.delim(name,header=FALSE, sep ="")
    n<-as.integer(a[1,1])
    fl<-a[2:(n+1),1:n]
    dis<-a[(n+2):(n+n+1),1:n]
    d <- list(n=n, f= fl, d = dis)
    return(d)
}

evaluateQAP <- function(sol, f, d){
    acum <- 0
    for(i in 1:(length(sol))){
        for(j in 1:(length(sol))){
            acum = acum + f[i,j]*d[sol[i],sol[j]]   
        }
    }
    return(acum)
}

swap <- function(sol,i,j){
    piv<-sol[i]
    sol[i]<-sol[j]
    sol[j]<-piv
    return(sol)
}

getNeighbor <- function(sol, i, j){
    return (swap(sol,i,j))
}

getNeighbors <- function(sol, posi, posj){
    neighbors <- matrix(nrow=((length(sol)-3)*(length(sol)-2)/2), ncol=(length(sol)+2)) #el +2 es pra poner el switch que se hizo
    posNeighbors <- 1
    for(i in 1:length(sol)){
        if(i == posi || i==posj) { next }
        if((i+1) > length(sol)) { break }
        for(j in (i+1):length(sol))
        {
            if(j==posi || j == posj) { next }
            neig <- getNeighbor(sol,i,j)
            neig <- c(neig, i, j) #Para guardar las posiciones de switch
            neighbors[posNeighbors,] <- neig
            posNeighbors <- posNeighbors + 1
        }
    }
    return (neighbors)
}

evaluateNeighbors <- function(matrixNeighbors, QAP){
    evaluatedNeighbors <- c()
    for(i in 1:nrow(matrixNeighbors)) 
    {
        evaluatedNeighbors <- c(evaluatedNeighbors, evaluateQAP(matrixNeighbors[i,1:QAP$n],QAP$f, QAP$d))
    }
    return (evaluatedNeighbors)
}

acceptNeighborTabu <- function(neighbor, tabuStruct){
    i <- neighbor[27] #posicion i del switch
    j <- neighbor[28] #posicion j del switch
    if( tabuStruct[i,j] == 0 ){ #Si no esta el movimiento prohibido en tabu, retorna true
        return (TRUE)
    } 
    return (FALSE)
}

acceptNeighborAspiration <- function(neighbor, bestValue){
    if( neighbor[29] < bestValue ) { #Si esta en tabu pero cumple la condicion de aspiracion
        return (TRUE)
    }
    return (FALSE)
}

updateTabuStruct <- function(tabuStruct){
    for(i in 1:sqrt(length(tabuStruct))){
        if((i+1) > sqrt(length(tabuStruct))) { break }
        for(j in (i+1):sqrt(length(tabuStruct))){
            if(tabuStruct[i,j] > 0){
                tabuStruct[i,j] <- tabuStruct[i,j] - 1
            }
        }
    }
    return (tabuStruct)
}

################# MAIN ####################
set.seed(0)


QAP <- readQAP("bur26a.dat")
tabuStruct <- matrix( rep( 0, len=(QAP$n)^2), nrow = QAP$n)

#Solucion inicial
sol <- sample(1:QAP$n)
evaluatedSol <- evaluateQAP(sol, QAP$f, QAP$d)
values <- c(evaluatedSol)
bestConfigSol <- sol
bestEvaluatedSol <- evaluatedSol

for(it in 1:10){
    #get 2 random numbers to set them and swap the others
    setPosition <- sample(1:QAP$n, 2)
    #get neighbors
    neighbors <- getNeighbors(sol, setPosition[1], setPosition[2])
    #Evalua los neighbors
    evaluatedNeighbors <- evaluateNeighbors(neighbors, QAP)
    #Le agrega el valor de la fo
    neighbors <- cbind(neighbors, evaluatedNeighbors)
    #Los ordena de menor a mayor
    neighbors <- neighbors[order(neighbors[,29]),]

    #Recorrer de mejor valor a peor
    for(e in 1:nrow(neighbors)){
        neigh <- neighbors[e,]
        #print("neigh: ")
        #print(neigh[1:28])
        ant <- acceptNeighborTabu(neigh, tabuStruct) 
        #print(paste("ant: ", ant))
        ana <- acceptNeighborAspiration(neigh, bestEvaluatedSol)
        #print(paste("ana: ", ana))
        if ( ant || ana ){
            values <- c(values, neigh[29])
            #Si se acepta, entonces hay que actualizar tabuStruct y cambiar los valores
            sol <- neigh[1:26]
            #print(sol)
            i <- neigh[27]
            j <- neigh[28]
            tabuStruct[i,j] <- 3 #el tabu dura 3 iteraciones
            if( ana ){ #Si fue aceptado por aspiracion
                bestEvaluatedSol <- neigh[29]
                bestConfigSol <- neigh[1:26]
            }
            break
        }
    }
    tabuStruct <- updateTabuStruct(tabuStruct)
}

