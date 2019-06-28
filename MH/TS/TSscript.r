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
    #neighbors <- matrix(nrow=((length(sol)-3)*(length(sol)-2)/2), ncol=(length(sol)+2)) #el +2 es pra poner el switch que se hizo
    neighbors <- matrix(nrow=0, ncol=(length(sol)+2)) #el +2 es pra poner el switch que se hizo
    len <- length(sol)
    for(i in 1:length(sol)){
        if (runif(1,0,1) > 0.33) { next }
        if(i == posi || i==posj) { next }
        if((i+1) > length(sol)) { break }
        for(j in (i+1):length(sol))
        {
            if( runif(1,0,1) > 0.33) { next }
            if(j==posi || j == posj) { next }
            neig <- getNeighbor(sol,i,j)
            neig <- c(neig, i, j) #Para guardar las posiciones de switch
            neighbors <- rbind(neighbors, neig)
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
bestEvaluatedSoles <- c(evaluatedSol)

for(it in 1:100){
    print(it)
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
        ant <- acceptNeighborTabu(neigh, tabuStruct) 
        ana <- acceptNeighborAspiration(neigh, bestEvaluatedSol)
        if ( ant || ana ){
            values <- c(values, neigh[29])
            #Si se acepta, entonces hay que actualizar tabuStruct y cambiar los valores
            sol <- neigh[1:26]
            #print(sol)
            i <- neigh[27]
            j <- neigh[28]
            tabuStruct[i,j] <- 20 #el tabu dura 20 iteraciones
            if ( neigh[29] < bestEvaluatedSol ) { #Si es que hay un nuevo mejor:
                bestConfigSol <- neigh[1:26] #Se guarda su configuracion
                bestEvaluatedSol <- neigh[29] #Se guarda el valor de la config
                bestEvaluatedSoles <- c(bestEvaluatedSoles, bestEvaluatedSol) #Se agrega como nuevo valor
            } else { #Si el vecino no es mejor, se sigue con la anterior
                bestEvaluatedSoles <- c(bestEvaluatedSoles, tail(bestEvaluatedSoles,1))
            }
            #Como ya esta fue aceptado, no hay que seguir con otros vecinos 
            break 
        }
    }
    tabuStruct <- updateTabuStruct(tabuStruct)
}

jpeg("img/valuesTS.jpeg")
plot(values, main="Valores obtenidos con TS", xlab="Iteraciones", ylab="Valores")
lines(bestEvaluatedSoles)
dev.off()