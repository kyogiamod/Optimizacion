probabilityFunction <- function(temp){
    return (  (exp( -(10000)/temp )) )
}

getTemp <- function(temp){
    return (0.95*temp)
}

values <- c()
initTemp <- 1000000
for(i in 1:100)
{
    initTemp <- getTemp(initTemp)
    values <- c(values, initTemp)
}

prob <- c()
for(i in 1:100)
{
    prob <- c(prob, probabilityFunction(values[i]))
}

#print(values)
#print(prob)
plot(prob)