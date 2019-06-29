library(xtable)
library(cluster)
library(factoextra)
library(microbenchmark)

colnames <- c("Area","Perimeter","Compatness","LoK","WoK","Asymmetry","LoKG","Class")
data <- read.table("seedB.txt", col.names = colnames)

data$Class <- as.factor(data$Class)

finalData <- data[,-c(2,4,5,8)]

#Se divide en 3 grupos y se hace el benchmark de lo que demora
times <- microbenchmark(clusters <- pam(finalData, diss = FALSE, k = 3), times=1)
write.table(t(c(summary(times$time)))[1,], "ExectTimeB.txt")

jpeg("img/big/kmeansB.jpeg")
plot(fviz_cluster(object = clusters, data = finalData, show.clust.cent = TRUE, ellipse.type = "t", geom="point") + 
        labs(title = "Resultados clustering K-means") + 
        theme_bw()
)
dev.off()
