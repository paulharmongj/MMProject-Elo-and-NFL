#take a look at predicted scores from guys
setwd("C:/Users/r74t532/Documents")
s <- read.csv("stats.csv", header = TRUE)
head(s)

meanfunk <- function(a){mean(a,na.rm = TRUE)}
sdfunk <- function(b){sd(b, na.rm = TRUE)}

meanscores <- apply(s,2,meanfunk)
sds <- apply(s,2,sdfunk)


#simulations
nsims <- 1000
sim.mat <- matrix(0,nrow = nsims, ncol = 5)

for (j in 1:5){
  sim.mat[,j] <- rnorm(1000, meanscores[j],sds[j])
}

newplayers <- sim.mat[,1] + sim.mat[,2]
oldplayers <- sim.mat[,4] + sim.mat[,5]

library(grDevices)
paul_pink <- rgb(18,2,0,alpha = 10, maxColorValue = 20)
ggplot() + geom_density(aes(x = newplayers), color = "blue", size = 2, fill = "lightblue") + geom_density(aes(x = oldplayers), color = "red", fill = paul_pink, size = 2) + ggtitle("Predicted Scores from Players")


