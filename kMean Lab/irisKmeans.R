nameSpecies = levels(iris$Species)
# iris2 <- iris[sample(nrow(iris)),1:4] # Shuffle Row of Data
iris2 <- matrix(c(t(iris[sample(nrow(iris)),1:4])), nrow = 150, ncol = 4, byrow = TRUE)
omar <- par()$mar
par(mar=c(0,0,0,0))
Group <- rep(0,nrow(iris2))

# Get the k sample
k <- 3
fac <- c(1,2,3,4)
n <- rep(1,k)
sum <- matrix(nrow = k, ncol = 4)
mean <- matrix(nrow = k, ncol = 4)
colnames(sum) <- c("Sepal.length","Sepal.width","Petal.length","Petal.width")
colnames(mean) <- c("Sepal.length","Sepal.width","Petal.length","Petal.width")

j <- 1
for(i in sample(1:nrow(iris2), k)) {
  sum[j,] <- iris2[i,]
  mean[j,] <- iris2[i,]
  Group[i] <- j
  j <- j + 1
}

distance <- function(p,q) {
  return(sqrt(sum((p[1:min(length(p),length(q))]-q[1:min(length(p),length(q))])^2)))
}

count <- 0
check <- TRUE
while(check) {
  check <- FALSE
  for(i in sample(nrow(iris2))) {
    min <- 0
    dist <- Inf
    for(j in 1:k) {
      tmp <- distance(mean[j,fac],iris2[i,fac])
      if(tmp < dist) {
        min <- j
        dist <- tmp
      }
    }
    
    if(Group[i] == min)
      next
    
    if(Group[i] != 0) {
      n[Group[i]] <- n[Group[i]] - 1
      sum[Group[i],] <- sum[Group[i],] - iris2[i,]
      mean[Group[i],] <- sum[Group[i],] / n[Group[i]]
    }
      
    n[min] <- n[min] + 1
    sum[min,] <- sum[min,] + iris2[i,]
    mean[min,] <- sum[min,] / n[min]
    
    Group[i] <- min
      
    count <- count + 1
    check <- TRUE
    
  }
}

pairs(rbind(iris2,mean),col = append(Group,c(1:k)), pch = append(rep(1,150),rep(15,k)), cex = 1.5)
title(paste("Iris Flower Data"), outer = TRUE, line = -1.2)
par(omar)
