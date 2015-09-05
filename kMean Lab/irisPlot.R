omar <- par()$mar
par(mar=c(0,0,0,0))
pairs(iris[1:4],col = as.numeric(iris$Species), pch = as.numeric(iris$Species), cex = 1.5)
title("Iris Flower Data", outer = TRUE, line = -1.2)
legend("bottomright", levels(iris$Species), pch = (1:3), col = (1:3))
par(omar)