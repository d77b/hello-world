.libPaths("X:/R/library")
library(neuralnet)

dados <- c(2.9,2.9,1.8)
y <- 0.82
dados.data <- data.frame(y, t(dados))
p1<-c(0.5, -0.5)
p2<-c(0.9, -0.4)
p3<-c(0.7, 0.4)

startweights <-as.matrix(cbind(p1,p2,p3))

print(net0 <- neuralnet(y~X1+X2+X3,  dados.data, hidden=2, rep=1, startweights = startweights, err.fct="ce", linear.output=FALSE))
plot(net0)

