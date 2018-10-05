.libPaths()
.libPaths("X:/R/library")
library(neuralnet)

#############################
XOR <- c(0,1,1,0)
xor.data <- data.frame(expand.grid(c(0,1), c(0,1)), XOR)
print(net.xor1 <- neuralnet( XOR~Var1+Var2, xor.data, hidden=0, rep=50))
plot(net.xor1, rep="best")
#############################

#############################
XOR <- c(0,1,1,0)
xor.data <- data.frame(expand.grid(c(0,1), c(0,1)), XOR)
print(net.xor2 <- neuralnet( XOR~Var1+Var2, xor.data, hidden=1, rep=50))
plot(net.xor2, rep="best")
#############################

#############################
XOR <- c(0,1,1,0)
xor.data <- data.frame(expand.grid(c(0,1), c(0,1)), XOR)
print(net.xor2 <- neuralnet( XOR~Var1+Var2, xor.data, hidden=1, rep=50, err.fct="sse",
                             linear.output=FALSE, likelihood=TRUE))
plot(net.xor2, rep="best")
#############################

data(infert, package="datasets")
print(net.infert <- neuralnet(case~parity+induced+spontaneous, infert, err.fct="ce", linear.output=FALSE, likelihood=TRUE))
gwplot(net.infert, selected.covariate="parity")
gwplot(net.infert, selected.covariate="induced")
gwplot(net.infert, selected.covariate="spontaneous")
