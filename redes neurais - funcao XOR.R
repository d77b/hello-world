.libPaths()
.libPaths("X:/R/library")
library(neuralnet)

#Redes Neurais função "XOR"#
##########################
XOR <- c(0,1,1,0)
xor.data <- data.frame(expand.grid(c(0,1), c(0,1)), XOR)
test.data <-data.frame(expand.grid(c(0,1),c(0,1)))

print(net0 <- neuralnet(XOR~Var1+Var2,  xor.data, hidden=0, rep=1, algorithm ='backprop', err.fct="ce", linear.output=FALSE))
plot(net0)

# Calcula as predições para a base de teste
predicted.nn.values0 <- compute(net0,test.data)
predicted.nn.values0

#-----------------------------------------------------------------------
print(net1 <- neuralnet(XOR~Var1+Var2,  xor.data, hidden=1, rep=1, algorithm ='backprop', err.fct="ce", linear.output=FALSE))
plot(net1)

# Calcula as predições para a base de teste
predicted.nn.values1 <- compute(net1,test.data)
predicted.nn.values1

#Resume a saída da rede neural
prediction(net1)

