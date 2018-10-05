.libPaths("X:/R/library")
library(neuralnet)

#Redes Neurais função "E"#
##########################
AND <- c(0,0,0,1)
and.data <- data.frame(expand.grid(c(0,1), c(0,1)), AND)
test.data <-data.frame(expand.grid(c(0,1),c(0,1)))

print(net0 <- neuralnet(AND~Var1+Var2,  and.data, hidden=0, rep=1, err.fct="ce", linear.output=FALSE))
plot(net0)

print(net1 <- neuralnet( AND~Var1+Var2, and.data, hidden=1, rep=500))
plot(net1, rep="best")

# Calcula as predições para a base de teste
predicted.nn.values0 <- compute(net0,test.data)
predicted.nn.values0

#-----------------------------------------------------------------------
print(net1 <- neuralnet(AND~Var1+Var2,  and.data, hidden=1, rep=1, err.fct="ce", linear.output=FALSE))
plot(net1)

# Calcula as predições para a base de teste
predicted.nn.values1 <- compute(net1,test.data)
predicted.nn.values1

#Resume a saída da rede neural
prediction(net1)


#-----------------------------------------------------------------------
print(net2 <- neuralnet(AND~Var1+Var2,  and.data, hidden=2, rep=1, err.fct="ce", linear.output=FALSE))
plot(net2)



#-----------------------------------------------------------------------
print(net2_2 <- neuralnet(AND~Var1+Var2,  and.data, hidden=c(2,2), rep=1, err.fct="ce", linear.output=FALSE))
plot(net2_2)

# Calcula as predições para a base de teste
predicted.nn.values2_2 <- compute(net2_2,test.data)
predicted.nn.values2_2