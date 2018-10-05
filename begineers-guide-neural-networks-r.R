#A Beginner's Guide to Neural Networks with R!
#http://www.kdnuggets.com/2016/08/begineers-guide-neural-networks-r.html

#tenta mudar o local da instalação do SAS
.libPaths()
.libPaths("X:/R/library")
#install.packages('neuralnet')

#dados de colégios
#install.packages('ISLR')
library(ISLR)
print(head(College,2))

#normalize data before training a neural network
#We will use the built-in scale() function in R to easily normalize.
# Create Vector of Column Max and Min Values
maxs <- apply(College[,2:18], 2, max)
mins <- apply(College[,2:18], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(College[,2:18],center = mins, scale = maxs - mins))

# Check out results
print(head(scaled.data,2))

#split our data into a training set and a test set.
#We will use the caTools to randomly split the data into a training set and test set.

# Convert Private column from Yes/No to 1/0
Private = as.numeric(College$Private)-1
data = cbind(Private,scaled.data)

#install.packages('bitops')
#install.packages('caTools')
library(caTools)
set.seed(101)

# Create Split (any column is fine)
split = sample.split(data$Private, SplitRatio = 0.70)

# Split based off of split Boolean Vector
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

#Neural Network Function
#create a formula to insert into the machine learning model.
feats <- names(scaled.data)

# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('Private ~',f)

# Convert to formula
f <- as.formula(f)

f

#install.packages('neuralnet')
library(neuralnet)
nn <- neuralnet(f,train,hidden=c(10,10,10),linear.output=FALSE)
#nn <- neuralnet(f,train,hidden=10,linear.output=FALSE)

#Predictions and Evaluations

# how well we performed! 
#We use the compute() function with the test data (jsut the features) to create predicted values. 
#This returns a list from which we can call net.result off of.

# Compute Predictions off Test Set
predicted.nn.values <- compute(nn,test[2:18])

# Check out net.result
print(head(predicted.nn.values$net.result))
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
table(test$Private,predicted.nn.values$net.result)

#We can visualize the Neural Network by using the plot(nn) command. 
#The black lines represent the weighted vectors between the neurons. 
#The blue line represents the bias added. 
#Unfortunately, even though the model is clearly a very powerful predictor, 
#it is not easy to directly interpret the weights. 
#This means that we usually have to treat Neural Network models more like black boxes.
plot(nn)

###############################################################################
# EXEMPLOS DO HELP DO PACOTE neuralnet
###############################################################################

AND <- c(rep(0,7),1)
OR <- c(0,rep(1,7))
binary.data <- data.frame(expand.grid(c(0,1), c(0,1), c(0,1)), AND, OR)
print(net <- neuralnet(AND+OR~Var1+Var2+Var3,  binary.data, hidden=0, rep=10, err.fct="ce", linear.output=FALSE))
plot(net)


data(infert, package="datasets")
print(net.infert <- neuralnet(case~parity+induced+spontaneous, infert, err.fct="ce", linear.output=FALSE, likelihood=TRUE))
plot(net.infert)
#############################
XOR <- c(0,1,1,0)
xor.data <- data.frame(expand.grid(c(0,1), c(0,1)), XOR)
print(net.xor <- neuralnet( XOR~Var1+Var2, xor.data, hidden=2, rep=5))
plot(net.xor, rep="best")
#############################

#############################
XOR <- c(0,1,1,0)
xor.data <- data.frame(expand.grid(c(0,1), c(0,1)), XOR)
print(net.xor <- neuralnet( XOR~Var1+Var2, xor.data, hidden=1, rep=5))
plot(net.xor, rep="best")
#############################

data(infert, package="datasets")
print(net.infert <- neuralnet(case~parity+induced+spontaneous, infert, err.fct="ce", linear.output=FALSE, likelihood=TRUE))
gwplot(net.infert, selected.covariate="parity")
gwplot(net.infert, selected.covariate="induced")
gwplot(net.infert, selected.covariate="spontaneous")
###############################################################################

AND <- c(0,0,0,1)
and.data <- data.frame(expand.grid(c(0,1), c(0,1)), AND)

print(net <- neuralnet(AND~Var1+Var2,  and.data, hidden=2, rep=1, err.fct="ce", linear.output=FALSE))
plot(net)

print(net <- neuralnet(AND~Var1+Var2,  and.data, hidden=2, rep=1, err.fct="ce", linear.output=FALSE))
plot(net)


print(net <- neuralnet(AND~Var1+Var2,  and.data, hidden=2, rep=1, err.fct="ce", linear.output=FALSE))
plot(net)

print(net <- neuralnet(AND~Var1+Var2,  and.data, hidden=c(2,2), rep=1, err.fct="ce", linear.output=FALSE))
plot(net)
