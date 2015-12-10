##This script builds a simple two layer neural network.

#The sigmoid function
sigmoid <- function(x){1/(1+exp(-x))}

#The non-linear function
nonlin <- function(x){
    x*(1-x)
}


#Sample Input Dataset
x <- matrix(c(0,1,1,0,0,1,0,1,1,1,1,1), nrow = 4, ncol =3)

#Sample Output Dataset
y = matrix(c(0,0,1,1))

#Set random seed
set.seed(11)

#Initialize weights randomly with mean 0
syn0 <- 2*runif(3)-1

for(i in range(1:10000)){
    #forward propagation
    layer.0 <- x
    layer.1 <- sigmoid(layer.0 %*% syn0)

    #how much did we miss?
    layer.1.error <- y - layer.1

    #multiply how much we missed by the slope of the sigmoid at the values
    #in the second layer
    layer.1.delta <- layer.1.error * nonlin(layer.1)

    #update weights
    syn0 <- syn0 + (t(layer.0) %*% layer.1.delta)
}

print("Output After Training:")
print(layer.1)