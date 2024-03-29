# Code example from slidwes
ALPHA=0.05 # learning parameter
nodes=c(5,8,5,1) # 5 inputs, 2 hidden layers, with 7 and 10 nodes , 1 output
nlayers=length(nodes) -1 # 3 sets of weights
net=list() # set up empty list
# net[[ j ]] holds weight matrix feeding nodes of layer j+1 from nodes in layer j

# Create weights fo each set of nodes ----
# make weights and fill with random numbers
for(j in 1:nlayers) net[[ j ]] <- matrix(runif(nodes[ j ]*nodes[j+1]),nodes[j+1],nodes[j])

# Apply the weights to the test file and re-scale with the sigmoid funtion
netsays <- function(x) { # Returns net output for some input vector x
  for(j in 1:nlayers) x <- 1/(1+exp(-net[[ j ]] %*% x))  #compare with the tanh function
  return(x)
}
backprop <- function(layer,n1,n2,factor){ # recursive function used for back-propagation
  if(layer>1) for(n in 1:nodes[layer-1])
    backprop(layer-1,n2,n,factor*net[[layer]][n1,n2]*r[[layer]][n2]*(1-r[[layer]][n2]))
  net[[layer]][n1,n2] <<- net[[layer]][n1,n2] - ALPHA*factor*r[[layer]][n2]
}
netlearns <- function(x,truth) { # like netsays but changes weights
  r <<- list() # to contain the outputs of all nodes in all layers
  r[[1]] <<- x # the input layer
  for(layer in 1:nlayers) r[[layer+1]] <<- as.vector(1/(1+exp(-net[[layer]] %*% r[[layer]])))
  u <- r[[nlayers+1]] # final answer, for convenience
  for(n in 1:nodes[nlayers]) backprop(nlayers,1,n,(u-truth)*u*(1-u))
}

# Test1 ----

sample1 <- read.table("Sample1", header = FALSE)
truth1 <-  sample1[,1]
sample1t <- sample1[,-1]


#Initial pred
v1 <- as.numeric(sample1t[1,])
netsays(v1)
