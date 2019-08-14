# Code example from slidwes
ALPHA=0.05 # learning parameter
nodes=c(5,8,5,1) # 5 inputs, 2 hidden layers, with 7 and 10 nodes , 1 output
nlayers=length(nodes) -1 # 3 sets of weights
net=list() # set up empty list
# net[[ j ]] holds weight matrix feeding nodes of layer j+1 from nodes in layer j

# Create weights fo each set of nodes ----

# make weights and fill with random numbers
set.seed(123)
for(j in 1:nlayers) net[[ j ]] <- matrix(runif(nodes[ j ]*nodes[j+1]),nodes[j+1],nodes[j])

# Apply the weights to the test file and re-scale with the sigmoid funtion ----
netsays <- function(x) { # Returns net output for some input vector x
  for(j in 1:nlayers) x <- 1/(1+exp(-net[[ j ]] %*% x))  #compare with the tanh function
  return(x)
}
# DONT TOUCH ----
backprop <- function(layer,n1,n2,factor){ # recursive function used for back-propagation
    if(layer>1) for(n in 1:nodes[layer-1])
    backprop(layer-1,n2,n,factor*net[[layer]][n1,n2]*r[[layer]][n2]*(1-r[[layer]][n2]))
 
   net[[layer]][n1,n2] <<- net[[layer]][n1,n2] - ALPHA*factor*r[[layer]][n2]
}
netlearns <- function(x,truth){ # like netsays but changes weights
  r <<- list() # to contain the outputs of all nodes in all layers
  r[[1]] <<- x # the input layer
  for(layer in 1:nlayers) r[[layer+1]] <<- as.vector(1/(1+exp(-net[[layer]] %*% r[[layer]])))
  u <- r[[nlayers+1]] # final answer, for convenience
  for(n in 1:nodes[nlayers]) backprop(nlayers,1,n,(u-truth)*u*(1-u))
}

# SAMPLE 1 ----
set.seed(123)
sample1 <- read.table("Sample1", header = FALSE)
sample<- sample.int(n = nrow(sample1), size = floor(.75*nrow(sample1)), replace = F)
train1 <- sample1[sample, ]
test1  <- sample1[-sample, ]


truth1_train <-  train1[,1]
sample1t_train <- train1[,-1]


truth1_test <-  test1[,1]
sample1t_test <- test1[,-1]

#Initial pred

v1 <- as.numeric(sample1t[1,])
pred <- netsays(v1)
#net2 <- net
#Backprop ----
#i=1
badness <-1/2*(pred-truth1[i])^2 

netlearns(v1,truth1[i])
#backprop(3,1,1,badness)


#Training 4loop for sigmoid----
#v1 <- as.numeric(sample1t[1,])
set.seed(333)
for(j in 1:nlayers) net[[ j ]] <- matrix(runif(nodes[ j ]*nodes[j+1]),nodes[j+1],nodes[j])

v1 <-  sample1t_train
bad <-c() 
pred <- c()

truth1_test <-  test1[,1]
sample1t_test <- test1[,-1]
predTest <- c()
badTest <- c()

for(i in 1:100){
  for(j in 1:nrow(v1)){
  pred[j] <- netsays(as.numeric(v1[j,]))
  }
  bad[i] <- mean(1/2*((pred-truth1_train)^2))
  #bad[i] <-1/2*(pred-truth1)^2
  for(j in 1:nrow(v1)){
  netlearns(as.numeric(v1[j,]),truth1_train[j])
  }
  for(j in 1:nrow(sample1t_test)){
    predTest[j] <- netsays(as.numeric(sample1t_test[j,]))
    }
    badTest[i] <- mean(1/2*((predTest-truth1_test)^2))
  print(bad[i])
}
plot(bad, type = "l", ylim = c(0,.25));par(new=TRUE)
plot(badTest, type = "o", col = "blue", ylim = c(0,.25))

#Saving the results for the first sample
pred1<-pred
bad1 <- bad

# SAMPLE 2 ------------------------------------------------------------
setwd("~/Documents/NNEts_exc/data")
sample2 <- read.table("Sample2", header = FALSE)
truth2 <-  sample2[,2]
sample2t <- sample2[,-1]


#Test 4loop for sigmoid----
v2 <- as.numeric(sample2t[1,])
v2 <-  sample2t
bad2 <-c() 
pred2 <- c()
for(i in 1:50){
  for(j in 1:nrow(v2)){
    pred2[j] <- netsays(as.numeric(v2[j,]))
  }
  bad2[i] <- mean(1/2*((pred2-truth2)^2))
  #bad[i] <-1/2*(pred-truth1)^2
  for(j in 1:nrow(v2)){
    netlearns(as.numeric(v2[j,]),truth2[j])
  }
  print(bad2[i])
}
plot(bad2, type = "l")

#Saving the results for the first sample
pred2<-pred2
bad2 <- bad2
