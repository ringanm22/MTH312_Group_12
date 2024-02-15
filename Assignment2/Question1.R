ecg <- read.csv("ecg.csv", header = F)

set <- sample(1:4998, 100)
joined_ecg <- ecg[set, ]
rownames(joined_ecg) <- NULL
ecg1 <- filter(ecg, V141 == 0)[1:50, ][, -141] 
ecg2 <- filter(ecg, V141 == 1)[1:50, ][, -141]

library(empichar)
############
# Set parameters
n <- 50 
p <- 140

#mean_vec <- rep(0, p)
#cov_mat <- diag(p) # Identity matrix of size p
X <- as.matrix(ecg1)
Y <- as.matrix(ecg2)

# p-1 thetas is to be generated
# s.t. 0 < theta_1 < 2pi and all other o to pi
projections <- function(p){
  
  thetas <- numeric(length = p-1)
  thetas[1] <- runif(1,0,2*pi)
  thetas[c(2:(p-1))] <- runif(p-2,0,pi)
  
  sin_values <- sin(thetas)
  cos_values <- cos(thetas)
  
  projection <- numeric(length = p)
  for(i in 1:p){
    if(i==1){
      projection[i] <- prod(sin_values)
    }
    else if(i==(p)){
      projection[i] <- cos_values[i-1]
    }
    else{
      projection[i] <- cos_values[i-1]*prod(sin_values[i:(p-1)])
    }
  }
  return(projection)
}

## ||t|| = r
r <- 1
t1 <- matrix(data = NA,nrow = 50, ncol = p) # 20 different t's
for(i in 1:dim(t1)[1]){
  t1[i,] <- r*projections(p)
}

t2 <- matrix(data = NA,nrow = 50, ncol = p) # 20 different t's
for(i in 1:dim(t2)[1]){
  t2[i,] <- r*projections(p)
}

e_1 <- ecf(t1, X)
e_2 <- ecf(t2, Y)

# Prepare to store combinations
combinations <- matrix(data = NA,nrow = 2500, ncol = 2*p)

# Generate combinations
for(k in 1:nrow(combinations)){
  for (i in 1:nrow(t1)) {
    for (j in 1:nrow(t2)) {
      combinations[k,] <- c(t1[i,],t2[j,])
    }
  }
}
combinations[1,]

jointXY <- cbind(X,Y)

e1.2 <- as.matrix(expand.grid(e_2,e_1))

e_12 <- ecf(combinations,jointXY)

marginal_prod <- e1.2[,1]*e1.2[,2]
e_12 <- ecf(combinations,jointXY)
marginal_prod <- e1.2[,1]*e1.2[,2]
plot(e_12-marginal_prod, xlab = "Real part", ylab = "Imaginary part", 
     main = "Difference between the joint characteristic function and product of \nindividual characteristic function", pch = 20)


hist(abs(e_12-marginal_prod), col = "white", main = 'Histogram of absolute error', xlab = 'Absolute error')
