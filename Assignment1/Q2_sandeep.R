library(MASS)

# used in step 1
first_lhs <- function(X,i,n)
{
  sum <- numeric(length = 2)
  sum[1] <- 0
  sum[2] <- 0
  for(j in 1:n){
    if(i==j){next}
    sum <- sum + (X[j, ]-X[i, ])/norm(X[j, ]-X[i, ],type = "2")
  }
  return(sum)
}

#used in step 1
rhs <- function(u){
  return(1+norm(u,type = "2"))
}

#used in step 1
lhs <- function(X,i,u,n){
  first_term <- as.matrix(first_lhs(X,i,n))
  return(norm(first_term + (n-1)*u,type ="2"))
}

#used in step 2
eqn <- function(X,n,u,Q){
  lhs <- c(0,0)
  for(i in 1:n){
    lhs <- lhs + (X[i,]-Q)/norm(X[i,]-Q,type = "2")
  }
  lhs <- lhs + n*u
  return(norm(lhs,type = "2"))
}

step1 <- function(X,mu,n){
  Q <- numeric(length = 2)
  for(i in 1:n){
    if(lhs(X,i,as.matrix(mu),n)<=rhs(mu)){
      Q <- X[i,]
      break
    }
  }
  if(Q[1]==0 & Q[2]==0){return(c(FALSE,Q))}
  else{return(c(TRUE,Q))}
}
step2 <- function(X,n,u){
  iter <- 1
  Qo <- c(runif(1),runif(1))
  Xnorms <- numeric(length = n)
  for(i in 1:n){
    Xnorms[i] <- norm(X[i,]-c(2,5),type = "2")
  }
  Qmax <- max(Xnorms)
  Qoptim <- optim(par = Qo,fn=eqn,X=X,n=n,u=u,method = "BFGS")
  par <- Qoptim$par
  while( iter<20 & norm(par-c(2,5),type = "2")>Qmax){
    Qo <- c(runif(1),runif(1))
    Qoptim <- optim(par = Qo,fn=eqn,X=X,n=n,u=u,method = "BFGS")
    par <- Qoptim$par
    iter <- iter +1
    #print(iter)
  }
  return(par)
}

# Generate a sequence of angles from 0 to 360 degrees
num_points <- 50
angles <- seq(0, 358, length.out = num_points)

# Function to convert degrees to radians
deg2rad <- function(degrees) {
  return(degrees * pi / 180)
}

# Calculate cosine and sine values for each angle
cos_values <- cos(deg2rad(angles))
sin_values <- sin(deg2rad(angles))

theta_values <- as.matrix(cos_values)
theta_values <- cbind(theta_values,as.matrix(sin_values))

r <- 9/10 # mod u
u <- r*theta_values

# Generate data
n <- 100
sig <- matrix(data = c(1,0,0,1),nrow = 2)
X <- mvrnorm(n=n,mu=c(2,5),Sigma = sig)

# r= 1/3 let store value for whole circle
# for num_points differnt u
quantile_points <- matrix(data = NA,nrow = num_points,ncol = 2)

for(i in 1:num_points){
  mu <- u[i,]
  quant <- c(0,0)
  f1 <- step1(X,mu,n)
  if(f1[1]==1){
    quant <- c(f1[2],f1[3]) # if found in first step
  }
  else{
    quant <- step2(X,n,mu)
  }
  quantile_points[i,] <- quant
  print(i)
}

plot(X)
lines(quantile_points[, 1], quantile_points[, 2], col = "red", lwd = 2)
