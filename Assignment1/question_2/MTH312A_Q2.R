
###########################################
df = read.csv("data_for_plot.csv")
###########################################

X <- matrix(data = c(df$x,df$y),nrow = length(df$x),ncol = 2)

plot(X)
n<-350
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

step1 <- function(X, mu, n) {
  Q <- numeric(length = 2)
  for (i in 1:n) {
    if (!any(is.na(c(lhs(X, i, as.matrix(mu), n), rhs(mu))))) {
      if (lhs(X, i, as.matrix(mu), n) <= rhs(mu)) {
        Q <- X[i,]
        break
      }
    }
  }
  
  if (Q[1] == 0 & Q[2] == 0) {
    return(c(FALSE, Q))
  } else {
    return(c(TRUE, Q))
  }
}

r <- 3/10
u <- r*theta_values
mu <- u[2,]
step1(X,mu,200)

step2 <- function(X,n,u){
  iter <- 1
  Qo <- c(runif(n=1,min = mean(X[,1])-1,max=mean(X[,1])+1),runif(n=1,min = mean(X[,2])-1,max=mean(X[,2])+1))
  Xnorms <- numeric(length = n)
  for(i in 1:n){
    Xnorms[i] <- norm(X[i,]-c(mean(X[,1]),mean(X[,2])),type = "2")
  }
  Qmax <- max(Xnorms)
  Qoptim <- optim(par = Qo,fn=eqn,X=X,n=n,u=u,method = "BFGS")
  par <- Qoptim$par
  val <- Qoptim$value
  while( iter<20 & val > 0.2 & norm(par-c(mean(X[,1]),mean(X[,2])),type = "2") > Qmax){
    Qo <- c(runif(1),runif(1))
    Qoptim <- optim(par = Qo,fn=eqn,X=X,n=n,u=u,method = "BFGS")
    par <- Qoptim$par
    val <- Qoptim$value
    iter <- iter +1
    #print(iter)
  }
  return(par)
}

# Generate a sequence of angles from 0 to 360 degrees
num_points <- 25
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


# Generate data
n <- 200
r <- 1/10
u <- r*theta_values
quantile_points1 <- matrix(data = NA,nrow = num_points,ncol = 2)
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
  quantile_points1[i,] <- quant
  print(i)
}

r <- 2/10
u <- r*theta_values
quantile_points2 <- matrix(data = NA,nrow = num_points,ncol = 2)
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
  quantile_points2[i,] <- quant
  print(i)
}

r <- 3/10
u <- r*theta_values
quantile_points3 <- matrix(data = NA,nrow = num_points,ncol = 2)
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
  quantile_points3[i,] <- quant
  print(i)
}

r <- 4/10
u <- r*theta_values
quantile_points4 <- matrix(data = NA,nrow = num_points,ncol = 2)
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
  quantile_points4[i,] <- quant
  print(i)
}

r <- 5/10
u <- r*theta_values
quantile_points5 <- matrix(data = NA,nrow = num_points,ncol = 2)
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
  quantile_points5[i,] <- quant
  print(i)
}

r <- 6/10
u <- r*theta_values
quantile_points6 <- matrix(data = NA,nrow = num_points,ncol = 2)
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
  quantile_points6[i,] <- quant
  print(i)
}

r <- 7/10
u <- r*theta_values
quantile_points7 <- matrix(data = NA,nrow = num_points,ncol = 2)
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
  quantile_points7[i,] <- quant
  print(i)
}

r <- 8/10
u <- r*theta_values
quantile_points8 <- matrix(data = NA,nrow = num_points,ncol = 2)
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
  quantile_points8[i,] <- quant
  print(i)
}

r <- 9/10
u <- r*theta_values
quantile_points9 <- matrix(data = NA,nrow = num_points,ncol = 2)
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
  quantile_points9[i,] <- quant
  print(i)
}


#contour plots
plot(X , ylim = c(0 , 140))
lines(quantile_points1[, 1], quantile_points1[, 2], col = "red", lwd = 2)
lines(quantile_points2[, 1], quantile_points2[, 2], col = "red", lwd = 2)
lines(quantile_points3[, 1], quantile_points3[, 2], col = "red", lwd = 2)
lines(quantile_points4[, 1], quantile_points4[, 2], col = "red", lwd = 2)
lines(quantile_points5[, 1], quantile_points5[, 2], col = "red", lwd = 2)
lines(quantile_points6[, 1], quantile_points6[, 2], col = "red", lwd = 2)
lines(quantile_points7[, 1], quantile_points7[, 2], col = "red", lwd = 2)
lines(quantile_points8[, 1], quantile_points8[, 2], col = "red", lwd = 2)
lines(quantile_points9[, 1], quantile_points9[, 2], col = "red", lwd = 2)

#===========================================================================


#Sunburst plot for bivariate data

#install.packages("aplpack")
library(aplpack)
bagplot(data_3d,create.plot=TRUE,
        show.outlier=TRUE,show.looppoints=TRUE,
        show.bagpoints=TRUE,dkmethod=2,
        show.whiskers=TRUE,show.loophull=FALSE,
        show.baghull=TRUE,verbose=FALSE, xlab = "glucose conc.", ylab = "Blood Pressure" , main= "Sunburst plot")
#=================================================================================================================
