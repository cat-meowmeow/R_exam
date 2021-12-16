# setting project directory as working directory
setwd("D:/hacxy/Downloads/Final Exam/Gibbs Sampling for Bayesian Laplace Regression/")

# sources header relative to the path for this file
# source("final_q2_files/headers/header.R")
library(mvtnorm)
library(SuppDists)
load("final_q2_data.RData")
x = as.matrix(x)# 442*10
y = as.matrix(y)#  442*1

################### headers################
draw_tau = function(data,state){
  shape = 1 + length(data$y)
  rate = 1 + sum(abs(data$y-data$x%*%state$beta))
  return(rgamma(1,shape,rate))# a number, tau
}

draw_beta = function(data,state){
  #browser()
  n = length(y)# n = 442
  m = ncol(x)# m = 10
  means_z = 1/(state$tau*abs(data$y-data$x%*%state$beta))  # means of nu,442*1
  shape_z = 1 # a vector of shape 1
  z <<- rinvGauss(n,means_z,shape_z) # z values,vector n,442
  
  Z = diag(z)
  a_ = solve(state$tau^2*t(data$x)%*%Z%*%(data$x)+diag(m)) #  10 *10
  mean_beta =a_%*%(state$tau^2*t(data$x)%*%Z%*%data$y) # rep(mean,len.col)
  #sigma = a_ # diag(len)
  #rmvnorm(nsize.row,mean_beta,sigma) # nsize:rows    rep(mean,column),   sigma diag(column)
  return(rmvnorm(m,mean_beta,diag(10)))
}

update_current_state = function(data, state){
  state$tau = draw_tau(data,state) # a number, tau
  # state$beta = draw_beta(data,state) # Not this
  
  ## Draw a vector z from draw_beta
  #draw.z = z # vector, 442 length
  ## Draw a beta vector from multivariate normal
  draw.beta = draw_beta(data,state) # 10*10
  state$beta = draw.beta[,sample(1:10,1)] # draw one column, num [1:10]
  return(state)
}

################### main #############
gibbs_laplace_regression = function(N_draws,x,y,beta_0,tau_0){
  # browser()
  # data manipulation 
 
#   Sorry I delete this, it's annoying to deal with data with characters in data frame. I converted them to matrix.
#  if(is.null(colnames(x))){
#    colnames(x) = paste("x",1:ncol(x),sep="_")
#  }
#  if(!all(abs(x[,1]-1)<.Machine$double.eps)){
#    x = cbind(c(1),x)
#    colnames(x)[1] = "intercept"
#  }
  data = list(y=y,x=x)
  
  # checking for missing initial values and making them if necessary
  if(missing(beta_0)) beta_0 =c(median(data$y),rep(0,ncol(data$x)-1))# beta_0 1*10 vector 
  if(missing(tau_0)) tau_0 = 1/mean(abs(data$y-data$x%*%beta_0)) # 442*1-442*10(1)= 442*1 --> tau_0: num
  
  # initial state creation
  current_state = list(beta=beta_0,tau=tau_0)
  
  # output creation
  out = list(beta=matrix(NA,ncol(x),N_draws),tau=rep(NA,N_draws)) #beta:10*10000 # tau:1*10000
  # rownames(out$beta) = colnames(x)
  
  # loop for sampling
  for(i in 1:N_draws){
    current_state = update_current_state(data,current_state)
    out$beta[,i] = current_state$beta
    out$tau[i] = current_state$tau
  }
  
  # returning
  return(out)
}

# test 100
# gibbs_laplace_regression(100,x,y)

###################### Get  10000 draws
result = gibbs_laplace_regression(10000,x,y)
# result$beta
# result$tau
apply(result$beta,1,mean)# posterior means of the beta draws
summary(result$tau)[4] # posterior means of the tau draws

##########################################
#make univariate 95 intervals for each element of beta#
mean_ = apply(result$beta,1,mean)
sd = apply(result$beta,1,sd)

lower = mean_ - 1.96*sd/100
upper = mean_ + 1.96*sd/100
ConfidenceInterval = rbind(lower,upper)
ConfidenceInterval

# I think the column 1,2,5,6,7,8 are non-zero in this Laplace regression. (Looking into that data could know their names. I do not want to check that...)
