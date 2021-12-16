# direct inversion
r_squared_fun_1 = function(x,y){
  y = y - mean(y)
  x = sweep(x,2,colMeans(x),"-")
  SSE_intercept_only = sum(y^2)
  xty = crossprod(x,y)
  xtx = crossprod(x)
  beta = solve(xtx)%*%xty
  SST = crossprod(xty,beta)[1,1]
  r_squared = SST/SSE_intercept_only
  return(r_squared)
}

# solve linear system
r_squared_fun_2 = function(x,y){
  y = y - mean(y)
  x = sweep(x,2,colMeans(x),"-")
  SSE_intercept_only = sum(y^2)
  xty = crossprod(x,y)
  xtx = crossprod(x)
  beta = solve(xtx,xty)
  SST = crossprod(xty,beta)[1,1]
  r_squared = SST/SSE_intercept_only
  return(r_squared)
}

# invert using eigen system
r_squared_fun_3 = function(x,y){
  y = y - mean(y)
  x = sweep(x,2,colMeans(x),"-")
  SSE_intercept_only = sum(y^2)
  xty = crossprod(x,y)
  xtx = crossprod(x)
  eigen_xtx = eigen(xtx,symmetric=TRUE)
  u = eigen_xtx$vectors
  d_inv = diag(1/eigen_xtx$values)
  beta = u %*% d_inv %*% t(u) %*% xty
  SST = crossprod(xty,beta)[1,1]
  r_squared = SST/SSE_intercept_only
  return(r_squared)
}

# avoid inversion using svd
r_squared_fun_4 = function(x,y){
  y = y - mean(y)
  x = sweep(x,2,colMeans(x),"-")
  SSE_intercept_only = sum(y^2)
  svd_x = svd(x)
  u = svd_x$u
  uty = crossprod(u,y)
  SST = crossprod(uty)[1,1]
  r_squared = SST/SSE_intercept_only
  return(r_squared)
}

# avoid inversion using svd
# ignore right singular vectors
r_squared_fun_5 = function(x,y){
  y = y - mean(y)
  x = sweep(x,2,colMeans(x),"-")
  SSE_intercept_only = sum(y^2)
  svd_x = svd(x,nv=0)
  u = svd_x$u
  uty = crossprod(u,y)
  SST = crossprod(uty)[1,1]
  r_squared = SST/SSE_intercept_only
  return(r_squared)
}

