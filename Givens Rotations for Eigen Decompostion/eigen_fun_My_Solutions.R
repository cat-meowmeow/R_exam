eigen_fun <- function(X,tol=.Machine$double.eps^0.75){
  d = ncol(X)
  U = diag(1,d,d,FALSE)
  
  Rlocal <- function(A){
    d = ncol(X)
    R2 <- A
    R1 <- diag(1,d,d,FALSE)
    for (i in 1:(d-1)) {
      X <- matrix(rep(0,d),nrow = d)
      X[i:d] <- R2[i:d,i]
      V <- X
      V[i] <- X[i]+norm(X,"F")*sign(X[i])
      xlocal <- V/norm(V,"F")
      R2 <- R2-2*(xlocal%*%t(xlocal)%*%R2)
      R1 <- R1-2*(xlocal%*%t(xlocal)%*%R1)
    }
    return(list("R1" = t(R1), "R2" = R2))
  }
  
  while(TRUE){
    did_we_update = FALSE
    for(i in 1:(d-1)){
      for(j in (i+1):d){
        # test whether abs(X[i,j]) is too large
        if(abs(X[i,j])>tol){
          # if so, get the Givens Rotation matrix R
          # and update X and U
          for (k in 1:1000){
          update <- Rlocal(U)
          Q = update$R1
          R = update$R2
          U = X %*% Q
          }
          # track that in this sweep through we have done at least one update
          did_we_update = FALSE
        }
        
      }
    }
    if(!did_we_update) break
  }
  
  values = diag(R)
  sorted_values = sort(values,index.return=TRUE,decreasing=TRUE)
  values = sorted_values$x
  vectors = Q[,sorted_values$ix]
  return(list(values=values, vectors= -vectors))
  
}
