eigen_fun = function(X,tol=.Machine$double.eps^0.75){
  if(!isSymmetric(X)) stop("x is not symmetric")
  d = ncol(X)
  U = diag(1,d,d,FALSE)
  while(TRUE){
    did_we_update = FALSE
    for(i in 1:(d-1)){
      for(j in (i+1):d){
        # test whether abs(X[i,j]) is too large
        if(abs(X[i,j])>tol){
          # if so, get the Givens Rotation matrix R
          # and update X and U

          # track that in this sweep through we have done at least one update
          did_we_update = TRUE
        }
        
      }
    }
    if(!did_we_update) break
  }
  values=diag(X)
  sorted_values = sort(values,index.return=TRUE,decreasing=TRUE)
  values = sorted_values$x
  vectors = U[,sorted_values$ix]
  return(list(values=values, vectors=vectors))
}
