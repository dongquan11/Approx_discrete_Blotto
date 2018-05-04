Dynpro_A <- function(){
  #APPROACH 1
  #APPROACH 1
  H <- matrix(data=0, nrow = m+1, ncol = N+1)
  V <- matrix(data=0, nrow = m+1, ncol = N+1)
  
  for (i in 1:(m+1)) {
    for (j in 2:(N+1)){                                    #battlefield 1 to N
      H[i,j] <- v[j-1]* (ecdf(DIU_store_B[,j-1])(i-2)) #B plays < i (because A is weaker)
      
      temp <- 0
      for (k in 1:i){
        temp <- max(temp, V[k,j-1] + H[i-k+1,j])
      }
      V[i,j] <- temp
    }
  }
  
  Opt <- V[m+1,N+1]
  return(Opt)
  
}
#############################################################################################
#############################################################################################
Dynpro_B <- function(){
  #APPROACH 1
  H <- matrix(data=0, nrow = p+1, ncol = N+1)
  V <- matrix(data=0, nrow = p+1, ncol = N+1)
  
  for (i in 1:(p+1)) {
    for (j in 2:(N+1)){                                    #battlefield 1 to N
      H[i,j] <- v[j-1]* (ecdf(DIU_store_A[,j-1])(i-1)) #B plays < i (because A is weaker)
      
      temp <- 0
      for (k in 1:i){
        temp <- max(temp, V[k,j-1] + H[i-k+1,j])
      }
      V[i,j] <- temp
    }
  }
  
  Opt <- V[p+1,N+1]
  return(Opt)
}