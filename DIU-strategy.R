DIU_A <- function(){
  x_A <- vector(mode="numeric", length= N)
  a <- vector(mode="numeric", length= N)
  count_0 <- N
  while (count_0 == N){
    for (i in 1:N){
      #a is generated from A_star
      # set the probability of zero
      pro_0 <- sample(c(0, 1), size = 1, replace = TRUE, prob = c( 1 - (m/p), m/p ))
      if(pro_0 == 1){
        a[i] <- runif(1, min = 0, max = 2*(v[i] *p)/(sum(v) * m))
        count_0 <- count_0 - 1
      } else{
        a[i] <- 0
      }
    }
  }
  #a <- c(0.34, 0 , 0.2, 0.2, 0.2, 0.2, 0.19)          #For checking
  #rounding A_star
  temp <- vector(mode="numeric", length= N+1) #a vector to store found a_hat
  a_sum <- 0
  for (i in 1:N){
      a_sum <- a_sum + (a[i]/ sum(a))
    for (a_hat in 0:m) {
      if ( ((a_hat/m)-(1/(2*m)) <= a_sum) & (a_sum <= (a_hat/m) + (1/(2*m)))) {
        temp[i+1] <- a_hat 
        break
      } 
    }
    x_A[i] <- temp[i+1] - temp[i]
  }
  return(x_A)
}

DIU_B <- function(){
  x_B <- vector(mode="numeric", length= N)
  b <- vector(mode="numeric", length= N)
  for (i in 1:N){
    #b is generated from B_star
     b[i] <- runif(1, min = 0, max = 2 * (v[i] *p)/ (sum(v) * m))
  }
  #b <- c(0.34, 0 , 0.2, 0.2, 0.2, 0.2, 0.19)          #For checking
  #rounding B_star
  temp <- vector(mode="numeric", length= N+1) #a vector to store found b_hat
  b_hat <- 0
  b_sum <- 0
  for (i in 1:N){
    b_sum <- b_sum + (b[i]/ sum(b))
    while (b_hat <= p) {
      if ( ((b_hat/m)-(1/(2*m)) <= (b_sum *p/m)) & ((b_sum*p/m) <= (b_hat/m) + (1/(2*m)))){
        temp[i+1] <- b_hat 
        break
      } else{
        b_hat <- b_hat +1
      } 
    }
    x_B[i] <- temp[i+1] - temp[i]
  }
  return(x_B)
}

