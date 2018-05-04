
Best_payoff_B <- function(x_A){
  
  obj_par <- vector(mode = "numeric", length = 2*N)
  for (i in 1:N){
    obj_par[N+i] <- v[i]
  }

  M_con <- c(rep(1,N), rep(0,N))    #starting with constraint (3)
  M_rhs <- p                        #starting with rhs of (3)
  # Constraints groups (1) and (2):
  for (i in 1:N){
    temp <- vector(mode = "numeric", length = 2*N)
    temp[i] <- 1
    temp[N+i] <- (-m)
    M_con <- rbind(M_con, temp)
    M_rhs <- c(M_rhs, x_A[i] - m )
    
    temp[N+i] <- (-p-1)
    M_con <- rbind(M_con, temp)
    M_rhs <- c(M_rhs, x_A[i] - 10^(-5))
  }
  #print (M_con)     #For checking
  #print (M_rhs)     #For checking
  
  M_direction <- c("<=", rep( c(">=","<"),N)  )
  #print(M_direction)   #For checking
  
  #checking the solution:
  #Find the best payoff of B
  Bin <- lp ("max", obj_par, M_con, M_direction, M_rhs, all.int=TRUE, binary.vec = (N+1):(2*N), presolve = 1 ) 
  #print(Bin[12])          #print solution to check
  #return(as.numeric(Bin[11]))
  return(as.vector(Bin[12]))
}

##################################################################
Best_payoff_A <- function(x_B){
  
  obj_par <- vector(mode = "numeric", length = 2*N)
  for (i in 1:N){
    obj_par[N+i] <- v[i]
  }
  
  M_con <- c(rep(1,N), rep(0,N))    #starting with constraint (3)
  M_rhs <- m                        #starting with rhs of (3)
  # Constraints groups (1) and (2):
  for (i in 1:N){
    temp <- vector(mode = "numeric", length = 2*N)
    temp[i] <- 1
    temp[N+i] <- (-(p+1))
    M_con <- rbind(M_con, temp)
    M_rhs <- c(M_rhs, x_B[i] - (p+1) + 10^(-5) )
    
    temp[N+i] <- (-m)
    M_con <- rbind(M_con, temp)
    M_rhs <- c(M_rhs, x_B[i])
  }
  #print (M_con)     #For checking
  #print (M_rhs)     #For checking
  
  M_direction <- c("<=", rep( c(">","<="),N)  )
  #print(M_direction)   #For checking
  
  #checking the solution:
  #Find the best payoff of B
  Ain <- lp ("max", obj_par, M_con, M_direction, M_rhs, all.int=TRUE, binary.vec = (N+1):(2*N), presolve = 1 ) 
  #print(Ain[12])          #print solution to check
  #print(as.numeric(Ain[11]))
  return(Ain$solution)
}













Best_payoff_B_verAPI<- function(x_A){
  
  best_res <- make.lp(nrow = (2*N) +1, ncol=2*N ) #create a model to optimizize
  
  obj_par <- vector(mode = "numeric", length = 2*N)
  for (i in 1:N){
    obj_par[N+i] <- (-v[i])
  }
  set.objfn(best_res , obj_par)  #set the parameters of objective function
  
  add.constraint(best_res, c(rep(1,N), rep(0,N)), type = "<=", p)   #contraint (3)
  
  for (i in 1:N){
    set.type(best_res, i, type="integer")
    set.type(best_res, N+i, type="binary")
  
    # Constraints groups (1) and (2):  
    temp <- vector(mode = "numeric", length = 2*N)
    temp[i] <- 1
    temp[N+i] <- (-m)
    add.constraint(best_res, temp, type = ">=", x_A[i]-m)
    
    temp[N+i] <- (-p-1)
    add.constraint(best_res, temp, type = "<", x_A[i])
  }
  #Integer and binary constraints
  write.lp(best_res, 'best_res',type="lp")
  solve(best_res)
  #print(get.sensitivity.obj(best_res))
  
}