#####################################
####  1. PARAMETERS OF THE GAME  ####
    N <-  t1    # Number of battlefields
    v_min <- 1
    v_max <- 8
    m <- 5*t2     #number of troops of player A
    p <- 6*t2   #number of troops of player B (which is >= m) 
    #Here lambda:= p/m = 6/5 
    
print(paste("Parameters = ",N,v_min,v_max,m,p))
n_min <- 2*(v_max * p)/ (v_min * m)
source("condition check.R")     #Call the source file that is used to check if condition N >= 2 (v_max/v_min)(p/m)
source("CSF.R")                 #Call the source file that is used to to define the Blotto rule
source("BF values.R")           #Call the source file that is used to create an array of battlefields values v
source("DIU-strategy.R")        #Call the source file that is used to create a realization of DIU_strategy (Algorithm 1)
source("DynPro.R")              #Call the source file that is used to to write the Dynamic Programming algorithm (Algorithm 2)

condition <- 1          #Initialize Condition=1.
condition_check()       #Function from ("condition check.R") to check N >= 2 (v_max/v_min)(p/m).

# Choose to create new values of battlefields or to read from data
if (condition == 1){
  #v <- c(3,1,1,1,1,1,1)  #Example of an array of battlefields values: v
  
  create_value()     #Function from ("BF values.R") to create a vector of battlefields' values (depends on N, m, p)
  #read_value()      #Function from ("BF values.R") to read a vector of battlefields' values from value_latest.csv

  
  
###############################################
####  2. EVALUATE THE EPSILON-EQUILIBRIUM  ####
for (R in 1:3){         # Number of repeated instances that are used to take the average results.
  start <- proc.time()  # Start counting the elapse_time
  ########### 2.1. Payoff of players when both play (DIU,DIU) ###########
  ###########  Approximate F_{A^D_i} and F_{B^D_i} by their eCDFs #############
  T <- 10*N        # Number of generating realizations to find eCDF
  u_A_sum = 0      # Initialize the payoff of A and B after T times running
  u_B_sum = 0
  
  DIU_store_A <- matrix(data = NA, nrow = T, ncol = N) #Initialize a matrix to store DIU realizations
  DIU_store_B <- matrix(data = NA, nrow = T, ncol = N)
 
  for (t in 1:T){
      DIU_store_A[t,] <- DIU_A()    #Generate a realization of DIU_A by the function from ("DIU-strategy.R") store it to DIU_store_A
      DIU_store_B[t,] <- DIU_B()    #Generate a realization of DIU_A by the function from ("DIU-strategy.R") store it to DIU_store_A
      A_gain = 0                    #Initialize what A gain from battlefields
      for (i in 1:N){
        A_gain = A_gain + v[i]*CSF_A(DIU_store_A[t,i],DIU_store_B[t,i])     #CSF_A is the Blotto rule for player A from ("CSF.R")  
      }
      u_A_sum = u_A_sum  + A_gain
      u_B_sum = u_B_sum  + (sum(v)- A_gain)         #The total gain of B is equal to the difference of the total value and total gain of A
  }
  u_A_DIU <- (u_A_sum/T)                            #Average payoff after T times  
  u_B_DIU <- (u_B_sum/T)    

  print(paste("Expected payoffs of player A and B, when both play DIU:", u_A_DIU," and ", u_B_DIU))
  elapseDIU <- proc.time() - start #time the process of generating for ECDF
 
  ####### 2.2 DynPro  u_A(best,DIU) and u_B(DIU,best) ##############
  ####### Running Dynamic Programming for finding Best_response ###############
  start_Dyn <- proc.time() #start counting time
  A_Dyn <- Dynpro_A()            #Function Dynpro_A from ("DynPro.R") to find the best response of player A against the ecdf(DIU_store_B)
  B_Dyn <- Dynpro_B()            #Function Dynpro_B from ("DynPro.R") to find the best response of player B against the ecdf(DIU_store_A)
  print(paste("A's best payoffs against DIU_B: ", A_Dyn))
  print(paste("B's best payoffs against DIU_A: ", B_Dyn))
  

##########################################################
  ######### 4. PRINT AND EXPORTING RESULT ##############
  epsilon <- max(  (abs(u_A_DIU-A_Dyn)),(abs(u_B_DIU-B_Dyn))) /sum(v) 
  elapsed_Dyn <- proc.time() - start_Dyn #time the process
  
  
  print(paste("Epsilon = ", epsilon))
  print(paste("Elapsed time = ", elapsed_Dyn[3]+elapseDIU[3]))
  
  res_matrix <- read.table("result.csv", sep = ",")
  new_res <- cbind(res_matrix,c(N,v_min,v_max,m,p,epsilon,u_A_DIU,u_B_DIU,elapseDIU[3],elapsed_Dyn[3]))
  write.table(new_res,
    file = "result.csv"
    ,row.names = FALSE, col.names = FALSE, sep = "," )
   
  cat("\n")
  cat("\n")
      }   #end of for loop of R condition
  
}   #end the if condition
    
