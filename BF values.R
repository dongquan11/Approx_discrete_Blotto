#Create/read the values of the battlefields.
create_value <- function (){
    v <<- runif(N, v_min, v_max)                #
    time <- gsub(":","",as.character(Sys.time()))
    write.csv(c(N,m,p,v), "values_latest.csv")  #write down the vector into the file to use later
}

read_value <- function(){
  value = read.csv("values_latest.csv")
  v <<- as.numeric(value[4:(N+3),2])
}
