CSF_A <- function(x,y){
  if (x > y){
    win <- 1
  } else{
    win <- 0
  }
  return(win)
}

CSF_B <- function(x,y){
  if (y >= x){
    win <- 1
  } else{
    win <- 0
  }
  return(win)
}