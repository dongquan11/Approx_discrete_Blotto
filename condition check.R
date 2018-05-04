condition_check <- function(){
  if (N < n_min){
    print("Parameters do not yield the condition N >= 2 (v_max/v_min) (p/m)")
    condition <<- 0
  }
  if (p < m){
    print("Parameters do not yield the condition m <= p)")
    condition <<- 0
  }
  if ( (N <=0) | (m <=0) | (p <= 0) | (v_min <=0) | (v_max <=0)){
    print("Some parameters do not satisfy positivity condition")
    condition <<- 0
  }
}