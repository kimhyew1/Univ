# not knowing when to stop: while loop
# knowing when to stop: for loop

eval_f <- function(x) {
  (x-1)**2
}

intval <- c(-5, 5)
x_min <- -3
x_max <- 3

eval_f(x_min)
eval_f(x_max)

# bi-section method
if (max(eval_f(x_min), eval_f(x_max)) == eval_f(x_max)) {
  intval[2] <- x_max
  # intval <- c(intval[1], x_max)
} else {
  intval[1] <- x_min
  # intval <- c(x_min, intval[2])
}

# Skeleton - bi-section function
method_bisect <- function(fn, intval, tol=0.01) {
  cnt <- 1

  while ((intval[2]-intval[1]) > tol) {
    
    x_min <- (2*intval[1] + intval[2]) / 3
    x_max <- (intval[1] + 2*intval[2]) / 3
    # 내분점 이상하게 잡았었음
    
    if (max(fn(x_min), fn(x_max)) == fn(x_max)) {
      intval[2] <- x_max
    } else {
      intval[1] <- x_min
    }
    cnt <- cnt + 1
  }
  return(list(cnt=cnt, 
              val_opt=mean(intval), 
              val_fun=fn(mean(intval))))
}

# show the result
method_bisect(eval_f, c(-3,3))

# $cnt
# [1] 17

# $val_opt
# [1] 0.9991806

# $val_fun
# [1] 6.714741e-07
