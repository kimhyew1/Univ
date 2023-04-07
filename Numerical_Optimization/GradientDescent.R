# Gradient Descent Method
eval_f <- function(x) {
  (x-1) **2
}

eval_f1 <- function(x) {
  2*(x-1)
}

x_old <- 10
x_new <- x_old - 0.001 * eval_f1(x_old)
x_old <- x_new

# Skeleton
method_gd <- function(fn, fn_deriv, init_val, tol=0.01, learn_rate=0.001) {
  x_old <- init_val
  cnt <- 1
  while (1) {
    x_new <- x_old - learn_rate * fn_deriv(x_old)
    cnt <- cnt + 1
    if (abs(fn(x_new)- fn(x_old)) < tol) {
      break
    } else {
      x_old <- x_new
    }
  }
  return (list(cnt=cnt,
               val_opt=x_new,
               val_fun=fn(x_new)))
}

# Show the result
method_gd(eval_f, eval_f1, 10)

# $cnt
# [1] 871

# $val_opt
# [1] 2.576934

# $val_fun
# [1] 2.48672
