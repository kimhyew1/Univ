eval_f <- function(x) {
  (x-1) **2
}

eval_f1 <- function(x) {
  2*(x-1)
}

x_old <- 10
x_new <- x_old - 0.001 * eval_f1(x_old)
x_old <- x_new

# Gradient Descent Method
method_gd_exact <- function(fn, fn_deriv, init_val, tol=0.01, learn_rate=eta) {
  x_old <- init_val
  cnt <- 1
  
  while (1) {
    if (fn_deriv(x_old) == 0) {
      break
    }
    
    learn_rate <- (x_old - 1) / fn_deriv(x_old)
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

method_gd_exact(fn=eval_f,
                fn_deriv=eval_f1,
                init_val=10,
                tol=0.01)

# Error Occur
# Error in if (abs(fn(x_new) - fn(x_old)) < tol) { : 
#   missing value where TRUE/FALSE needed
# >> learn_rate의 분모=0 (x_old=1) 인 경우 있어서 error 발생
# >> x_old==1 일 때는 계산 중지하도록 짜야함
