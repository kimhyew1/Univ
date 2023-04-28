method_cd <- function(fn, init_val, tol=0.01, learn_rate=0.001) {
  x_old <- init_val
  x_new <- init_val
  cnt <- 1
  
  while(1) {
    x_new[1] <- sum((y - x[,2] * x_old[2]) * x[,1]) / sum(x[,1]**2)
    x_new[2] <- sum((y - x[,1] * x_new[1]) * x[,2]) / sum(x[,2]**2)

    cnt <- cnt + 1
    if (abs(fn(x_new) - fn(x_old)) < tol) {
       break
    }
    x_old <- x_new
  }
  return(list(cnt=cnt,
              val_opt=x_new,
              val_fun=fn(x_new)))
}

data("USArrests")
y <- matrix(USArrests[,1])
x <- as.matrix(cbind(1, USArrests[,2]))

eval_f <- function(y, x, beta) {
  0.5 * sum((y - x %*% beta) ** 2)
}

method_cd(fn=function(x) eval_f(y, x, beta=x),
          #원래 x,y 둘 다 넣어야 식이 계산되었는데, 이제 x만 넣어도 가능하도록
          init_val=rep(0, ncol(x)),
          learn_rate=1e-4,
          tol=1e-7)

eval_f1 <- function(y, x, beta=x) {
  - t(x) %*% y + t(x) %*% x %*% beta
}

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

method_gd(fn=function(x) eval_f(y, x, beta=x),
          fn_deriv = function(x) eval_f1(y, x, beta=x),
          init_val=c(0, 0),
          learn_rate=1e-7,
          tol=1e-5)
