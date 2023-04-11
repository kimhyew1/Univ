# CC Biplot for First and Second Sons' Head
# Data Matrix
data <- read.table('head.txt', header=T)
n = nrow(data)

# Sets of Variables: X, Y
X <- data[, 1:2]
X <- scale(X, scale=F)
Y <- data[, 3:4]
Y <- scale(Y, scale=F)

# Covarianve Matrix S
S <- cov(data)
Sxx <- coc(X)
Syy <- cov(Y)
Sxy <- t(X) %*% Y/(n-1)

Exx <- eigen(Sxx)
Eyy <- eigen(Syy)
Sx <- Exx$vectors %*% diag(sqrt(Exx$values)) %*% t(Exx$vectors)
Sy <- Eyy$vectors %*% diag(sqrt(Eyy$values)) %*% t(Eyy$vectors)

# SVD: k=SxSxySy
K <- solve(Sx) %*% Sxy %*% solve(Sy)
svd.K <- svd(K)
U <- solve(Sx) %*% svd.K$u
V <- solve(Sx) %*% svd.K$v

# Rows and Columns Coordinates: Rx Cx Ry Cy
Rx <- (X %*% U %*% diag(svd.K$d))[, 1:2]
Cx <- (U %*% diag(svd.K$d))[, 1:2]
Ry <- (Y %*% U %*% diag(svd.K$d))[, 1:2]
Cy <- (V %*% diag(svd.K$d))[, 1:2]
rownames(Cx) <- colnames(X)
rownames(Cy) <- colnames(Y)

# Goodness-of-fit
cc <- svd.K$d
eig <- (svd.K$d)**2
per <- eig / sum(eig) * 100
cc;eig;per

# CC Biplot
par(mfrow=c(1, 2))
par(pty='s')
biplot(Rx, Cx, xlab='1st Dimension', ylab='2nd Dimension', main='(a) CC Biplot for First Sons', xlm=c(-1.5, 1.5), ylim=c(-0.2, 0.2), cex=0.8, pch=16)
abline(v=0, h=0)
biplot(Ry, Cy, xlab='1st Dimension', ylab='2nd Dimension', main='(b) CC Biplot for Second Sons', xlm=c(-1.5, 1.5), ylim=c(-0.2, 0.2), cex=0.8, pch=16)
abline(v=0, h=0)
