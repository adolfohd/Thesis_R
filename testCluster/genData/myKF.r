myKF <- function(Ts,
                 z,     # measuredPosition
                 x,     # currentStateVector
                 P,     # currentCovarianceMatrix
                 R,     # measurementCovariance
                 sigma  # ProcessVariance
                 ) {
  T_ = seq(1:5)*0
  T_[1] = Ts
  for (i in 2:5){ # powers of Ts
    T_[i] <- T_[i-1]*T_[1]
  }
  # state transition matrix
    Fm = matrix(c(1,T_[1],T_[2], 
                  0, 1, T_[1], 
                  0, 0 , 1), 
                ncol = 3, nrow = 3, byrow = TRUE)
    H = c(1,0,0)
  # Process Variance Matrix
    Q = sigma * matrix(c(   T_[5]/20,  T_[4]/8,  T_[3]/6, 
                            T_[4]/8,   T_[3]/3,  T_[2]/2, 
                            T_[3]/6,   T_[2]/2,  T_[1]), 
               ncol = 3, nrow = 3, byrow = TRUE)
  # predict
    x_apriori <- Fm %*% x
    P_apriori <- (Fm %*% P) %*% t(Fm) + Q
  # Update
    y <- z - H %*% x_apriori
    S <- t(H) %*% P_apriori  %*% H + R
    K <- P_apriori %*% H %*% solve(S)
  # Output
    x <- x_apriori + K %*% y
    P <- (diag(1,nrow = 3) - K%*%H) %*% P_apriori
    return (list("x"=x, "P"=P))
}