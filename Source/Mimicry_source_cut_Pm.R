mimicry <- function(times, state, parms) {
  with(as.list(c(state, parms)), {
    Pc <- Pm^k
    dP_dt  <-  P * (((S * (x * n * Vn + C * Pc * Vc + (1 - sigma) * m * Pm * Vm)) / (1 + S * (x * n * Hn + C * Pc * Hc + m * Pm * (sigma * Hms + (1 - sigma) * Hme)))) - Up - f * P) # Predator
    dC_dt  <-  (rc * C * ((Kc - C - ALcm * m) / Kc)) - ((S * P * C * Pc) / (1 + S * (x * n * Hn + C * Pc * Hc + m * Pm * (sigma * Hms + (1 - sigma) * Hme)))) # Mimic
    dM_dt  <-  (rm * m * ((Km - m - ALmc * C) / Km)) - (((1 - sigma) * (S * P * m * Pm)) / (1 + S * (x * n * Hn + C * Pc * Hc + m * Pm * (sigma * Hms + (1 - sigma) * Hme)))) # Model
    dN_dt  <-  (D * (n0 - n)) - ((S * P * x * n) / (1 + S * (x * n * Hn + C * Pc * Hc + m * Pm * (sigma * Hms + (1 - sigma) * Hme)))) # Alternative prey
    dPm_dt <- 0
    dx_dt <- 0
    return(list(c(dP_dt, dC_dt, dM_dt, dN_dt, dPm_dt, dx_dt)))  # return the results  
  })
}

eventfun_mimicry <- function(times, state, parms){
  with(as.list(c(state, parms)),{
    N <- 10000
    Pm <- seq(0, 1, (1/N)) # Probability of attacking the mimic
    Pc <- Pm^k # Probability of attacking the model
    Va <- rep(NA, N + 1) # Space to store the optimal value
    probability0 <- cbind(Pc, Pm, Va) # Space to store the probability and value whit out alternative prey
    probability1 <- cbind(Pc, Pm, Va) # Space to store the probability and value with alternative prey
    probability0[,3] <- (S * (probability0[,1] * C * Vc + (1 - sigma) * m * Vm * probability0[,2])) / (1 + S * (C * Hc * probability0[,1] + probability0[,2] * m * (sigma * Hms + (1 - sigma) * Hme))) # Calculating the value of different probability with out alternative prey
    probability1[,3] <- (S * (1 * n *Vn + probability1[,1] * C * Vc + (1 - sigma) * m * Vm * probability1[,2])) / (1 + S * (1 * n * Hn + C * Hc * probability1[,1] + probability1[,2] * m * (sigma * Hms + (1 - sigma) * Hme))) # Calculating the value of different probability with
    a <- which(probability0[,3] == max(probability0[,3]), arr.ind = TRUE) # Finding the maximum value with out alternative prey
    b <- which(probability1[,3] == max(probability1[,3]), arr.ind = TRUE) # Finding the maximum value with alternative prey
    P  <-  P
    C <-  C
    m <-  m
    n <-  n
    Pm  <-  ifelse(probability0[a,3] > probability1[b,3], probability0[a,2], probability1[b,2]) # Determine 
    x  <-  ifelse(probability0[a,3] > probability1[b,3], 0L, 1L)
    Pm <- mean(Pm)
    x <- mean(x)
    return(as.numeric(c(P, C, m, n, Pm, x)))
  }) }
