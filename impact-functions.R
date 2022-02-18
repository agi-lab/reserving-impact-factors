source("data-functions.R")
library(plot3D)
library(xtable)

##########################################################
# Central Estimates
##########################################################
####################################################################################################################
### 3.1.1 Chain Ladder

impact.function.3.1.1 <- function(triangle, i, I=10) {
    
    IF.R <- triangle
    Ri <- CL.accident.reserves(triangle, I)[i]
    ult.claims.hat <- ultimate.claims(triangle, I)
    
    for (k in 1:I) {
        for (j in 1:(I-k+1)) {
            if (k > i) { IF.R[k, j] <- 0}
            else if (k == i) { IF.R[k, j] <- Ri/triangle[i, I-i+1]}
            else {
                temp <- 0
                for (p in k:(i-1)) {
                    temp1 <- 1/sum(triangle[1:p, I-p+1])
                    temp2 <- 1/sum(triangle[1:p, I-p])
                    
                    temp = temp + temp1*(j<=I-p+1) - temp2*(j<=I-p)
                }
            
                IF.R[k, j] <- ult.claims.hat[i]*temp
            }
        }
    }
    
    return (IF.R)
}

IF.3.1.1 <- impact.function.3.1.1(cum.claims, 8)                                # Equation 3.1 / Table 1 / Figure 2
# hist3D(z = IF.3.1.1, theta=-45, phi = 10)
####################################################################################################################
### 3.1.2 Bornhuetter Ferguson

impact.function.3.1.2 <- function(triangle, i, mu_i, I=10) {
    
    IF.R <- triangle
    d.facts <- dev.factors(triangle, I)
    
    for (k in 1:I) {
        for (j in 1:(I-k+1)) {
            if (k >= i) {IF.R[k, j] <- 0}
            else {
                temp = 0
                for (p in k:(i-1)) {
                    temp1 <- 1/sum(triangle[1:p, I-p+1])
                    temp2 <- 1/sum(triangle[1:p, I-p])
                    
                    temp = temp + temp1*(j<=I-p+1) - temp2*(j<=I-p)
                }
                
                d.factor <- d.facts[(I-i+1):(I-1)]
                if (i==1) {d.factor <- 1}
                
                IF.R[k, j] <- mu_i*temp/prod(d.factor)
            }
        }
    }
    
    return (IF.R)
}

IF.3.1.2 <- impact.function.3.1.2(cum.claims, 8, ultimate.claims(cum.claims)[8])# Equation 3.5/ Figure 3
# hist3D(z = IF.3.1.2, theta=-45, phi = 10)

##########################################################
# Total Reserves
##########################################################
####################################################################################################################
### 3.2.1 IF for Total Reserves Mack Model

impact.function.3.2.1 <- function(triangle, I=10) {
    IF.3.2.1 <- impact.function.3.1.1(triangle, 1, I)
    for (i in 2:10) {
        IF.3.2.1 <- IF.3.2.1 + impact.function.3.1.1(triangle, i, I)
    }  
    return (IF.3.2.1)
}                                                                               # Equation 3.6 / Figure 4

IF.3.2.1 <- impact.function.3.2.1(cum.claims)
# hist3D(z = IF.3.2.1, theta=-45, phi = 10)

####################################################################################################################
# 3.2.1 Marginal Contributions of incremental claims

impact.function.3.2.1.marg <- function(triangle, inc.claims, I=10) {
    IF.3.2.1.marginal <- impact.function.3.2.1(triangle, I)
    for (k in 1:10) {
        for (j in 1:(10-k+1)) {
            IF.3.2.1.marginal[k, j] <- IF.3.2.1.marginal[k, j]*inc.claims[k, j]
        }
    }
    return (IF.3.2.1.marginal)
}                                                                               # Equation 3.7 / Figure 5

IF.3.2.1.marginal <- impact.function.3.2.1.marg(cum.claims, inc.claims, 10)
hist3D(z = IF.3.2.1.marginal, theta=-45, phi = 10)

### 3.2.2 IF for Total Reserves BF Model

impact.function.3.2.2 <- function(triangle, I=10) {
    prior.ultimate <- CL.accident.reserves(triangle)
    IF.3.2.2 <- impact.function.3.1.2(triangle, 1, prior.ultimate[1])
    for (i in 2:10) {
        IF.3.2.2 <- IF.3.2.2 + impact.function.3.1.2(triangle, i, prior.ultimate[i])
    }
    return (IF.3.2.2)
}                                                                               # Figure 6

IF.3.2.2 <- impact.function.3.2.2(cum.claims, 10)
hist3D(z = IF.3.2.2, theta=-45, phi = 10)

##########################################################
# Mean Squared Error
##########################################################
####################################################################################################################
### 4.1 IF of MSE Individual Accident Year Reserves
mseIF.4.1 <- function(triangle, i, I=10) {
    
    stopifnot(i>1) # Cannot calculate IF of reserve for AY1 or below
    mseIF.R <- triangle
    C_i <- triangle[i, I-i+1]
    u.claims <- ultimate.claims(triangle, I)
    
    d.facts <- dev.factors(triangle, I)
    d.facts2 <- d.facts^2
    dy.sigma2 <- MM.dy.sigma2(triangle, I)
    
    B.67 <- appendix_B.67(triangle, dy.sigma2, d.facts, i, I)
    
    for (k in 1:I) {
        for (j in 1:(I-k+1)) {
            if (k > i) {mseIF.R[k, j] <- 0}
            else if (k == i) {
                
                if (i==2) {
                    part1 <- dy.sigma2[I-i+1]
                } else {
                    # When j = I-i+1, the lhs does not exist
                    part1 <- dy.sigma2[I-i+1]*prod(d.facts2[(I-i+2):(I-1)])
                    
                    for (p in (I-i+2):(I-2)) {
                        lhs <- prod(d.facts[(I-i+1):(p-1)])
                        rhs <- prod(d.facts2[(p+1):(I-1)])
                        part1 <- part1 + lhs*dy.sigma2[p]*rhs
                    }
                    
                    # When j = I-1, the rhs does not exist
                    part1 <- part1 + prod(d.facts[(I-i+1):(I-2)])*dy.sigma2[I-1]
                }

                part2 <- 2*C_i*B.67

                mseIF.R[k, j] <- part1 + part2
                
            } else if (k < i) {
                
                part1 <- -2*C_i*sqrt(B.67)*u.claims[i]
                
                part2 <- 0
                for (p in k:(i-1)) {
                    temp1 <- 1/sum(triangle[1:p, I-p+1])
                    temp2 <- 1/sum(triangle[1:p, I-p])
                    
                    part2 = part2 + temp1*(j<=I-p+1) - temp2*(j<=I-p)
                }
                
                mseIF.R[k, j] <-part1*part2
            }
        }
    }
    
    return (mseIF.R)
}                                                                               # Equation 4.1

rmseIF.4.1 <- function(triangle, i, I=10) {
    
    mseIF.R <- mseIF.4.1(triangle, i, I)
    mse.i <- MM.mse.reserve2(triangle, i, I)
    
    return (0.5*mseIF.R/sqrt(mse.i))
}                                                                               # Adjustment for RMSE, pg 10

IF.4.1 <- rmseIF.4.1(cum.claims, 8)                                             # Table 2 / Figure 7
hist3D(z = IF.4.1, theta=-45, phi = 10)

####################################################################################################################
mseIF.4.2 <- function(triangle, I=10) {
    
    mseIF.RT <- triangle
    C.hat_ik <- claims.projections(triangle, I)
    d.facts <- dev.factors(triangle, I)
    dy.sigma2 <- MM.dy.sigma2(triangle, I)
    
    for (k in 1:I) {
        for (j in 1:(I-k+1)) {
            mseIF.RT[k, j] <- mseIF.4.2.kj(triangle, k, j, I, C.hat_ik, d.facts, dy.sigma2)
        }
    }
    return (mseIF.RT)
}                                                                               # Equation 4.2 

mseIF.4.2.kj <- function(triangle, k, j, I, C.hat_ik, d.facts, dy.sigma2) {
    # Helper function for the above
    
    d.facts2 <- d.facts^2
    mseIF.RT.kj <- 0
    
    for (i in 2:I) {
        part1 <- mseIF.4.1(triangle, i, I)[k, j]
        
        part2 <- C.hat_ik[i, I]
        temp2a <- 1
        if (i < I) {
            temp2a <- 0
            for (q in (i+1):I) {
                temp2a <- temp2a + C.hat_ik[q, I]
            }
        }
        temp2b <- 0
        for (r in (I-i+1):(I-1)) {
            temp2b.numer <- 0
            temp2b.denom <- 0
            for (n in 1:(I-r)) {
                temp2b.numer <- temp2b.numer + C.hat_ik[n, r]*d.facts2[r]*(dlnC.dX(triangle, n, r, k, j) + 2*dlnf.dX(triangle, r, I, k, j))

                temp2b.denom <- temp2b.denom + C.hat_ik[n, r]*d.facts2[r]
            }
            temp2b.numer <- temp2b.numer*-2*dy.sigma2[r]
            temp2b.denom <- temp2b.denom^2
            
            temp2b <- temp2b.numer/temp2b.denom
        }
        part2 <- part2*temp2a*temp2b
        
        
        temp3a <- 0
        for (r in (I-i+1):(I-1)) {
            temp3a.numer <- 2*dy.sigma2[r]/d.facts2[r]
            temp3a.denom <- 0
            for (n in 1:(I-r)) {
                temp3a.denom <- temp3a.denom + triangle[n, r]
            }
            
            temp3a <- temp3a + temp3a.numer/temp3a.denom
        }
        temp3b <- C.hat_ik[i, I]
        temp3c <- 1
        if (i < I) {
            temp3c <- 0
            for (q in (i+1):I) {
                temp3c <- temp3c + impact.function.3.1.1(triangle, q, I)[k, j] + dC.dX(q, I-q+1, k, j)
            }
        }

        temp3d <- 1
        if (i < I) {
            temp3d <- 0
            for (q in (i+1):I) {
                temp3d <- temp3d + C.hat_ik[q, I]
            }
        }
        temp3e <- impact.function.3.1.1(triangle, i, I)[k, j] + dC.dX(i, I-i+1, k, j)
        
        part3 <- temp3a*(temp3b*temp3c + temp3d*temp3e)
        
        mseIF.RT.kj <- mseIF.RT.kj + part1 + part2 + part3
    }
    
    return (mseIF.RT.kj)
}

dC.dX <- function(n, r, k, j) {
    # Helper for 4.2
    # \frac{\partial C_{n, r}}/{\partial X_{k, j}}
    if ((n==k) & (j <= r)) {
        return (1)
    }
    return (0)
}

dlnC.dX <- function(triangle, n, r, k, j) {
    # Helper for 4.2
    # \frac{\partial lnC_{n, r}}/{\partial X_{k, j}}
    # Different formula to B.3                                                  
    if ((n==k) & (j <= r)) {
        return (1/triangle[k, j])
    }
    return (0)
}

dlnf.dX <- function(triangle, s, I, k, j) {             
    # Helper for 4.2
    # \frac{\partial f^\hat_r}/{\partial X_{k, j}}
    
    if (k > I-s) {return(0)} # From B.6

    temp1 <- 1/sum(triangle[1:(I-s), s+1])
    temp2 <- 1/sum(triangle[1:(I-s), s])
    
    return(temp1*(j<=s+1) - temp2*(j<=s))
}

rmseIF.4.2 <- function(triangle, I=10) {
    
    mseIF.TR <- mseIF.4.2(triangle, I)
    mse.TR <- MM.mse.total_reserves(triangle, I)
    
    return (0.5*mseIF.TR/sqrt(mse.TR))
}                                                                               # Figure 8

IF.4.2 <- rmseIF.4.2(cum.claims, 10) 
hist3D(z = IF.4.2, theta=-250, phi = 20)                                        
####################################################################################################################
ln.IF.5 <- function(triangle, q, I=10) {
    
    IF.TR <- impact.function.3.2.1(triangle, I)
    IF.mseTR <- mseIF.4.2(triangle)
    mseTR <- MM.mse.total_reserves(triangle, I)
    TR <- sum(CL.accident.reserves(triangle, I))
    Norm.inv <- qnorm(q)
    
    IF.Finv.q <- triangle
    
    for (k in 1:I) {
        for (j in 1:(I-k+1)) {
            part1a <- (2*IF.TR[k, j]*TR - IF.mseTR[k, j])/(2*(mseTR + TR^2))
            part1b <- Norm.inv*(IF.mseTR[k, j]*TR - 2*mseTR*IF.TR[k, j])/(2*TR*(mseTR + TR^2)*sqrt(log(1 + mseTR/TR^2)))
            
            part2a <- log(TR) 
            part2b <- - 0.5*log(1+ mseTR/TR^2)
            part2c <- Norm.inv*sqrt(log(1+ mseTR/TR^2))
            
            IF.Finv.q[k, j] <- (part1a + part1b)*exp(part2a + part2b + part2c)
        }
    }
    
    return (IF.Finv.q)
}                                                                               # Equation 5.3 / Table 3 / Figure 9

IF.5 <- ln.IF.5(cum.claims, 0.995, 10)
hist3D(z = IF.5, theta=-45, phi = 10)   

# write.csv(IF.3.1.1, "IF-3-1-1.csv")
# write.csv(IF.3.1.2, "IF-3-1-2.csv")
# write.csv(IF.3.2.1, "IF-3-2-1.csv")
# write.csv(IF.3.2.1.marginal, "IF-3-2-1_marginal.csv")
# write.csv(IF.3.2.2, "IF-3-2-2.csv")
# write.csv(IF.4.1, "IF-4-1.csv")
# write.csv(IF.4.2, "IF-4-2.csv")
# write.csv(IF.5, "IF-5.csv")

# xtable(IF.3.1.1, digits = 4)
# xtable(IF.4.1, digits = 4)
# xtable(IF.5, digits = 4)
