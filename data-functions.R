##########################################################
# Claims Triangles
##########################################################
# Incremental Triangle

# Mack 2019: Example 2
yr1  <- c(357848, 1124788, 1735330, 2218270, 2745596, 3319994, 3466336, 3606286, 3833515, 3901463)
yr2  <- c(352118, 1236139, 2170033, 3353322, 3799067, 4120063, 4647867, 4914039, 5339085, NA)
yr3  <- c(290507, 1292306, 2218525, 3235179, 3985995, 4132918, 4628910, 4909315, NA, NA)
yr4  <- c(310608, 1418858, 2195047, 3757447, 4029929, 4381982, 4588268, NA, NA, NA)
yr5  <- c(443160, 1136350, 2128333, 2897821, 3402672, 3873311, NA, NA, NA, NA)
yr6  <- c(396132, 1333217, 2180715, 2985752, 3691712, NA, NA, NA, NA, NA)
yr7  <- c(440832, 1288463, 2419861, 3483130, NA, NA, NA, NA, NA, NA)
yr8  <- c(359480, 1421128, 2864498, NA, NA, NA, NA, NA, NA, NA)
yr9  <- c(376686, 1363294, NA, NA, NA, NA, NA, NA, NA, NA)
yr10 <- c(344014, NA, NA, NA, NA, NA, NA, NA, NA, NA)

cum.claims_MM1 <- matrix(nrow=10, ncol=10,
                     dimnames = list(c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10"),
                                     c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10")))
cum.claims_MM1[,] <- rbind(yr1, yr2, yr3, yr4, yr5, yr6, yr7, yr8, yr9, yr10)

# Mack 2019: Example 2
yr1 <- c(58046, 127970,  476599, 1027692, 1360489, 1647310, 1819179, 1906852, 1950105)
yr2 <- c(24492, 141767,  984288, 2142656, 2961978, 3683940, 4048898, 4115760, NA)
yr3 <- c(32848, 274682, 1522637, 3203427, 4445927, 5158781, 5342585, NA, NA)
yr4 <- c(21439, 529828, 2900301, 4999019, 6460112, 6853904, NA, NA, NA)
yr5 <- c(40397, 763394, 2920745, 4989572, 5648563, NA, NA, NA, NA)
yr6 <- c(90748, 951994, 4210640, 5866482, NA, NA, NA, NA, NA)
yr7 <- c(62096, 868480, 1954797, NA, NA, NA, NA, NA, NA)
yr8 <- c(24983, 284441, NA, NA, NA, NA, NA, NA, NA)
yr9 <- c(13121, NA, NA, NA, NA, NA, NA, NA, NA)

cum.claims_MM2 <- matrix(nrow=9, ncol=9,
                     dimnames = list(c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"),
                                     c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9")))
cum.claims_MM2[,] <- rbind(yr1, yr2, yr3, yr4, yr5, yr6, yr7, yr8, yr9)


# Lavender 2021
yr1  <- c(135338126,  90806681, 68666715, 55736215, 46967279, 35463367, 30477244, 24838121, 18238489, 14695083)
yr2  <- c(125222434,  89639978, 70697962, 58649114, 46314227, 41369299, 34394512, 26554172, 24602209, NA)
yr3  <- c(136001521,  91672958, 78246269, 62305193, 49115673, 36631598, 30210729, 29882359, NA, NA)
yr4  <- c(135277744, 103604885, 78303084, 61812683, 48720135, 39271861, 32029697, NA, NA, NA)
yr5  <- c(143540778, 109316613, 79092473, 65603900, 51226270, 44408236, NA, NA, NA, NA)
yr6  <- c(132095863,  88862933, 69269383, 57109637, 48818781, NA, NA, NA, NA, NA)
yr7  <- c(127299710,  92979311, 61379607, 50317305, NA, NA, NA, NA, NA, NA)
yr8  <- c(120660241,  89469673, 71570718, NA, NA, NA, NA, NA, NA, NA)
yr9  <- c(134132283,  87016365, NA, NA, NA, NA, NA, NA, NA, NA)
yr10 <- c(131918566, NA, NA, NA, NA, NA, NA, NA, NA, NA)

inc.claims <- matrix(nrow=10, ncol=10,
                   dimnames = list(c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10"),
                                   c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10")))
inc.claims[,] <- rbind(yr1, yr2, yr3, yr4, yr5, yr6, yr7, yr8, yr9, yr10)

# Cumulative Triangle
cum.claims <- t(apply(inc.claims, 1, cumsum))

##########################################################
# Misc Functions
##########################################################
get.diagonals <- function(triangle, Is, Js) {
    
    output <- c()
    
    for (iter in 1:length(Is)) {
        output <- c(output, triangle[Is[iter], Js[iter]])
    }
    
    return (output)
}

appendix_B.67 <- function(triangle, sigma2, d.facts, i, I=10) {
    
    d.facts2 <- d.facts^2
    
    temp.outer <- 0
    for (s in (I-i+1):(I-1)) {
        
        temp.inner <- 0
        for (p in 1:(I-s)) {
            temp.inner <- temp.inner + triangle[p, s]
        }
        
        temp.outer <- temp.outer + sigma2[s]/d.facts2[s]/temp.inner
    }
    
    return (prod(d.facts[(I-i+1):(I-1)])^2 * temp.outer)
}

##########################################################
# Chain Ladder
##########################################################

# Chain Ladder / Mack Model for Reserves
dev.factors <- function(triangle, I=10) {
    
    d.facts <- c()
    for (j in 1:(I-1)) {
        d.fact <- sum(triangle[1:(I-j), j+1])/sum(triangle[1:(I-j), j])
        d.facts <- c(d.facts, d.fact)
    }
    return(d.facts)
}

ultimate.claims <- function(triangle, I=10) {
    
    u.claims <- c(triangle[1, I])
    d.facts <- dev.factors(triangle, I)
    for (i in 2:I) {
        u.claim <- triangle[i, I-i+1] * prod(d.facts[(I-i+1):(I-1)])
        u.claims <- c(u.claims, u.claim)
    }
    return(u.claims)
}

claims.projections <- function(triangle, I=10) {
    
    d.facts <- dev.factors(triangle, I)
    C.hat_ik <- triangle
    
    for (i in 2:I) {
        for (j in (I-i+2):I) {
            C.hat_ik[i, j] <- C.hat_ik[i, I-i+1]*prod(d.facts[(I-i+1):(j-1)])
        }
    }
    return (C.hat_ik)
}

CL.accident.reserves <- function(triangle, I=10) {
    
    acc.reserves <- c()
    ult.claims <- get.diagonals(triangle, 1:I, I-(1:I)+1)
    ult.claims.hat <- ultimate.claims(triangle, I)
    
    return(ult.claims.hat - ult.claims)
}


##########################################################
# Mack Model
##########################################################

MM.dy.sigma2 <- function(triangle, I=10) {
    # From Mack 1993 Sec 3
    
    dy.sigma2 <- c()
    d.facts <- dev.factors(triangle, I)
    
    
    for (k in 1:(I-2)) {
        dyk.sigma2 <- 0
        for (i in 1:(I-k)) {
            dyk.sigma2 <- dyk.sigma2 + triangle[i, k]*(triangle[i, k+1]/triangle[i, k] - d.facts[k])^2
        }
        dy.sigma2 <- c(dy.sigma2, dyk.sigma2/(I-k-1))
    }
    
    # sigma_I-1
    dy.sigma2 <- c(dy.sigma2, 
                   min((dy.sigma2[I-2]^2)/dy.sigma2[I-3], 
                       dy.sigma2[I-3], 
                       dy.sigma2[I-2]))
    
    return (dy.sigma2)
}

MM.mse.reserve <- function(triangle, i, I=10) {
    # Formulas from Mack 1993 pg 218 (proof of theorem 3)
    
    stopifnot(i >= 2)
    
    C_i <- triangle[i, I-i+1]
    dy.sigma2 <- MM.dy.sigma2(triangle, I)
    d.facts <- dev.factors(triangle, I)
    d.facts2 <- d.facts^2
    
    first <- dy.sigma2[I-i+1]*prod(d.facts2[(I-i+2):(I-1)])
    last <- prod(d.facts[(I-i+1):(I-2)])*dy.sigma2[I-1]
    
    if (i==2) {
        part1 <- C_i*dy.sigma2[I-i+1]
    } else if (i==3) {
        part1 <- C_i*(first + last)
    } else {
        temp <- first + last
        for (j in (I-i+2):(I-2)) {
            lhs <- prod(d.facts[(I-i+1):(j-1)])
            rhs <- prod(d.facts2[(j+1):(I-1)])
            temp <- temp + lhs*dy.sigma2[j]*rhs
        }
        part1 <- C_i*temp
    }
    
    part2 <- C_i^2 * appendix_B.67(triangle, dy.sigma2, d.facts, i, I)
    
    return (part1 + part2)
}

MM.mse.reserve2 <- function(triangle, i, I=10) {
    # This should return the same value as MM.mse.reserve()
    # This was created with a simplified formula of the one used above.
    
    stopifnot(i >= 2)
    
    C_i <- triangle[i, I-i+1]
    dy.sigma2 <- MM.dy.sigma2(triangle, I)
    d.facts <- dev.factors(triangle, I)
    d.facts2 <- d.facts^2
    C.hat_ik <- claims.projections(triangle)
    
    temp <- 0
    for (k in (I-i+1):(I-1)) {
        temp <- temp + dy.sigma2[k]/d.facts2[k]/C.hat_ik[i, k]
    }
    part1 <- temp*C.hat_ik[i, I]^2
    
    part2 <- C_i^2 * appendix_B.67(triangle, dy.sigma2, d.facts, i, I)
    
    return (part1 + part2)
}

MM.mse.total_reserves <- function(triangle, I=10) {
    
    dy.sigma2 <- MM.dy.sigma2(triangle, I)
    d.facts <- dev.factors(triangle, I)
    d.facts2 <- d.facts^2
    u.claims <- ultimate.claims(triangle, I)
    C.hat_ik <- claims.projections(triangle, I)
    
    mse.T_R <- 0
    for (i in 2:I) {
        mse.T_R <- mse.T_R + MM.mse.reserve(triangle, i, I)
        
        temp0 <- u.claims[i]
        temp1 <- 1
        temp2 <- 0
        
        if (i < I) {
            temp1 <- 0
            for (j in (i+1)) {
                temp1 <- temp1 + u.claims[j]
            }
        }
        
        for (k in (I-i+1):(I-1)) {
            temp2.1 <- 0
            for (n in 1:(I-k)) {
                temp2.1 <-  temp2.1 + C.hat_ik[n, k]
            }
            
            temp2 <- temp2 + 2*dy.sigma2[k]/d.facts2[k]/temp2.1
        }
        
        mse.T_R <- mse.T_R + temp0*temp1*temp2
    }
    
    return (mse.T_R)
}
