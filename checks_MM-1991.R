source("data-functions.R")
# Example 1
# Pg 221 of Mack 1991 Table 1, 2, 3
I = 10
dev.factors(cum.claims_MM1, I)
MM.dy.sigma2(cum.claims_MM1, I)/1000
CL.accident.reserves(cum.claims_MM1, I)/1000
sum(CL.accident.reserves(cum.claims_MM1, I)/1000)

for (i in 2:I) {
    print(100*sqrt(MM.mse.reserve(cum.claims_MM1, i, I))/CL.accident.reserves(cum.claims_MM1, I)[i])
}

sqrt(MM.mse.total_reserves(cum.claims_MM1, I))/sum(CL.accident.reserves(cum.claims_MM1, I))

# Example 2
# Pg 223 of Mack 1991 Table 4, 5, 6
I = 9
dev.factors(cum.claims_MM2, I)
MM.dy.sigma2(cum.claims_MM2, I)/1000+
CL.accident.reserves(cum.claims_MM2, I)/1000
sum(CL.accident.reserves(cum.claims_MM2, I)/1000)

for (i in 2:I) {
    print(100*sqrt(MM.mse.reserve(cum.claims_MM2, i, I))/CL.accident.reserves(cum.claims_MM2, I)[i])
}

sqrt(MM.mse.total_reserves(cum.claims_MM2, I))/sum(CL.accident.reserves(cum.claims_MM2, I))
