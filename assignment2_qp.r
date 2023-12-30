library("quadprog")


means = c(0.13,0.1,0.08)
sd = c(0.2,0.18,0.12)

cor_matrix = matrix(c(1.0,0.6,0.5,0.6,1.0,0.3,0.5,0.3,1.0), 
                    nrow = 3, ncol = 3, byrow = T)

cov_matrix = matrix(, nrow = 3, ncol = 3)

for(i in 1:dim(cov_matrix)[1]) {
  for(j in 1:dim(cov_matrix)[2]) {
    cov_matrix[i,j] = cor_matrix[i,j]*sd[i]*sd[j]
  }
}

tolerance = 1.0e-5

#M-V calculation
Amat <- matrix(1, ncol = 3, nrow = 1)
Amat <- rbind(Amat, means)
Amat <- rbind(Amat,diag(1, ncol = 3, nrow = 3))

target_means = seq(from=0.08+tolerance, to=0.13-tolerance, length.out = 1000)

OptWeights <- matrix(0, nrow = length(target_means), ncol = 3)
OptMeans <- matrix(0,
                   nrow = length(target_means),
                   ncol = 1)
OptStdevs <- matrix(0,
                    nrow = length(target_means),
                    ncol = 1)

for(i in 1:length(target_means)){
  # Build the right hand side values
  RHS <- matrix(0, nrow = 5, ncol = 1)
  RHS[1] <- 1
  RHS[2] <- target_means[i]
  solution <- solve.QP(cov_matrix,
                       rep(0,3),
                       t(Amat), RHS, meq = 2,
                       factorized = FALSE)
  solutionMat <- as.matrix(solution$solution)
  OptMeans[i] <- target_means[i]
  OptStdevs[i] <- sqrt(t(solutionMat) %*%
                            cov_matrix %*%
                            solutionMat)
  OptWeights[i,] = solutionMat
  
}

result = cbind(OptMeans, OptStdevs, OptWeights)
result

#plot mean-variance
plot(result[,1]~result[,2], type="l", xlab = "stdev" , ylab = "mean", col="red")


#MAD Calculation using lpSolveAPI
library(lpSolveAPI)
target_means = seq(from=0.08+tolerance, to=0.13-tolerance, length.out = 1000)
MADWeights <- matrix(0, nrow = length(target_means), ncol = 3)
MADMeans <- matrix(0,
                   nrow = length(target_means),
                   ncol = 1)
MADStdevs <- matrix(0,
                    nrow = length(target_means),
                    ncol = 1)
for(i in 1:length(target_means)){
  lprec = make.lp(0,3)
  lp.control(lprec, sense="min")
  
  set.objfn(lprec, sd)
  add.constraint(lprec,c(1,1,1), "=",1)
  # add.constraint(lprec, c(1,0,0), ">=", 0)
  # add.constraint(lprec, c(0,1,0), ">=", 0)
  # add.constraint(lprec, c(0,0,1), ">=", 0)
  add.constraint(lprec, means, "=", target_means[i])
  
  solve(lprec)
  solutionMat <- as.matrix(get.variables(lprec))
  MADMeans[i] <- target_means[i]
  MADStdevs[i] <- sqrt(t(solutionMat) %*%
                     cov_matrix %*%
                     solutionMat)
  MADWeights[i,] = solutionMat
  
}

resultMAD = cbind(MADMeans, MADStdevs, MADWeights)
resultMAD
lines(resultMAD[,1]~resultMAD[,2], type="l", xlab = "stdev" , ylab = "mean", col="green")