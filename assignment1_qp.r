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

Amat <- matrix(1, ncol = 3, nrow = 1)
Amat <- rbind(Amat,diag(1, ncol = 3, nrow = 3))

# Build the right hand side values
RHS <- matrix(0, nrow = 4, ncol = 1)
RHS[1] <- 1

RTol <-seq(from=0,to=0.65,length.out = 1000)

OptWeights <- matrix(0, nrow = length(RTol), ncol = 3)
OptMeans <- matrix(0,
                   nrow = length(RTol),
                   ncol = 1)
OptStdevs <- matrix(0,
                    nrow = length(RTol),
                    ncol = 1)

for(rtol in 1:length(RTol)){
  solution <- solve.QP(cov_matrix,
                       RTol[rtol] * means,
                       t(Amat), RHS, meq = 1,
                       factorized = FALSE)
  solutionMat <- as.matrix(solution$solution)
  OptMeans[rtol] <- means %*%
    solutionMat
  OptStdevs[rtol] <- sqrt(t(solutionMat) %*%
                            cov_matrix %*%
                            solutionMat)
  OptWeights[rtol,] = solutionMat
  
}

result = cbind(RTol, OptMeans, OptStdevs, OptWeights)
result