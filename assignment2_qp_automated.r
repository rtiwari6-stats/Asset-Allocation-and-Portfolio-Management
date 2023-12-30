library(portfolio.optimization)

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

