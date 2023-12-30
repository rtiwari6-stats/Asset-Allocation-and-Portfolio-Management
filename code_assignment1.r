#code for assignment 1

library("IntroCompFinR")
library("zoo")
library("PerformanceAnalytics")
library("PortfolioAnalytics")

#Question 1

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

gm_portfolio = globalMin.portfolio(means, cov_matrix, shorts = T)
gm_portfolio$er
gm_portfolio$sd
gm_portfolio$weights


ef = efficient.frontier(means, cov_matrix, shorts = F, nport = 1000,alpha.max = 1.0, alpha.min = 0.0)
ep = efficient.portfolio(means,cov_matrix,shorts = F, target.return = 0.13)

result = cbind(ef$er, ef$sd, ef$weights)

#calculate risk tolerance
expr = sum(solve(cov_matrix)%*%means)
a = seq(from=1.0,to=0.0,length=1000)
result = cbind(-(a-1)/(expr),result )

result


