setwd("~/UW/CFRM-503/Project 1")

install.packages("readxl")
library("readxl")
library(quantmod)
library(tidyverse)
library("quadprog")


setwd("~/UW/CFRM-503/Project 1")

REIT = read_excel("REIT_Data.xlsx", sheet = "Constituents")

REIT_current = subset(REIT, startsWith(Name, 'D') | startsWith(Name, 'E') | startsWith(Name, 'F'))

REIT_future = subset(REIT, startsWith(Name, 'F') | startsWith(Name, 'G') | startsWith(Name, 'H'))

wc = c(REIT_current$`Market Capitalization`/sum(REIT_current$`Market Capitalization`), rep(0,19))

wf = c(rep(0,20),REIT_future$`Market Capitalization`/sum(REIT_future$`Market Capitalization`))

#read forecast values
forecasts = read.csv("REIT_Forecast.csv", header = T)
means = forecasts$Mean
sd = forecasts$Stdev

#covariance and correlation
cor_full = forecasts[, 5:48]
cor_matrix_full=data.matrix(cor_full)
cov_matrix_full = (sd)%*%t(sd)*cor_matrix_full

cov_wc = rep(0, dim(cov_matrix_full)[1])

#covariance of single assets with benchmark
i = 1
while (i <= length(cov_wc)){
  cov_wc[i] = (wc %*% cov_matrix_full[i,])
  i = i+1
}

cov_wf = rep(0, dim(cov_matrix_full)[1])
i = 1
while (i <= length(cov_wf)){
  cov_wf[i] = (wf %*% cov_matrix_full[i,])
  i = i+1
}

#build the constraints
Amat <- matrix(1, ncol = dim(cor_matrix_full)[1], nrow = 1)
Amat <- rbind(Amat, t(means))
Amat <- rbind(Amat,diag(1, ncol = dim(cor_matrix_full)[1], nrow = dim(cor_matrix_full)[1]))

#build the right hand side values
RHS <- matrix(0, nrow = dim(cor_matrix_full)[1]+2, ncol = 1)
RHS[1] <- 1
RHS[2] <- 0.002 + t(means)%*%wc

#call in loop while adjusting weights on current and future benchmarks.
load = c(1,0.8,0.6,0.3,0.0)
i = 0

solutionlist = list()
te_list_c = list()
te_list_f = list()
er_list_c = list()
er_list_f = list()
var_list = list()

index = 1
for (i in load){
  print 
   if(i == 0.0){ #when the importance of current benchmark drops to zero
     RHS[2] <- 0.002 + t(means)%*%wf
   
   }
   solution <- solve.QP(cov_matrix_full,
                       i*cov_wc+(1-i)*cov_wf,
                       t(Amat), RHS, meq = 1,
                       factorized = FALSE)
  #optimal weights
  solutionlist[[index]] <- solution$solution
  
  j = 1
  while(j <= length(solutionlist[[index]])){
    if(solutionlist[[index]][j] < 1.0e-8){
      solutionlist[[index]][j] = 0
    }
    j = j+1
  }
  
  #tracking error relative to current and future benchmark weights
  te_list_c[[index]] = sqrt(t(solution$solution-wc)%*%cov_matrix_full%*%(solution$solution-wc))
  te_list_f[[index]] = sqrt(t(solution$solution-wf)%*%cov_matrix_full%*%(solution$solution-wf))
  
  #expected excess return relative to current and future benchmark
  er_list_c[[index]] = t(means)%*% (solution$solution-wc)
  er_list_f[[index]] = t(means)%*% (solution$solution-wf)
  
  #portfolio variance
  var_list[[index]] = t(solution$solution) %*% cov_matrix_full %*% (solution$solution)
  index = index+1
  
}
df <- data.frame(matrix(unlist(solutionlist), nrow=length(solutionlist), byrow=TRUE))
df$tracking_error_current = unlist(te_list_c)
df$tracking_error_future = unlist(te_list_f)
df$er_current = unlist(er_list_c)
df$er_future = unlist(er_list_f)
df$portfolio_var = unlist(var_list)
colnames(df)[1:44] = REIT$Symbol

#check means/sd patterns by combining into a matrix
df_matrix = as.matrix(df)
df_matrix = rbind(df_matrix, c(means, rep(0,4)))
df_matrix = rbind(df_matrix, c(sd, rep(0,4)))
df_matrix

#benchmark variances
t(wc) %*% cov_matrix_full %*% (wc)
t(wf) %*% cov_matrix_full %*% (wf)