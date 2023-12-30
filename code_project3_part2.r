# code for project 3 part 2

setwd("~/UW/CFRM-503/project3")
data = read.csv("Project3_Data.csv", header = TRUE)
#central function
decompose_timeseries = function(p_weights,b_weights,p_returns,b_returns){
  #pick ith segment
  # selection = w_b_i * (r_p_i - r_b_i)
  # allocation = (w_p_i - w_b_i) * (r_b_i-r_b)
  #interaction = (w_p_i - w_b_i) * (r_p_i - r_b_i)
  mat = matrix(, nrow = 1, ncol = (length(p_weights)*4))
  p_total_return = t(p_weights) %*% p_returns
  b_total_return =  t(b_weights) %*% b_returns
  start = 0
  allocation_total = 0
  interaction_total = 0
  selection_total = 0
  total = 0
  for(i in 1:length(p_weights)){
    selection = b_weights[i] * (p_returns[i]-b_returns[i])
    allocation = (p_weights[i] - b_weights[i]) * (b_returns[i] - b_total_return[1,1])
    interaction = (p_weights[i] - b_weights[i]) * (p_returns[i] - b_returns[i])
    #mat = rbind(mat, c(allocation, selection, interaction))
    mat[1,start+1] = allocation
    mat[1,start+2] = selection
    mat[1,start+3] = interaction
    mat[1,start+4] = allocation+selection+interaction
    
    allocation_total = allocation_total + allocation
    interaction_total = interaction_total + interaction
    selection_total = selection_total + selection
    total = total + mat[1,start+4]
    start = start+4
  }
  res.df = as.data.frame(mat)
  res.df$allocation_total = allocation_total
  res.df$selection_total = selection_total
  res.df$interaction_total= interaction_total
  res.df$total = total
  
  names = c()
  #construct names vector
  for(i in 1: length(p_weights)){
    names = c(names, paste(names(p_weights)[i], "_allocation", sep=""))
    names = c(names, paste(names(p_weights)[i], "_selection", sep=""))
    names = c(names, paste(names(p_weights)[i], "_interaction", sep=""))
    names = c(names, paste(names(p_weights)[i], "_total", sep=""))
  }
  colnames(res.df) = c(names, "allocation_total","selection_total","interaction_total","total")
  
  res.df*100
}

#professor example
p_weights = c(0.6,0.1,0.3)
b_weights = c(0.55,0.05,0.4)
p_returns = c(-0.037, -0.035,-0.001)
b_returns = c(-0.019,-0.053,-0.009)
names(p_weights) = c("LC","SC","FI")
res.df_flattened = decompose_timeseries(p_weights, b_weights, p_returns, b_returns)

#step 1
res.mat.1 = matrix(, nrow = 0, ncol = 12)
for(i in 1:length(data$GlobEq_weight_p)){
  p_weights = c(data$GlobEq_weight_p[i], data$Bonds_weight_p[i])
  b_weights = c(data$GlobEq_weight_b[i], data$Bonds_weight_b[i])
  p_returns = c(data$GlobEq_return_p[i], data$Bonds_return_p[i])
  b_returns = c(data$GlobEq_return_b[i], data$Bonds_return_b[i])
  names(p_weights) = c("GlobalEquity","Bonds")
  res.df = decompose_timeseries(p_weights, b_weights, p_returns, b_returns)
  res.mat.1 = rbind(res.mat.1, as.matrix(res.df))
}
rownames(res.mat.1) = data$ï..Date
write.csv(res.mat.1, "step1.csv")

#step 2
res.mat.2 = matrix(, nrow = 0, ncol = 16)
for(i in 1:length(data$USEq_weight_p)){
  p_weights = c(data$USEq_weight_p[i], data$EEM_weight_p[i], data$EFA_weight_p[i])
  b_weights = c(data$USEq_weight_b[i], data$EEM_weight_b[i], data$EFA_weight_b[i])
  p_returns = c(data$USEq_return_p[i], data$EEM_return_p[i], data$EFA_return_p[i])
  b_returns = c(data$USEq_return_b[i], data$EEM_return_b[i], data$EFA_return_b[i])
  names(p_weights) = c("USEquity","Em.Mkt","Dev.I")
  res.df = decompose_timeseries(p_weights, b_weights, p_returns, b_returns)
  res.mat.2 = rbind(res.mat.2, as.matrix(res.df))
}
rownames(res.mat.2) = data$ï..Date
write.csv(res.mat.2, "step2.csv")

#step 3
res.mat.3 = matrix(, nrow = 0, ncol = 16)
for(i in 1:length(data$GOVT_weight_p)){
  p_weights = c(data$GOVT_weight_p[i], data$HYG_weight_p[i], data$LQD_weight_p[i])
  b_weights = c(data$GOVT_weight_b[i], data$HYG_weight_b[i], data$LQD_weight_b[i])
  p_returns = c(data$GOVT_return_p[i], data$HYG_return_p[i], data$LQD_return_p[i])
  b_returns = c(data$GOVT_return_b[i], data$HYG_return_b[i], data$LQD_return_b[i])
  names(p_weights) = c("Gov.bond","HY.Bond", "Inv.Bond")
  res.df = decompose_timeseries(p_weights, b_weights, p_returns, b_returns)
  res.mat.3 = rbind(res.mat.3, as.matrix(res.df))
}
rownames(res.mat.3) = data$ï..Date
write.csv(res.mat.3, "step3.csv")

#step 4
decompose_timeseries_bysize = function(p_weights,b_weights,p_returns,b_returns){
  #pick ith segment
  # selection = w_b_i * (r_p_i - r_b_i)
  # allocation = (w_p_i - w_b_i) * (r_b_i-r_b)
  #interaction = (w_p_i - w_b_i) * (r_p_i - r_b_i)
  mat = matrix(, nrow = 0, ncol = 3)
  p_total_return = t(p_weights) %*% p_returns
  b_total_return =  t(b_weights) %*% b_returns
  for(i in 1:length(p_weights)){
    selection = b_weights[i] * (p_returns[i]-b_returns[i])
    allocation = (p_weights[i] - b_weights[i]) * (b_returns[i] - b_total_return[1,1])
    interaction = (p_weights[i] - b_weights[i]) * (p_returns[i] - b_returns[i])
    mat = rbind(mat, c(allocation, selection, interaction))
  }
  res.df = as.data.frame(mat)
  res.df$Total = rowSums(res.df)
  res.df = rbind(res.df,colSums(res.df))
  colnames(res.df) = c("allocation", "selection", "interaction","Total")
  rownames(res.df) = c(names(p_weights), "Total")
  res.df*100
}


res.mat.4 = matrix(, nrow = 0, ncol = 12)
for(i in 1:length(data$XLF_weight_p)){
  p_weights = as.numeric(as.vector(data[i, which(colnames(data)=="XLF_weight_p"):which(colnames(data) == "PSCU_weight_p")]))
  b_weights = as.numeric(as.vector(data[i, which(colnames(data)=="XLF_weight_b"):which(colnames(data) == "PSCU_weight_b")]))
  p_returns = as.numeric(as.vector(data[i, which(colnames(data)=="XLF_return_p"):which(colnames(data) == "PSCU_return_p")]))
  b_returns = as.numeric(as.vector(data[i, which(colnames(data)=="XLF_return_b"):which(colnames(data) == "PSCU_return_b")]))
  names(p_weights) = c("XLF","PSCF","XLB","PSCM","XLI","PSCI","XLY","PSCD","XLK","PSCT","XLE","PSCE","XLV","PSCH","XLP","PSCC","XLU","PSCU")
  res.df = decompose_timeseries_bysize(p_weights, b_weights, p_returns, b_returns)
  res.df_4_base = res.df[-nrow(res.df),]
  res.df_4_base$size = c("L","S","L","S","L","S","L","S","L","S","L","S","L","S",
                         "L","S","L","S")
  size_agg = aggregate(cbind(res.df_4_base$allocation,res.df_4_base$selection,res.df_4_base$interaction, res.df_4_base$Total), 
                       by=list(Category=res.df_4_base$size), FUN=sum)
  sizes = size_agg$Category
  size_agg = size_agg[,2:4]
  size_agg$Total = rowSums(size_agg)
  size_agg = rbind(size_agg,colSums(size_agg))
  
  #manually create the 12-values
  v = c(size_agg[1,1], size_agg[1,2], size_agg[1,3], size_agg[2,1], size_agg[2,2], size_agg[2,3], 
        size_agg[1,4], size_agg[2,4], size_agg[3,1], size_agg[3,2], size_agg[3,3], size_agg[3,4])
  names(v) = c("L_allocation","L_selection","L_interation","S_allocation","S_selection","S_interation",
               "L_total", "S_total", "allocation_total", "selection_total", "interaction_total", "total")
  res.mat.4 = rbind(res.mat.4, v)
  
}
rownames(res.mat.4) = data$ï..Date
write.csv(res.mat.4, "step4.csv")

library(PerformanceAnalytics)
dates = seq(as.Date("2012/5/1"), as.Date("2021/5/1"), by = "month")
data_xts_p = as.xts(data$Portfolio_return_p, order.by = dates)
data_xts_b = as.xts(data$Portfolio_return_b, order.by = dates)
data_xts_rf = as.xts(data$TBill_return_b, order.by = dates)

#risk measures
x1 = CAPM.beta(data_xts_p, data_xts_b, data_xts_rf)
x2 = CAPM.alpha(data_xts_p, data_xts_b, data_xts_rf)

x3 = SharpeRatio(data_xts_p, data_xts_rf, FUN = "StdDev", annualize = TRUE)
x4 = TreynorRatio(data_xts_p, data_xts_b, data_xts_rf)

x5 = InformationRatio(data_xts_p, data_xts_b)

r = c(x1,x2,x3,x4,x5)
names(r) = c("beta","alpha","sharperatio","treynorratio","informationratio")
