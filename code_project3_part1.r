#code for project 3 part 1

#central function
decompose = function(p_weights,b_weights,p_returns,b_returns){
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

#professor example
p_weights = c(0.6,0.1,0.3)
b_weights = c(0.55,0.05,0.4)
p_returns = c(-0.037, -0.035,-0.001)
b_returns = c(-0.019,-0.053,-0.009)

names(p_weights) = c("LC","SC","FI")
res.df = decompose(p_weights, b_weights, p_returns, b_returns)


#part 1
p_weights_1 = c(0.62,0.38)
b_weights_1 = c(0.6,0.4)
p_returns_1 = c(0.0228, 0.0019)
b_returns_1 = c(0.0220, 0.0030)
names(p_weights_1) = c("GlobalEquity","Bonds")
res.df_1 = decompose(p_weights_1, b_weights_1, p_returns_1, b_returns_1)

#part 2
p_weights_2 = c(0.42,0.33,0.25)
b_weights_2  = c(0.5,0.3,0.2)
p_returns_2 = c(0.0108,-0.0064,-0.0327)
b_returns_2 = c(0.0235,0.0348,0.0165)
names(p_weights_2) = c("USEquity","Dev.I","Em.Mkt")
res.df_2 = decompose(p_weights_2, b_weights_2, p_returns_2, b_returns_2)

#part 3
p_weights_3 = c(0.35,0.37,0.28)
b_weights_3 = c(0.4,0.35,0.25)
p_returns_3 = c(0.0023,0.0072,0.0038)
b_returns_3 = c(0.0019,0.0062,0.0003)
names(p_weights_3) = c("Gov.bond","Inv.Bond","HY.Bond")
res.df_3 = decompose(p_weights_3, b_weights_3, p_returns_3, b_returns_3)


#part 4
p_weights_4 =c(0.18,0.02,0.04,0.02,0.14,0.01,0.07,0.0,0.19,0.01,0.05,0.02,0.18,0.0,0.02,0.0,0.04,0.01)
b_weights_4=c(0.14,0.015,0.03,0.01,0.1,0.01,0.1,0.01,0.21,0.02,0.06,0.01,0.15,0.015,0.07,0.01,0.03,0.01)
p_returns_4 = c(0.059,0.0654,0.0457,0.0082,0.0218,-0.0436,0.0275,0.0945,0.0286,0.0738,0.0643,0.1376,-0.0419,-0.0387,0.0242,0.1236,-0.1743,0.0234)
b_returns_4 = c(0.0477,0.0132,0.0508,0.0174,0.0313,-0.0052,-0.0340,0.0287,-0.0093,-0.0044,0.0571,0.1667,0.0187,-0.0046,0.0177,0.0845,-0.0234,0.0164)
names(p_weights_4) = c("XLF","PSCF","XLB","PSCM","XLI","PSCI","XLY","PSCD","XLK","PSCT","XLE","PSCE","XLV","PSCH","XLP","PSCC","XLU","PSCU")
res.df_4=decompose(p_weights_4, b_weights_4, p_returns_4, b_returns_4)

res.df_4_base = res.df_4[-nrow(res.df_4),]
res.df_4_base$sector = c("Financials","Financials","Basic Materials","Basic Materials",
                    "Industrials","Industrials","Consumer Dis","Consumer Dis",
                    "Tech","Tech","Energy", "Energy", "Healthcare","Healthcare",
                    "Consumer Stp",
                    "Consumer Stp","Utilities","Utilities")
res.df_4_base$size = c("L","S","L","S","L","S","L","S","L","S","L","S","L","S",
                       "L","S","L","S")

#sector aggs
sector_agg = aggregate(cbind(res.df_4_base$allocation,res.df_4_base$selection,res.df_4_base$interaction, res.df_4_base$Total), 
                       by=list(Category=res.df_4_base$sector), FUN=sum)
sectors = sector_agg$Category
sector_agg = sector_agg[,2:4]
sector_agg$Total = rowSums(sector_agg)
sector_agg = rbind(sector_agg,colSums(sector_agg))
colnames(sector_agg) = c("allocation", "selection", "interaction","Total")
rownames(sector_agg) = c(sectors, "Total")

#size aggs
size_agg = aggregate(cbind(res.df_4_base$allocation,res.df_4_base$selection,res.df_4_base$interaction, res.df_4_base$Total), 
                       by=list(Category=res.df_4_base$size), FUN=sum)
sizes = size_agg$Category
size_agg = size_agg[,2:4]
size_agg$Total = rowSums(size_agg)
size_agg = rbind(size_agg,colSums(size_agg))
colnames(size_agg) = c("allocation", "selection", "interaction","Total")
rownames(size_agg) = c(sizes, "Total")

