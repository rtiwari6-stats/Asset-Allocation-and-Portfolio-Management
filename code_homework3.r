#code for homework 3

library(quantmod)

startdate = "2010-04-08"
enddate = "2021-03-31"
table = 1
#download spdr data
getSymbols(c("XLF","XLB","XLI", "XLY","XLC", "XLK","XLE","XLRE","XLV","XLP","XLU","PSCD","PSCE",
             "PSCM","PSCI","PSCT","PSCF","PSCH","PSCC","PSCU"), from=startdate, 
           to=enddate, src="yahoo")

dat_spdr <- na.locf(merge(XLF$XLF.Adjusted,
                          XLB$XLB.Adjusted,XLI$XLI.Adjusted,
                          XLY$XLY.Adjusted,XLC$XLC.Adjusted,
                          XLK$XLK.Adjusted, XLE$XLE.Adjusted,
                          XLRE$XLRE.Adjusted, XLV$XLV.Adjusted,
                          XLP$XLP.Adjusted, XLU$XLU.Adjusted,
                          PSCD$PSCD.Adjusted,PSCE$PSCE.Adjusted,
                          PSCM$PSCM.Adjusted,PSCI$PSCI.Adjusted,
                          PSCT$PSCT.Adjusted,PSCF$PSCF.Adjusted,
                          PSCH$PSCH.Adjusted,PSCC$PSCC.Adjusted,
                          PSCU$PSCU.Adjusted))[index(XLF)]

#download djia data
getSymbols(c("MMM","AXP","AMGN","AAPL","BA","CAT","CVX","CSCO","DD","GS","HON","IBM","INTC",
             "JNJ","JPM","MCD","MRK","MSFT","NKE","PG","CRM","KO","HD","TRV","DIS","UNH","VZ",
             "V","WBA","WMT","T","GE"), from=startdate, 
           to=enddate, src="yahoo")

getSymbols(c("XOM","PFE","RTX"),from=startdate, 
           to=enddate, src="yahoo")

dat_djia = na.locf(merge(MMM$MMM.Adjusted, AXP$AXP.Adjusted,AMGN$AMGN.Adjusted,AAPL$AAPL.Adjusted,
                         BA$BA.Adjusted,CAT$CAT.Adjusted,CVX$CVX.Adjusted,CSCO$CSCO.Adjusted,
                         DD$DD.Adjusted,GS$GS.Adjusted,HON$HON.Adjusted,IBM$IBM.Adjusted,
                         INTC$INTC.Adjusted,JNJ$JNJ.Adjusted,JPM$JPM.Adjusted,MCD$MCD.Adjusted,
                         MRK$MRK.Adjusted,MSFT$MSFT.Adjusted,NKE$NKE.Adjusted,PG$PG.Adjusted,
                         CRM$CRM.Adjusted,KO$KO.Adjusted,HD$HD.Adjusted,TRV$TRV.Adjusted,
                         DIS$DIS.Adjusted,UNH$UNH.Adjusted,VZ$VZ.Adjusted,V$V.Adjusted,
                         WBA$WBA.Adjusted,WMT$WMT.Adjusted,T$T.Adjusted,GE$GE.Adjusted,
                         XOM$XOM.Adjusted, PFE$PFE.Adjusted, RTX$RTX.Adjusted))[index(MMM)]

#download Russell 3000 data
getSymbols("^RUA",from=startdate, 
           to=enddate, src="yahoo")
dat_rua = na.locf(RUA)

#read fama-french 3
setwd("~/UW/CFRM-503/Project2/")
ff3 = read.csv("F-F_Research_Data_Factors_daily.csv")
ff3$Date = as.Date(as.character(ff3$X), format="%Y%m%d")
#convert into decimal
ff3$Mkt.RF = ff3$Mkt.RF/100
ff3$SMB = ff3$SMB/100
ff3$HML = ff3$HML/100
ff3$RF  = ff3$RF/100

#read fama-french 5
ff5 = read.csv("F-F_Research_Data_5_Factors_2x3_daily.csv")
ff5$Date = as.Date(as.character(ff5$X), format="%Y%m%d")
#convert into decimal
ff5$Mkt.RF = ff5$Mkt.RF/100
ff5$SMB = ff5$SMB/100
ff5$HML = ff5$HML/100
ff5$RMW = ff5$RMW/100
ff5$CMA = ff5$CMA/100
ff5$RF  = ff5$RF/100

#part 1
#calculate daily returns

#spdr
dat_spdr$XLFDailyReturn = dailyReturn(dat_spdr$XLF.Adjusted)
dat_spdr$XLBDailyReturn = dailyReturn(dat_spdr$XLB.Adjusted)
dat_spdr$XLIDailyReturn = dailyReturn(dat_spdr$XLI.Adjusted)
dat_spdr$XLYDailyReturn = dailyReturn(dat_spdr$XLY.Adjusted)
dat_spdr$XLCDailyReturn = dailyReturn(dat_spdr$XLC.Adjusted)
dat_spdr$XLKDailyReturn = dailyReturn(dat_spdr$XLK.Adjusted)
dat_spdr$XLEDailyReturn = dailyReturn(dat_spdr$XLE.Adjusted)
dat_spdr$XLREDailyReturn = dailyReturn(dat_spdr$XLRE.Adjusted)
dat_spdr$XLVDailyReturn = dailyReturn(dat_spdr$XLV.Adjusted)
dat_spdr$XLPDailyReturn = dailyReturn(dat_spdr$XLP.Adjusted)
dat_spdr$XLUDailyReturn = dailyReturn(dat_spdr$XLU.Adjusted)
dat_spdr$PSCDDailyReturn = dailyReturn(dat_spdr$PSCD.Adjusted)
dat_spdr$PSCEDailyReturn = dailyReturn(dat_spdr$PSCE.Adjusted)
dat_spdr$PSCMDailyReturn = dailyReturn(dat_spdr$PSCM.Adjusted)
dat_spdr$PSCIDailyReturn = dailyReturn(dat_spdr$PSCI.Adjusted)
dat_spdr$PSCTDailyReturn = dailyReturn(dat_spdr$PSCT.Adjusted)
dat_spdr$PSCFDailyReturn = dailyReturn(dat_spdr$PSCF.Adjusted)
dat_spdr$PSCHDailyReturn = dailyReturn(dat_spdr$PSCH.Adjusted)
dat_spdr$PSCCDailyReturn = dailyReturn(dat_spdr$PSCC.Adjusted)
dat_spdr$PSCUDailyReturn = dailyReturn(dat_spdr$PSCU.Adjusted)

#djia
dat_djia$MMMDailyReturn = dailyReturn(dat_djia$MMM.Adjusted)
dat_djia$AXPDailyReturn = dailyReturn(dat_djia$AXP.Adjusted)
dat_djia$AMGNDailyReturn = dailyReturn(dat_djia$AMGN.Adjusted)
dat_djia$AAPLDailyReturn = dailyReturn(dat_djia$AAPL.Adjusted)
dat_djia$BADailyReturn = dailyReturn(dat_djia$BA.Adjusted)
dat_djia$CATDailyReturn = dailyReturn(dat_djia$CAT.Adjusted)
dat_djia$CVXDailyReturn = dailyReturn(dat_djia$CVX.Adjusted)
dat_djia$CSCODailyReturn = dailyReturn(dat_djia$CSCO.Adjusted)
dat_djia$DDDailyReturn = dailyReturn(dat_djia$DD.Adjusted)
dat_djia$GSDailyReturn = dailyReturn(dat_djia$GS.Adjusted)
dat_djia$HONDailyReturn = dailyReturn(dat_djia$HON.Adjusted)
dat_djia$IBMDailyReturn = dailyReturn(dat_djia$IBM.Adjusted)
dat_djia$INTCDailyReturn = dailyReturn(dat_djia$INTC.Adjusted)
dat_djia$JNJDailyReturn = dailyReturn(dat_djia$JNJ.Adjusted)
dat_djia$JPMDailyReturn = dailyReturn(dat_djia$JPM.Adjusted)
dat_djia$MCDDailyReturn = dailyReturn(dat_djia$MCD.Adjusted)
dat_djia$MRKDailyReturn = dailyReturn(dat_djia$MRK.Adjusted)
dat_djia$MSFTDailyReturn = dailyReturn(dat_djia$MSFT.Adjusted)
dat_djia$NKEDailyReturn = dailyReturn(dat_djia$NKE.Adjusted)
dat_djia$PGDailyReturn = dailyReturn(dat_djia$PG.Adjusted)
dat_djia$CRMDailyReturn = dailyReturn(dat_djia$CRM.Adjusted)
dat_djia$KODailyReturn = dailyReturn(dat_djia$KO.Adjusted)
dat_djia$HDDailyReturn = dailyReturn(dat_djia$HD.Adjusted)
dat_djia$TRVDailyReturn = dailyReturn(dat_djia$TRV.Adjusted)
dat_djia$DISDailyReturn = dailyReturn(dat_djia$DIS.Adjusted)
dat_djia$UNHDailyReturn = dailyReturn(dat_djia$UNH.Adjusted)
dat_djia$VZDailyReturn = dailyReturn(dat_djia$VZ.Adjusted)
dat_djia$VDailyReturn = dailyReturn(dat_djia$V.Adjusted)
dat_djia$WBADailyReturn = dailyReturn(dat_djia$WBA.Adjusted)
dat_djia$WMTDailyReturn = dailyReturn(dat_djia$WMT.Adjusted)
dat_djia$TDailyReturn = dailyReturn(dat_djia$T.Adjusted)
dat_djia$GEDailyReturn = dailyReturn(dat_djia$GE.Adjusted)
dat_djia$XOMDailyReturn = dailyReturn(dat_djia$XOM.Adjusted)
dat_djia$PFEDailyReturn = dailyReturn(dat_djia$PFE.Adjusted)
dat_djia$RTXDailyReturn = dailyReturn(dat_djia$RTX.Adjusted)


#RUA
dat_rua$RUADailyReturn = dailyReturn(dat_rua$RUA.Adjusted)

#riskfree rate
ff3_ranged = ff3[ff3$Date >= startdate,]
ff3_ranged = ff3_ranged[ff3_ranged$Date < enddate,]
dat_spdr = dat_spdr[,21:ncol(dat_spdr)]
dat_djia = dat_djia[,36:ncol(dat_djia)]

#question 1
library(PerformanceAnalytics)

#spdr
res = matrix(, nrow = 0, ncol = 3)
rf = mean(ff3_ranged$RF)
for(i in 1:ncol(dat_spdr)){
  sharperatio = SharpeRatio(dat_spdr[,i], rf,  FUN = "StdDev", annualize = TRUE)
  TreynorRatio = TreynorRatio(dat_spdr[,i], dat_rua$RUADailyReturn,rf)
  M2ratio = sharperatio * StdDev(dat_rua$RUADailyReturn-ff3_ranged$RF) + rf
  res = rbind(res, c(sharperatio, TreynorRatio, M2ratio))
}
rownames(res) =  sub("DailyReturn", '', colnames(dat_spdr))
colnames(res) = c("SR", "TR", "M2")

#djia
res1 = matrix(, nrow = 0, ncol = 3)
rf = mean(ff3_ranged$RF)
for(i in 1:ncol(dat_djia)){
  sharperatio = SharpeRatio(dat_djia[,i], rf,  FUN = "StdDev", annualize = TRUE)
  TreynorRatio = TreynorRatio(dat_djia[,i], dat_rua$RUADailyReturn,rf)
  M2ratio = sharperatio * StdDev(dat_rua$RUADailyReturn-ff3_ranged$RF) + rf
  res1 = rbind(res1, c(sharperatio, TreynorRatio, M2ratio))
}
rownames(res1) =  sub("DailyReturn", '', colnames(dat_djia))
colnames(res1) = c("SR", "TR", "M2")

#get top three ratio
sort(res[,1], decreasing = TRUE)[1:3]
sort(res[,2], decreasing = TRUE)[1:3]
sort(res[,3], decreasing = TRUE)[1:3]
sort(res1[,1], decreasing = TRUE)[1:3]
sort(res1[,2], decreasing = TRUE)[1:3]
sort(res1[,3], decreasing = TRUE)[1:3]

#Question 2 part a
pc_sel = c("XLFDailyReturn","XLBDailyReturn","XLIDailyReturn", "XLYDailyReturn","XLKDailyReturn",
           "XLEDailyReturn", "XLVDailyReturn","XLPDailyReturn","XLUDailyReturn")
dat_spdr_pc = dat_spdr[,pc_sel]
cov_spdr = cov(dat_spdr_pc)

w_p1= rep(1/9,9)
mc_var1 = 2*w_p1 %*% cov_spdr
tc_var1 = mc_var1 * w_p1
p1 = rbind(mc_var1, tc_var1)
rownames(p1) = c("mar.contrib", "tot.contrib")
colnames(p1) = sub("DailyReturn", '', pc_sel)

w_p2 = c(0.05,0.1,0.1,0.15,0.20,0.15,0.10,0.1,0.05)
mc_var2 = 2*w_p2 %*% cov_spdr
tc_var2 = mc_var2 * w_p2
p2 = rbind(mc_var2, tc_var2)
rownames(p2) = c("mar.contrib", "tot.contrib")
colnames(p2) = sub("DailyReturn", '', pc_sel)

w_p3 = c(0.16,0.14,0.13,0.12,0.11,0.10,0.09,0.08,0.07)
mc_var3 = 2*w_p3 %*% cov_spdr
tc_var3 = mc_var3 * w_p3
p3 = rbind(mc_var3, tc_var3)
rownames(p3) = c("mar.contrib", "tot.contrib")
colnames(p3) = sub("DailyReturn", '', pc_sel)


library(IntroCompFinR)

p = globalMin.portfolio(colMeans(dat_spdr_pc), cov_spdr)
