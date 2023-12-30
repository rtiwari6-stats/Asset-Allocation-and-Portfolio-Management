#project 2 CFRM 503

library(quantmod)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(factoextra)
library(stargazer)

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

#Subtract riskfree rate
ff3_ranged = ff3[ff3$Date >= startdate,]
ff3_ranged = ff3_ranged[ff3_ranged$Date < enddate,]
dat_spdr[,21:ncol(dat_spdr)] = dat_spdr[,21:ncol(dat_spdr)] - ff3_ranged$RF
dat_djia[,36:ncol(dat_djia)] = dat_djia[,36:ncol(dat_djia)] - ff3_ranged$RF

#part 2
pc_sel = c("XLFDailyReturn","XLBDailyReturn","XLIDailyReturn", "XLYDailyReturn","XLEDailyReturn", "XLKDailyReturn",
           "XLVDailyReturn","XLPDailyReturn","XLUDailyReturn")
dat_spdr_pc = dat_spdr[,pc_sel]
dat_spdr.pca = prcomp(dat_spdr_pc, scale = TRUE)
fviz_eig(dat_spdr.pca)
fviz_pca_var(dat_spdr.pca, col.var = "contrib")
d1 = coredata(dat_spdr_pc) %*% (dat_spdr.pca$rotation[,1])
d2 = coredata(dat_spdr_pc) %*% (dat_spdr.pca$rotation[,2])
d = cbind(d1,d2)
dat_spdr.pca_xts = as.xts(d, order.by = index(dat_spdr_pc))
colnames(dat_spdr.pca_xts) = c("pc1DailyReturn","pc2DailyReturn")
dat_spdr_pc = dat_spdr[,21:ncol(dat_spdr)]

#part 3
dat_djia_pc = na.omit(dat_djia[,36:ncol(dat_djia)])
dat_djia.pca = prcomp(dat_djia_pc, scale = TRUE)
fviz_eig(dat_djia.pca)
fviz_pca_var(dat_djia.pca, col.var = "contrib")
d1 = coredata(dat_djia_pc) %*% (dat_djia.pca$rotation[,1])
d2 = coredata(dat_djia_pc) %*% (dat_djia.pca$rotation[,2])
d = cbind(d1,d2)
dat_djia.pca_xts = as.xts(d, order.by = index(dat_djia_pc))
colnames(dat_djia.pca_xts) = c("pc1DailyReturn","pc2DailyReturn")

#part 4 covariances

cov_printer = function(matrix, file_name){
  iter = 1
  for(i in seq(1, dim(matrix)[2], by=5)){
    j = i+4
    m = matrix[, i:j]
    nm = paste(file_name,iter, ".txt", sep = "")
    sink(nm)
    print(m)
    sink()
    iter = iter+1
  }
}

cov_spdr = cov(dat_spdr_pc)
cov_djia = cov(dat_djia_pc)
cov_spdr = round(cov_spdr, digits = 5)
cov_djia = round(cov_djia, digits = 5)
cov_printer(cov_spdr,"cov_spdr")
cov_printer(cov_djia,"cov_djia")
#XLC and XLRE covariance with the rest
cov_xlc_xlre = cov(na.omit(dat_spdr_pc))
cov_xlc_xlre = cov_xlc_xlre[c("XLCDailyReturn", "XLREDailyReturn"),]
cov_printer(cov_xlc_xlre,"cov_xlc_xlre")

#part 5
reg = function(y,x){
  res_list = list()
  for(i in colnames(y)){
    z = merge(y[,i],x, join = 'inner')
    lm_r = lm(as.data.frame(z))
    res_list[[i]] = lm_r
    
  }
  res_list
}


printer = function(data_list, name, col_names){
  setwd("~/UW/CFRM-503/Project2/outputs/")
    iter = 1
    for(i in seq(1, length(data_list), by=5)){
    j = i+4
    models = data_list[i:j]
    nm = paste(name,iter, ".html", sep = "")
    cm = col_names[i:j]
    stargazer(models, type = "html", out = nm, column.labels = cm, title = paste("Table: ", toString(table)), mean.sd = TRUE)
    iter = iter+1
    table <<- table+1
  }
}


#CAPM: the Russell 3000 index
dat_spdr_rua_res_list = reg(dat_spdr_pc, dat_rua$RUADailyReturn-ff3_ranged$RF)
printer(dat_spdr_rua_res_list,"PART_A_dat_spdr_rua_res_list", colnames(dat_spdr_pc))
dat_djia_rua_res_list = reg(dat_djia_pc, dat_rua$RUADailyReturn-ff3_ranged$RF)
printer(dat_djia_rua_res_list,"PART_A_dat_djia_rua_res_list", colnames(dat_djia_pc))

#Sectors: the Sector ETFs
dat_djia_spdr_res_list = reg(dat_djia_pc, dat_spdr_pc)
printer(dat_djia_spdr_res_list,"PART_B_dat_djia_spdr_res_list", colnames(dat_djia_pc))

#Sector PCs
index(dat_spdr.pca_xts) = as.Date(index(dat_spdr.pca_xts))
#colnames(dat_spdr.pca_xts) = paste(colnames(dat_spdr.pca_xts), "_pc",sep="")
dat_spdr_spdr_pc_res_list = reg(dat_spdr_pc,dat_spdr.pca_xts)
printer(dat_spdr_spdr_pc_res_list,"PART_C_dat_spdr_spdr_pc_res_list", colnames(dat_spdr_pc))
dat_djia_spdr_pc_res_list = reg(dat_djia_pc, dat_spdr.pca_xts)
printer(dat_djia_spdr_pc_res_list,"PART_C_dat_djia_spdr_pc_res_list", colnames(dat_djia_pc))

#DJIA PCs
index(dat_djia.pca_xts) = as.Date(index(dat_djia.pca_xts))
#colnames(dat_djia.pca_xts) = paste(colnames(dat_djia.pca_xts), "_pc",sep="")
dat_spdr_djia_pc_res_list = reg(dat_spdr_pc, dat_djia.pca_xts)
printer(dat_spdr_djia_pc_res_list,"PART_D_dat_spdr_djia_pc_res_list",colnames(dat_spdr_pc))
dat_djia_djia_pc_res_list = reg(dat_djia_pc, dat_djia.pca_xts)
printer(dat_djia_djia_pc_res_list,"PART_D_dat_djia_djia_pc_res_list",colnames(dat_djia_pc))


#Fama-French 3-Factor Model Variables
ff3_xts = xts(ff3[,2:4], order.by = ff3$Date)
spdr_ff3_res_list = reg(dat_spdr_pc, ff3_xts)
printer(spdr_ff3_res_list,"PART_E_spdr_ff3_res_list", colnames(dat_spdr_pc))
djia_ff3_res_list = reg(dat_djia_pc, ff3_xts)
printer(djia_ff3_res_list,"PART_E_djia_ff3_res_list", colnames(dat_djia_pc))


#Fama-French 5-Factor Model Variables
ff5_xts = xts(ff5[,2:6], order.by = ff5$Date)
spdr_ff5_res_list = reg(dat_spdr_pc, ff5_xts)
printer(spdr_ff5_res_list,"PART_F_spdr_ff5_res_list", colnames(dat_spdr_pc))
djia_ff5_res_list= reg(dat_djia_pc, ff5_xts)
printer(djia_ff5_res_list,"PART_F_djia_ff5_res_list", colnames(dat_djia_pc))


