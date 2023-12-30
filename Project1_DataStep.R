
#########
##
##  Project 1 
##
##
#########

# clear environment
rm(list = ls())

# load libraries
library(tidyverse)
library(quantmod)
library(readxl)
library(writexl)


# directories and files
workingDirectory <- "C:/Users/arize/Documents/CFRM/CFRM503/2021/Assignments/Project1"
inputFile <- "REIT_Data.xlsx"
sheetName <- "Constituents"
outputFile <- "REIT_Forecast.csv"

## read in data

constituents_df <- read_excel(file.path(workingDirectory,inputFile),
                              sheet = sheetName,
                              skip = 1)

constituents_df <- constituents_df %>% mutate(firstLetter = substring(Name,1,1))

# filter to securities with names beginning with A, B or C

SCAREY_REITs <- constituents_df %>% filter(firstLetter %in% LETTERS[4:8])
names(SCAREY_REITs) <- gsub(" ","",names(SCAREY_REITs))
GroupA <- LETTERS[4:6]
GroupB <- LETTERS[6:8]
MktCapGroupA <- sum(SCAREY_REITs$MarketCapitalization[SCAREY_REITs$firstLetter %in% GroupA])
MktCapGroupB <- sum(SCAREY_REITs$MarketCapitalization[SCAREY_REITs$firstLetter %in% GroupB])

numSCAREY_REITs <- length(SCAREY_REITs$Symbol)  
SCAREY_REITs$CurrentWeight <- 1
SCAREY_REITs <- SCAREY_REITs %>% mutate(CurrentWeight = 
                                          ifelse(firstLetter %in% GroupA,
                                                 MarketCapitalization / MktCapGroupA,
                                                 0),
                                        FutureWeight = 
                                          ifelse(firstLetter %in% GroupB,
                                                 MarketCapitalization / MktCapGroupB,
                                                 0))
# Confirm that weights add to 1.0    
sum(SCAREY_REITs$CurrentWeight)
sum(SCAREY_REITs$FutureWeight)

# grab daily returns for all SCAREY_REITS
getSymbols(SCAREY_REITs$Symbol, src = "yahoo")

DateRange <- "2018-12-31/2020-12-31"

for(nreit in 1:numSCAREY_REITs){
  
  # convert each xts time series object to weekly frequency
  temp <- to.weekly(get(SCAREY_REITs$Symbol[nreit]))
  
  # limit data to desired date range and keep only the Adjusted Close data
  temp <- temp[DateRange,grepl("Adjusted",names(temp))]
  
  # set name
  names(temp) <- c("Adjusted")
  
  # calculate log returns
  temp$logReturns <- log(temp$Adjusted/stats::lag(temp$Adjusted))

  if(nreit == 1){
    SCAREY_REITs_df <- data.frame(date = index(temp), 
                                  logReturn = coredata(temp[,"logReturns"]))
  } else {
    SCAREY_REITs_df <- merge(SCAREY_REITs_df,data.frame(date = index(temp), 
                                                        logReturn = coredata(temp[,"logReturns"])),
                             all = TRUE)
  }
  names(SCAREY_REITs_df)[dim(SCAREY_REITs_df)[2]] = SCAREY_REITs$Symbol[nreit]
}
  
# Remove NA entries in first row
SCAREY_REITs_df <- SCAREY_REITs_df[-1,]

scarey_means = colMeans(SCAREY_REITs_df[,-1])
scarey_stdevs = apply(SCAREY_REITs_df[,-1],2,sd)
scarey_correlations = cor(SCAREY_REITs_df[,-1])

# ensure correlation matrix is positive semi-definite
# Adjust if needed
temp <- nearPD(cor(SCAREY_REITs_df[,-1]), corr = TRUE, eig.tol = 1e-4)
scarey_correlations <- as.matrix(temp$mat)
#chol(scarey_correlations)

scarey_covariance <- diag(scarey_stdevs, 
                          nrow = numSCAREY_REITs,
                          ncol = numSCAREY_REITs) %*%
                     scarey_correlations %*%
                     diag(scarey_stdevs, 
                          nrow = numSCAREY_REITs,
                          ncol = numSCAREY_REITs)



output_df <- merge(as.data.frame(scarey_means), 
                   as.data.frame(scarey_stdevs), 
                   by = "row.names") 
names(output_df) <- c("Symbol","Mean","Stdev")
output_df <- merge(output_df,
                   as.data.frame(scarey_correlations) %>% mutate(Symbol = rownames(scarey_correlations)),
                   by = "Symbol")
output_df <- merge(SCAREY_REITs[,c("Name","Symbol")],
                   output_df,
                   by = "Symbol")
output_df <- output_df[order(output_df$Name),]

write_excel_csv(output_df,file.path(workingDirectory,outputFile))
