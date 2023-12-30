########
#
#  Mean-Variance solver example
#
#  CFRM503, Asset Allocation and Portfolio Management
#  University of Washington
#  Steve Murray
#  April 6, 2021
#
########


# clear environment
rm(list = ls())

# load libraries
library(tidyverse)
library("Matrix")
library("readxl")
library(RColorBrewer)
library(quadprog)
library(CLA)

# identify necessary folders
workingDirectory <- "C:/Users/rohan/Documents/UW/CFRM-503/Assignment 1"
inputFile <- "MVInputsExample.xlsx"
# a larger set of investments
#inputFile <- "MVInputs.xlsx"

# files to source


ReadAndRepairInputs <- function(fileNameIncludingPath){

  # read investment statistics
  mvInputs <- read_excel(fileNameIncludingPath)

  # Parse investment statistics data into means, standard deviations, #  correlations and covariance
  NumInvestments = length(mvInputs$Investment) 

  investmentMeans <- matrix(mvInputs$Mean,ncol= 1)
  investmentStdevs <- matrix(mvInputs$Stdev,ncol = 1)
  originalCorrelations <- as.matrix(mvInputs[1:NumInvestments,4:(NumInvestments + 3)],
                                  nrow = NumInvestments, ncol = NumInvestments)

  # repair correlation matrix if necessary
  temp <- nearPD(originalCorrelations, corr = TRUE, eig.tol = 1e-4)
  investmentCorrelations <- as.matrix(temp$mat)

  rownames(investmentMeans) <- mvInputs$Investment
  rownames(investmentStdevs) <- mvInputs$Investment
  rownames(investmentCorrelations) <- mvInputs$Investment

  investmentCovariance <- diag(investmentStdevs[,1]) %*% 
                            investmentCorrelations %*% 
                            diag(investmentStdevs[,1])
  colnames(investmentCovariance) <- mvInputs$Investment
  rownames(investmentCovariance) <- mvInputs$Investment

  # risk-free rate
  riskFree = 0.01
  
  return(list(Means = investmentMeans,
              Stdevs = investmentStdevs,
              Covariance = investmentCovariance,
              Correlation = investmentCorrelations,
              riskFree = riskFree))
}


BuildMatrix <- function(numInvestments){

  # Build the constraint matrix
  # equality constraints must come first when using solve.QP()
  
  # full investment constraint
  Amat <- matrix(1, 
               ncol = numInvestments, 
               nrow = 1)
  
  # non-negativity constraints
  Amat <- rbind(Amat, 
              diag(1,
                   ncol = numInvestments,
                   nrow = numInvestments))
  
  # Build the right hand side values
  RHS <- matrix(0,
              nrow = numInvestments + 1,
              ncol = 1)
  RHS[1] <- 1
  
  # number of equality constraints
  meq = 1
  
  return(list(Amat = Amat,
              RHS = RHS,
              meq = 1))
}


MVSolve <- function(constraintMatrix, forecast, numFrontierPoints = 100){
  
  # tolerance for matching values
  tolerance = 1.0e-6
  
  # determine minimum risk tolerance
  minVarPortfolio <- solve(forecast$Covariance,
                           matrix(1, nrow = dim(forecast$Covariance)[1], ncol = 1))
  minVarPortfolio <- minVarPortfolio/sum(minVarPortfolio)
  minVar <- sqrt(t(minVarPortfolio) %*%
                   forecast$Covariance %*%
                   minVarPortfolio)
  minRiskTol <- 1.0e-8
  optStdev = 0
  while(optStdev <= minVar * (1+tolerance)){
    
    solution <- solve.QP(forecast$Covariance,
                         minRiskTol * forecast$Means, 
                         t(constraintMatrix$Amat), 
                         constraintMatrix$RHS, 
                         meq = constraintMatrix$meq, 
                         factorized = FALSE)
    optStdev <- sqrt(t(as.matrix(solution$solution)) %*%
                            forecast$Covariance %*%
                            as.matrix(solution$solution))
                       
    minRiskTol <- minRiskTol * 1.2
  }
  minRiskTol <- minRiskTol / 1.2

    # determine maximum risk tolerance
  maxRiskTol = minRiskTol
  optMean = 0
  while(optMean < max(forecast$Means)*(1-tolerance)){  ## The logic for identifying
                                         ## maximum risk tolerance 
                                         ## only works if leverage
                                         ## is not allowed
    solution <- solve.QP(forecast$Covariance,
                         maxRiskTol * forecast$Means, 
                         t(constraintMatrix$Amat), 
                         constraintMatrix$RHS, 
                         meq = constraintMatrix$meq, 
                         factorized = FALSE)
    optMean <- t(forecast$Means) %*% as.matrix(solution$solution)
    maxRiskTol <- maxRiskTol * 1.2
  }
  
  # Create risk tolerance values
  RTol <- seq(minRiskTol, maxRiskTol, length.out = numFrontierPoints)

  # Pre-allocate space for the results
  OptWeights <- matrix(0,
                       nrow = length(RTol), 
                       ncol = length(subsetInvestments))
  OptObj <- matrix(0,
                   nrow = length(RTol), 
                   ncol = 1)
  OptMeans <- matrix(0,
                     nrow = length(RTol), 
                     ncol = 1)
  OptStdevs <- matrix(0,
                      nrow = length(RTol), 
                      ncol = 1)
  
  
  for(rtol in 1:length(RTol)){
    # solve the model
    solution <- solve.QP(forecast$Covariance,
                         RTol[rtol] * forecast$Means, 
                         t(constraintMatrix$Amat), 
                         constraintMatrix$RHS, 
                         meq = constraintMatrix$meq, 
                         factorized = FALSE)
    
    # save important parts of the solution
    solutionMat <- as.matrix(solution$solution)
    OptMeans[rtol] <- t(forecast$Means) %*%
      solutionMat 
    OptStdevs[rtol] <- sqrt(t(solutionMat) %*%
                              forecast$Covariance %*%
                              solutionMat)
    OptWeights[rtol,] = solutionMat
    OptObj[rtol] = solution$value 
  }
  return(list(OptMeans = OptMeans,
              OptStdevs = OptStdevs,
              OptWeights = OptWeights,
              OptObj = OptObj,
              RTol = RTol,
              numFrontierPoints = numFrontierPoints))
}


MinRiskSolve <- function(constraintMatrix, forecast, numFrontierPoints = 100){
  tolerance = 1.0e-5
  
  minReturnTarget <- min(forecast$Means)
  maxReturnTarget <- max(forecast$Means)

  # Append return target constraint to other constraints
  constraintMatrix$Amat <- rbind(t(forecast$Means),
                                 constraintMatrix$Amat)
  constraintMatrix$RHS <- rbind(minReturnTarget,
                                constraintMatrix$RHS)
  
  # Create return target values
  RTarget <- seq(minReturnTarget + tolerance, 
                 maxReturnTarget - tolerance, 
                 length.out = numFrontierPoints)
  
  # Pre-allocate space for the results
  OptWeights <- matrix(0,
                       nrow = length(RTarget), 
                       ncol = length(subsetInvestments))
  OptObj <- matrix(0,
                   nrow = length(RTarget), 
                   ncol = 1)
  OptMeans <- matrix(0,
                     nrow = length(RTarget), 
                     ncol = 1)
  OptStdevs <- matrix(0,
                      nrow = length(RTarget), 
                      ncol = 1)
  
  ZeroVector = matrix(0,nrow = 1, ncol = length(forecast$Means))
  for(rtarget in 1:length(RTarget)){
    # solve the model
    constraintMatrix$RHS[1,] = RTarget[rtarget]
    solution <- solve.QP(forecast$Covariance,
                         ZeroVector, 
                         t(constraintMatrix$Amat), 
                         constraintMatrix$RHS, 
                         meq = constraintMatrix$meq + 1, 
                         factorized = FALSE)
    
    # save important parts of the solution
    solutionMat <- as.matrix(solution$solution)
    OptMeans[rtarget] <- t(forecast$Means) %*%
      solutionMat 
    OptStdevs[rtarget] <- sqrt(t(solutionMat) %*%
                              forecast$Covariance %*%
                              solutionMat)
    OptWeights[rtarget,] = solutionMat
    OptObj[rtarget] = solution$value 
  }
  return(list(OptMeans = OptMeans,
              OptStdevs = OptStdevs,
              OptWeights = OptWeights,
              OptObj = OptObj,
              numFrontierPoints = numFrontierPoints))
}

   

# Done declaring functions


#######
##
##  Main commands
##
######

forecast <- ReadAndRepairInputs(file.path(workingDirectory,inputFile))

# Organize a subset of available investments
subsetInvestments <- c(1, 2,3) # USLC, Commodity and Agg
subsetNames <- rownames(forecast$Means)[subsetInvestments]
subsetMeans <- forecast$Means[subsetInvestments]
subsetStdevs <- forecast$Stdevs[subsetInvestments]
subsetCorrelations <- forecast$Correlations[subsetInvestments,subsetInvestments]
subsetCovariance <- forecast$Covariance[subsetInvestments,subsetInvestments]
subsetForecast <- list(Means = subsetMeans,
                       Stdevs = subsetStdevs,
                       Covariance = subsetCovariance,
                       Correlation = subsetCorrelations,
                       riskFree = forecast$riskFree)

# Build constraint matrix
constraintMatrix <- BuildMatrix(length(subsetInvestments))

# Solve for frontier
MV_frontier <- MVSolve(constraintMatrix, subsetForecast)
MinRisk_frontier <- MinRiskSolve(constraintMatrix, subsetForecast)


# Organize plot data
plotData <- data.frame(Means = MV_frontier$OptMeans, 
                       Stdevs = MV_frontier$OptStdevs, 
                       Group = "MeanVar")
plotData <- rbind(plotData,
                  data.frame(Means = MinRisk_frontier$OptMeans,
                             Stdevs = MinRisk_frontier$OptStdevs,
                             Group = "MinRisk"))


# Create the MeanVar plot
MeanVar_Plot <- ggplot(data = plotData %>% 
                   filter(Group == "MeanVar"), 
                 aes(x = Stdevs, y = Means)) +
  ggtitle("Mean Variance Frontier") +
  geom_line(aes(x = Stdevs, y = Means), 
            color = "darkblue", size=  1.5) +
  theme(text = element_text(size = 16)) + 
  scale_x_continuous(labels = scales::percent, 
                     limits = c(0,NA)) +
  scale_y_continuous(labels = scales::percent, 
                     limits = c(0,NA)) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")

# Plot it
plot(MeanVar_Plot)



# Create the MinRisk plot
MinRisk_Plot <- ggplot(data = plotData %>% 
                         filter(Group == "MinRisk"), 
                       aes(x = Stdevs, y = Means)) +
  ggtitle("Min Risk Frontier") +
  geom_point(aes(x = Stdevs, y = Means), 
            color = "darkblue", size=  1.5) +
  theme(text = element_text(size = 16)) + 
  scale_x_continuous(labels = scales::percent, 
                     limits = c(0,NA)) +
  scale_y_continuous(labels = scales::percent, 
                     limits = c(0,NA)) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")

# Plot it
plot(MinRisk_Plot)


# Create an area chart of allocations
AllocationData <- as.data.frame(MV_frontier$OptWeights)
names(AllocationData) <- subsetNames
AllocationData <- AllocationData %>% 
                      mutate(RTol = MV_frontier$RTol,
                              RAversion = 1.0/MV_frontier$RTol,
                              Stdev = MV_frontier$OptStdevs,
                              Vars = Stdev^2)
AllocationData <- AllocationData %>% pivot_longer(subsetNames, 
                                                  names_to = "Investment", 
                                                  values_to = "Allocation")

# Allocation versus standard deviation
Allocation_Plot <- ggplot(data = AllocationData, 
                          aes(x=Stdev, y = Allocation, fill = Investment)) +
  geom_area() +
  ylim(0,1.0)
plot(Allocation_Plot)

# Allocation versus variance
Allocation_Plot <- ggplot(data = AllocationData, 
                          aes(x=Vars, y = Allocation, fill = Investment)) +
  geom_area() +
  ylim(0,1.0)
plot(Allocation_Plot)

# Allocation versus risk aversion
Allocation_Plot <- ggplot(data = AllocationData, 
                          aes(x=RAversion, y = Allocation, fill = Investment)) +
  geom_area() +
  ylim(0,1.0)
plot(Allocation_Plot)


# Allocation versus risk tolerance
CLA_Results <- CLA(forecast$Means, forecast$Covariance,0,1)
Allocation_Plot <- ggplot(data = AllocationData, 
                          aes(x=RTol, y = Allocation, fill = Investment)) +
  geom_area() +
  geom_vline(xintercept = CLA_Results$lambdas, color = "darkblue",
             size = 1.1)
plot(Allocation_Plot)

