# Random Forest Model -----------------------------------------------------

trainRandomForest <- function(data,
                              response,
                              predictors = names(data)[names(data) != response],    # Default to all variables except the response variable
                              requiredCostReduction = 0.2,                          # Defualt to 0.2
                              samplePredictorCount = floor(length(predictors)^0.5), # Defualt to m = sqrt(p)
                              treeCount,                                            # NEW to determine number of decision trees to grow
                              bootstrapRatio = 0.8){                                # NEW to determine bootstrap sample size
  
  # A bit of housekeeping ---------------------------------------------------
  
  # Set progress bar
  pb <- txtProgressBar(min = 0, max = treeCount, style = 3)
  
  # Initialize list of n trees
  output <- vector("list", length = treeCount)
  
  # Name each element for readibility
  names(output) <- c(1:treeCount)
  
  
  # Grow random forest ------------------------------------------------------
  
  # Loop through our treeCount variable to grow n trees
  for(i in 1:treeCount){
    
    # Update progress bar
    setTxtProgressBar(pb, i)
    
    # Perform our bootstrap selection of observations
    #sample.data <- data[sample(nrow(data),floor(nrow(data)*bootstrapRatio), replace = TRUE),]
    res <- resample(data, n.resamples = 10, resampler = "kfold", verbose = TRUE)
    data.train <- data[res$Fold_1, ]
    data.test <- data[-res$Fold_1, ]
      
      
    # Train decision tree i on our bootstrapped data
    output[[i]] <- rtemis::s.ADDTREE(data.train,x.test = data.test, gamma = .8, learning.rate = .1)
  
  }
  
  # Close progress bar
  close(pb)
  
  # Return list of decision trees
  output
  
}
# Finished!
