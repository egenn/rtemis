# Random Forest Model -----------------------------------------------------

trainRandomForest <- function(data,
                     n.trees = 1000,
                     weights = NULL,
                     ipw = TRUE,
                     ipw.type = 2,
                     ipw.case.weights = TRUE,
                     ipw.class.weights = FALSE,
                     upsample = FALSE,
                     downsample = FALSE,
                     resample.seed = NULL,
                     autotune = FALSE,
                     classwt = NULL,
                     treeCount = 500,
                     stepFactor = 2,
                     mtry = NULL,
                     mtryStart = NULL,
                     inbag.resample = NULL,
                     stratify.on.y = FALSE,
                     grid.resample.rtset = rtset.resample("kfold", 5),
                     grid.search.type = c("exhaustive", "randomized"),
                     grid.randomized.p = .1,
                     metric = NULL,
                     maximize = NULL,
                     probability = FALSE,
                     importance = "impurity",
                     replace = TRUE,
                     min.node.size = NULL,
                     splitrule = NULL,
                     strata = NULL,
                     sampsize = if (replace) nrow(x) else ceiling(.632*nrow(x)),
                     tune.do.trace = FALSE,
                     imetrics = FALSE,
                     n.cores = rtCores,
                     print.tune.plot = FALSE,
                     print.plot = TRUE,
                     plot.fitted = NULL,
                     plot.predicted = NULL,
                     plot.theme = getOption("rt.fit.theme", "lightgrid"),
                     question = NULL,
                     grid.verbose = TRUE,
                     verbose = TRUE,
                     outdir = NULL,
			   bootstrapRatio = 0.8,
                     save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) 
  {
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
    
    ## 75% of the sample size
	smp_size <- floor(0.75 * nrow(mtcars))

	## set the seed to make your partition reproducible
	set.seed(123)
	train_ind <- sample(seq_len(nrow(mtcars)), size = smp_size)

	train <- data[train_ind, ]
	test <- data[-train_ind, ]

    
    # Train decision tree i on our bootstrapped data
    output[[i]] <- s.ADDTREE(train ,test, gamma = .8, learning.rate = .1)
  
  }
  
  # Close progress bar
  close(pb)
  
  # Return list of decision trees
  output
  
}
# Finished!
