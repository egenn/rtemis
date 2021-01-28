#' devmode internal use only
#'
devmode <- function() {

  library(rtemis)
  library(plyr)
  library(parallel)
  intro <<- rtemis:::intro
  outro <<- rtemis:::outro
  getName <<- rtemis:::getName
  errorSummary <<- rtemis:::errorSummary
  dataSummary <<- rtemis:::dataSummary
  gridSummary <<- rtemis:::gridSummary
  gridCheck <<- rtemis:::gridCheck
  gridSearchLearn <<- rtemis:::gridSearchLearn
  checkType <<- rtemis:::checkType
  rtMod.out <<- rtemis:::rtMod.out
  rtCores <<- 4
  rtenv <<- new.env()

} # rtemis::devmode
