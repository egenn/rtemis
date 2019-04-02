# p.MXINCEPTION.R
# ::rtemis::
# 2018 Efstathios D. Gennatas egenn.github.io
# https://mxnet.incubator.apache.org/tutorials/r/classifyRealImageWithPretrainedModel.html

#' Classify Images using pre-trained Inception network with \code{mxnet} [C]
#'
#' Predict using Inception
#'
#' @author Efstathios D. Gennatas
#' @family Pretrained Models
#' @family Deep Learning
#' @export

p.MXINCEPTION <- function(img, img.path = NULL,
                          rt.home = getOption("rt.home", "~/.rtemis/"),
                          iteration = 39,
                          plot.input = FALSE,
                          plot.normed = FALSE,
                          plot.graphviz = FALSE,
                          verbose = TRUE) {
  
  # [ INTRO ] ====
  if (missing(img) & is.null(img.path)) {
    stop("Please provide img or img.path")
  }
  
  # [ DEPENDENCIES ] ====
  if (!depCheck("mxnet", "imager", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }
  
  # [ IMAGE ] ====
  if (missing(img)) {
    img <- imager::load.image(file.path(img.path))
  }
  
  if (plot.input) plot(img)
  
  # [ INCEPTION ] ====
  inception.path <- paste0(file.path(rt.home), "/Inception/Inception_BN")
  if (verbose) msg("Loading Inception...\n")
  model <- mxnet::mx.model.load(inception.path, iteration = iteration)
  if (class(model)[1] != "MXFeedForwardModel") stop("Inception pretrained network not found")
  if (plot.graphviz) mxnet::graph.viz(model$symbol)
  mean.img.path <- file.path(paste0(rt.home, "/Inception/mean_224.nd"))
  mean.img <- as.array(mxnet::mx.nd.load(mean.img.path)[["mean_img"]])
  normed <- preproc.image(img, mean.img, plot.normed = plot.normed)
  if (plot.normed) plot(normed)
  
  # [ PREDICT ] ====
  synsets <- readLines(paste0(file.path(rt.home), "/Inception/synset.txt"))
  prob <- predict(model, X = normed)
  max.idx <- max.col(t(prob))
  # print(paste0("Predicted Top-class: ", synsets [[max.idx]]))
  msg("Predicted Top-class: ", synsets[[max.idx]])
  
} # rtemis::p.MXINCEPTION


preproc.image <- function(im, mean.image, plot.normed = FALSE) {
  # crop the image
  shape <- dim(im)
  short.edge <- min(shape[1:2])
  xx <- floor((shape[1] - short.edge) / 2)
  yy <- floor((shape[2] - short.edge) / 2)
  cropped <- imager::crop.borders(im, xx, yy)
  # resize to 224 x 224, needed by input of the model.
  resized <- imager::resize(cropped, 224, 224)
  # convert to array (x, y, channel)
  arr <- as.array(resized) * 255
  dim(arr) <- c(224, 224, 3)
  # subtract the mean
  normed <- arr - mean.image
  # Reshape to format needed by mxnet (width, height, channel, num)
  dim(normed) <- c(224, 224, 3, 1)
  if (plot.normed) plot(imager::as.cimg(normed))
  normed
}
