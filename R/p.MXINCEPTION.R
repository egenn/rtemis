# p.MXINCEPTION.R
# ::rtemis::
# 2018 Efstathios D. Gennatas egenn.github.io
# https://mxnet.incubator.apache.org/tutorials/r/classifyRealImageWithPretrainedModel.html

#' Classify Images using pre-trained Inception network with \code{mxnet} [C]
#'
#' Predict using Inception
#'
#' Download the pre-trained network from http://data.mxnet.io/mxnet/data/Inception.zip and save it under
#' "~/.rtemis/"
#'
#' @param img Image, "cimg" class: Input image - e.g. as read using \code{imager::load.image}. Alternatively, specify
#' \code{img.path}
#' @param img.path Character: Path to input image
#' @param inception.path Character: Path to downloaded pretrained MXNet Inception network.
#' Default = "~/.rtemis/Inception/Inception_BN"
#' @param iteration Integer: Load this iteration from the inception model. Default = 39
#' @param plot.input Logical: If TRUE, plot input image. Default = FALSE
#' @param plot.normed Logical: If TRUE, plot normed image. Default = FALSE
#' @param plot.graphviz Logical: If TRUE, plot network using graphviz. Default = FALSE
#' @param verbose Logical: If TRUE, print messages to console. Default = TRUE
#'
#' @author Efstathios D. Gennatas
#' @family Pretrained Models
#' @family Deep Learning
#' @export

p.MXINCEPTION <- function(img, img.path = NULL,
                          inception.path = "~/.rtemis/Inception/",
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

  if (plot.input) plot(img, axes = FALSE)

  # [ INCEPTION ] ====
  if (verbose) msg("Loading Inception...\n")
  model <- mxnet::mx.model.load(paste0(inception.path, "Inception_BN"), iteration = iteration)
  if (class(model)[1] != "MXFeedForwardModel") stop("Inception pretrained network not found")
  if (plot.graphviz) mxnet::graph.viz(model$symbol)
  mean.img.path <- file.path(paste0(inception.path, "/mean_224.nd"))
  mean.img <- as.array(mxnet::mx.nd.load(mean.img.path)[["mean_img"]])
  normed <- preproc.image(img, mean.img, plot.normed = FALSE)
  if (plot.normed) plot(normed, axes = FALSE)

  # [ PREDICT ] ====
  synsets <- readLines(file.path(paste0(inception.path, "/Inception/synset.txt")))
  prob <- predict(model, X = normed)
  max.idx <- max.col(t(prob))
  msg("Predicted Top-class: ", synsets[[max.idx]])

} # rtemis::p.MXINCEPTION


preproc.image <- function(im, mean.image, plot.normed = FALSE) {

  # Crop the image
  shape <- dim(im)
  short.edge <- min(shape[1:2])
  xx <- floor((shape[1] - short.edge) / 2)
  yy <- floor((shape[2] - short.edge) / 2)
  cropped <- imager::crop.borders(im, xx, yy)
  # Resize to 224 x 224, needed by input of the model.
  resized <- imager::resize(cropped, 224, 224)
  # Convert to array (x, y, channel)
  arr <- as.array(resized) * 255
  dim(arr) <- c(224, 224, 3)
  # Subtract the mean
  normed <- arr - mean.image
  # Reshape to format needed by mxnet (width, height, channel, num)
  dim(normed) <- c(224, 224, 3, 1)
  if (plot.normed) plot(imager::as.cimg(normed), axes = FALSE)
  normed
}
