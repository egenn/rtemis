# mplot3_prp
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Plot CART Decision Tree
#'
#' Plot output of a regression or classification tree created using `rpart`
#' A wrapper for `rpart.plot::rpart.plot`
#'
#' @param object Output of [s_CART]
#' @param palette Color vector
#' @export

mplot3_prp <- function(
  object,
  type = 0,
  extra = "auto",
  branch.lty = 1,
  under = FALSE,
  fallen.leaves = TRUE,
  palette = NULL,
  filename = NULL,
  pdf.width = 7,
  pdf.height = 5,
  ...
) {
  # Arguments ----
  if (class(object)[1] == "rtMod") {
    if (class(object$mod)[1] == "rpart") {
      .mod <- object$mod
    } else stop("rtMod must be trained using s_CART")
  } else if (class(object)[1] == "rpart") {
    .mod <- object
  } else {
    stop("Input object must be either of class rtMod (s_CART) or rpart")
  }

  # Dependencies ----
  dependency_check("rpart.plot")

  # Arguments ----
  # Output directory
  if (!is.null(filename))
    if (!dir.exists(dirname(filename)))
      dir.create(dirname(filename), recursive = TRUE)

  # Palette
  if (is.null(palette)) {
    if (.mod$method == "class") {
      palette <- list(
        Blues = colorGrad(99, lo = pennCol$blue, hi = pennCol$lightestBlue),
        Reds = colorGrad(99, lo = pennCol$red, hi = pennCol$lightestRed),
        Greens = colorGrad(99, lo = pennCol$green, hi = pennCol$lightestGreen),
        Oranges = colorGrad(
          99,
          lo = pennCol$orange,
          hi = pennCol$lightestOrange
        )
      )
    } else {
      palette <- colorGrad(
        99,
        lo = pennCol$lightestBlue,
        mid = pennCol$lightestPurple,
        hi = pennCol$lighterYellow
      )
    }
  }

  # rpart.plot ----
  if (!is.null(filename)) pdf(filename, width = pdf.width, height = pdf.height)
  rpart.plot::rpart.plot(
    .mod,
    type = type,
    extra = extra,
    branch.lty = branch.lty,
    under = under,
    fallen.leaves = fallen.leaves,
    box.palette = palette,
    ...
  )

  # Outro ----
  if (!is.null(filename)) dev.off()
} # rtemis::mplot3_prp
