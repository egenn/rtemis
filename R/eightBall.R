# eightball.R
# ::rtemis::
# 2019 E.D. Gennatas lambdamd.org

#' Magic 8-Ball
#'
#' @param question Character: Your question for the magic 8-ball
#' @export
#' @author E.D. Gennatas

eightball <- function(question = NULL) {

  response <- c("It is certain.",
                "It is decidedly so.",
                "Without a doubt.",
                "Yes - definitely.",
                "You may rely on it.",
                "As I see it, yes.",
                "Most likely.",
                "Outlook good.",
                "Yes.",
                "Signs point to yes.",
                "Reply hazy, try again.",
                "Ask again later.",
                "Better not tell you now.",
                "Cannot predict now.",
                "Concentrate and ask again.",
                "Don't count on it.",
                "My reply is no.",
                "My sources say no.",
                "Outlook not so good.",
                "Very doubtful.")
  type <- c(rep("affirmative", 10), c(rep("non-commital", 5)), c(rep("negative", 5)))

  rn <- sample(seq(response), 1)
  .response <- response[rn]
  .type <- type[rn]
  col <- switch(.type,
                 affirmative = crayon::green$bold$italic,
                 `non-commital` = crayon::yellow$bold$italic,
                 negative = crayon::red$bold$italic)

  if (!is.null(question)) cat("  ", bold(question), "\n")
  cat(col(">>", .response), "\n")

  if (length(grep("darwin", sessionInfo()$platform)) == 1) {
    system(paste("say -v Samantha", gsub("'", "", .response)))
  }

} # rtemis::eightBall
