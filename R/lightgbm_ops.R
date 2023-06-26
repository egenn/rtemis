# lightgbm_ops
# ::rtemis::
# 2023 EDG lambdamd.org

# get_lgb_tree ----

#' Get LightGBM Booster Trees
#' 
#' @returns A list of trees
#' @keywords internal
get_lgb_tree <- function(x, n_iter = -1) {
    out <- lapply(
        jsonlite::fromJSON(
            lightgbm::lgb.dump(
                booster = x,
                num_iteration = n_iter
            ),
            simplifyVector = FALSE
        )$tree_info,
        \(y) y$tree_structure
    )
    names(out) <- paste0("Tree_", seq_along(out))
    out
}


# preorderlgb ----

#' Preorder Traversal of LightGBM Tree
#' 
#' Called by `lgbtree2rules` and operates on `tree` environment in place.
#' 
#' @param tree A LightGBM tree
#' 
#' @keywords internal
preorderlgb <- function(tree,
                        node,
                        rule = "TRUE",
                        left = "left_child",
                        right = "right_child",
                        split_feature = "split_feature",
                        threshold = "threshold",
                        xnames,
                        factor_levels,
                        trace = 0) {
    if (is.null(node[[split_feature]])) {
        names(rule) <- "leaf"
        if (trace > 0) {
            cat("Reached a leaf; rule is ", rule, "\n")
        }
        tree$leafs <- c(tree$leafs, rule)
        return(rule)
    }
    rule_left <- paste0(
        rule,
        " & ",
        xnames[node[[split_feature]] + 1],
        decision_left(node[["decision_type"]]),
        fmt_thresh(
            node[["decision_type"]] == "==", 
            xnames[node[[split_feature]] + 1],
            node[["threshold"]], 
            factor_levels
        )
    )
    rule_right <- paste0(
        rule,
        " & ",
        xnames[node[[split_feature]] + 1],
        decision_right(node[["decision_type"]]),
        fmt_thresh(
            node[["decision_type"]] == "==",
            xnames[node[[split_feature]] + 1],
            node[["threshold"]], 
            factor_levels
        )
    )
    rule_left <- preorderlgb(
        tree,
        node[[left]], rule_left, left, right, split_feature,
        threshold, 
        xnames,
        factor_levels,
        trace
    )
    rule <- c(rule, rule_left)
    rule_right <- preorderlgb(
        tree,
        node[[right]], rule_right, left, right, split_feature,
        threshold, 
        xnames,
        factor_levels,
        trace
    )
    rule <- c(rule, rule_right)
}


# lgbtree2rules ----
lgbtree2rules <- function(x, xnames, factor_levels) {
    tree <- new.env()
    tree$leafs <- character()
    preorderlgb(tree, x, xnames = xnames, factor_levels = factor_levels)
    # remove root node "TRUE & "
    substr(tree$leafs, 8, 99999)
}


# lgb2rules ----
#' Convert LightGBM Booster to set of rules
#' 
#' @param x LightGBM Booster object
#' @param n_iter Integer: Number of trees to convert to rules
#' @param xnames Character vector: Names of features
#' 
#' @return Character vector of rules
#' @keywords internal
lgb2rules <- function(Booster, n_iter = NULL, xnames, factor_levels) {
    if (is.null(n_iter)) n_iter <- length(Booster)
    trees <- get_lgb_tree(Booster, n_iter)
    lapply(
        trees,
        \(x) lgbtree2rules(x, xnames, factor_levels)
    ) |> unlist() |> unique()
}


#' Extract set of rules from LightGBM rtMod
#' 
#' @param rtmod LightGBM rtMod object
#' @param dat Data frame used to train rtmod
#' @param n_iter Integer: Number of trees to convert to rules
#' 
#' @return Character vector of rules
#' @keywords internal
rtlgb2rules <- function(rtmod, dat, n_iter = NULL) {
    lgb2rules(
        Booster = rtmod$mod,
        n_iter = n_iter,
        xnames = rtmod$xnames, 
        factor_levels = dt_get_factor_levels(dat)
    )
}


# decision_left ----
decision_left <- function(decision_type) {
    switch(
        decision_type,
        "<=" = " <= ",
        "==" = " %in% "
    )
}

decision_right <- function(decision_type) {
    switch(decision_type,
        "<=" = " > ",
        "==" = " %!in% "
    )
}

# fmt_thresh <- function(threshold) {
#     if (grepl("\\|\\|", threshold)) {
#         paste0(
#             "c(",
#             paste(
#                 strsplit(threshold, "\\|\\|")[[1]], 
#                 collapse = ","), 
#             ")"
#         )
#     } else {
#         threshold
#     }
# }

#' Format rules to convert integer levels to factor levels
#'
#' @param rules Character vector of rules
#' @param factor_levels Named list of factor levels. Names should correspond to training
#' set column names.
#'
#' @keywords internal
# catlabel_rules <- function(rules, factor_levels) {

# }

#' Format rule thresholds for categorical splits
#' 
#' @param feature Character: feature name
#' @param threshold Character: threshold as reported by lightgbm
#' @param factor_levels Named list of factor levels. Names should correspond to training
#' set column names.
fmt_thresh <- function(catsplit, feature, threshold, factor_levels) {
    if (catsplit) {
        flevels <- as.integer(strsplit(threshold, "\\|\\|")[[1]]) + 1
        flevels <- factor_levels[[feature]][flevels]
        paste0(
            "c(",
            paste0('"', flevels, '"', collapse = ","),
            ")"
        )
    } else {
        threshold
    }
}


