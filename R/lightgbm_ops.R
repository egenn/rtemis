# lightgbm_ops
# ::rtemis::
# 2023 EDG lambdamd.org

# get_lgb_tree ----
get_lgb_tree <- function(x, n_iter = 10) {
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
preorderlgb <- function(tree,
                        node,
                        rule = "TRUE",
                        left = "left_child",
                        right = "right_child",
                        split_feature = "split_feature",
                        # decision_type = "decision_type",
                        threshold = "threshold",
                        xnames,
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
        " <= ",
        # node[[decision_type]],
        # " ",
        node[[threshold]]
    )
    rule_right <- paste0(
        rule,
        " & ",
        xnames[node[[split_feature]] + 1],
        " > ",
        # node[[decision_type]],
        # " ",
        node[[threshold]]
    )
    rule_left <- preorderlgb(
        tree,
        node[[left]], rule_left, left, right, split_feature,
        # decision_type,
        threshold, xnames, trace
    )
    rule <- c(rule, rule_left)
    rule_right <- preorderlgb(
        tree,
        node[[right]], rule_right, left, right, split_feature,
        # decision_type,
        threshold, xnames, trace
    )
    rule <- c(rule, rule_right)
}


# lgb2rules ----
lgbtree2rules <- function(x, xnames) {
    tree <- new.env()
    tree$leafs <- character()
    preorderlgb(tree, x, xnames = xnames)
    # remove root node "TRUE & "
    substr(tree$leafs, 8, 99999)
}


# lgb2rules ----
lgb2rules <- function(x, n_iter = NULL, xnames) {
    if (is.null(n_iter)) n_iter <- length(x)
    trees <- get_lgb_tree(x, n_iter)
    lapply(
        trees,
        \(x) lgbtree2rules(x, names(dat))
    ) |> unlist() |> unique()
}
