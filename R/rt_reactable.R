# rt_reactable.R
# ::rtemis::
# 2022 EDG lambdamd.org

#' View table using reactable
#' 
#' @param x data.frame, data.table or similar
#' @param lightsout Logical: If TRUE, use dark theme
#' 
#' @author E D Gennatas
#' @export

rt_reactable <- function(x,
                         lightsout = TRUE,
                         bg = "#121212",
                         pagination = TRUE,
                         searchable = TRUE,
                         bordered = TRUE, ...) {
    theme <- if (lightsout) {
        reactable::reactableTheme(
            color = "#fff",
            backgroundColor = bg,
            borderWidth = 0,
            stripedColor = "hsl(0, 0%, 12%)",
            highlightColor = "hsl(233, 12%, 24%)",
            inputStyle = list(
                backgroundColor = bg,
                borderColor = "hsl(0, 0%, 37%)"
            ),
            headerStyle = list(
                background = "hsl(0, 0%, 20%)",
                color = "#fff",
                "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 15%)"),
                "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 10%)"),
                borderColor = "#00000000"
            )
        )
    } else {
        reactable::reactableTheme(
            color = "#080808",
            backgroundColor = bg,
            borderWidth = 0,
            headerStyle = list(
                borderWidth = 0,
                background = "hsl(0, 0%, 85%)",
                color = "#000",
                "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 80%)"),
                "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 75%)"),
                borderColor = "#ffffff00"
            )
        )
    }

    options(reactable.theme = reactable::reactableTheme(
        # color = "hsl(233, 9%, 87%)",
        backgroundColor = "hsl(233, 9%, 19%)",
        # borderColor = "hsl(233, 9%, 22%)",
        # stripedColor = "hsl(233, 12%, 22%)",
        # highlightColor = "hsl(233, 12%, 24%)",
        # inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        # selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        # pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        # pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    ))
    
    reactable::reactable(x,
        searchable = searchable,
        pagination = pagination,
        bordered = bordered,
        resizable = TRUE,
        striped = TRUE,
        showSortable = TRUE,
        defaultColDef = reactable::colDef(
            header = function(value) gsub("_", " ", value, fixed = TRUE),
            cell = function(value) format(value, digits = 2, nsmall = 2),
            align = "right",
            headerStyle = list(background = "#707070", color = "#fff")
        ),
        theme = theme, ...
    )
}