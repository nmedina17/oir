library(here)
i_am(
  "R/ordTbl.R"
)
#ordinate

#' Organize ordination attributes into plot-able tibble
#'
#' @param ...commTbl
#' @param ...metaTbl
#'
#' @return tibble of coordinates and sample attributes
#' @export
#' @import tidyverse, vegan
#' @examples
getOrdVarTbl <- function(
  ...commTbl,
  ...metaTbl
) {


  ord = rda(
    ...commTbl
  )

  ordTbl = scores(
    ord
  )$sites %>%
    as_tibble()


  cbind(
    ...metaTbl,
    ordTbl
  ) %>%
    as_tibble()
}
