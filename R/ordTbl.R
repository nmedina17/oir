#ordinate

#' Organize ordination attributes into plot-able tibble
#'
#' @param ...commTbl
#' @param ...metaTbl
#'
#' @return tibble of coordinates and sample attributes
#' @export
#'
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
