library(here)
i_am(
  "R/ordTbl.R"
)
#ordinate

#' Organize ordination attributes into plot-able tibble
#'
#' @param ...commTbl
#' @param ...metaTbl tbl of site info as cols
#'
#' @return tibble of coordinates and sample attributes
#' @export
#' @import tidyverse, vegan
#' @examples
getOrdVarTbl <- function(
  ...commTbl,
  ...metaTbl
) {


  ord <- rda(
    ...commTbl
  )

  ordTbl <- scores(
    ord
  )$sites %>%
    as_tibble()


  cbind(
    ...metaTbl,
    ordTbl
  ) %>%
    as_tibble() %>%

    return()
}
#ggplot::scale_color_brewer()



#experimental----


#' Get stats from ordination
#'
#' @param ...commTbl
#' @param ...cleanData
#' @param uniqueLevels vector of variable names
#'
#' @return
#' @export
#' @import vegan, tidyverse
#' @examples
getOrdStatTbl <- function(
  ...commTbl,
  ...cleanData,
  ...uniqueLevels,
  ...mainVar
) {


  uLevels <- quote(
    uniqueLevels
  )

  distMat <- vegdist(
    commTbl
  )

  metaTbl <- ...cleanData %>%
    distinct(
      #likelyIssueHere
      uLevels
    )


  # ordModel <- quote(
  #   distMat ~
  #     ...mainVar
  # )

  ordStat <- adonis(
    ordModel,
    metaTbl,
    # 99999
    permutations = 99
  )
}
