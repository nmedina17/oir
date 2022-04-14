library(here)
i_am(
  "R/ordTbl.R"
)
library(tidyverse)
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


  #PCA----
  ord <- rda(
    ...commTbl
  )

  #xy----
  ordTbl <- scores(
    ord
  )$sites %>%
    as_tibble()

  #percent----
  ordAxes <- ord %>%
    summary() %>%
    .$cont %>%
    .$importance %>%
    as_tibble(.) %>%
    # select(
    #   PC1, PC2
    # ) %>%
    filter(
      row_number() == 2
    ) %>%
    round(2)
    # as.numeric()


  #join----
  cbind(
    ...metaTbl,
    ordTbl
  ) %>%
    as_tibble() %>%
    mutate(
      ordAxes = ordAxes %>%
        rename_with(
          ~ paste0(
            .,
            "_prop"
          )
        )
    ) %>%
    unnest(
      ordAxes
      # names_repair = "universal"
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
