library(here)
i_am(
  "R/statsTbl12.R"
)



#' Gather results, for already-summarized data, of quadratic regression w/o assumption or generalized check/options
#'
#' @param ...nestedVarDataTbl
#' @param ...formula
#'
#' @return large tibble
#' @export
#' @import tidyverse
#' @examples
getStatsTbl12 <- function(
  #has"varData1"col
  ...nestedVarDataTbl,
  #user's
  ...formula
) {

  ...nestedVarDataTbl %>%

    statFitTbl1(
      ...formula
    ) %>%
    addStatEval2(
      ...formula
    )
}


#uniqueSub-funcs

#' Mutates quadratic plots, for already-summarized data, onto tbl--needs $varData
#'
#' @param ...statEvalNonTbl2
#' @param ....formula
#'
#' @return mutated tbl
#' @export
#' @import tidyverse
#' @examples
addGraph12 <- function(
  ...statEvalNonTbl2,
  ....formula
) {

  ...statEvalNonTbl2 %>%

    mutate(
      graph2 =
        varData1 %>%  #diffhere
        modify(
          ~ .x %>%
            ggplot(
              aes(
                x = {
                  ....formula[[3]] %>%
                    eval()
                },
                y = {
                  ....formula[[2]] %>%
                    eval()
                }
              )
            ) +
            geom_quasirandom() +
            geom_smooth(
              method = "lm",
              formula = y ~ poly(
                x,
                2
              )
            ) +
            stat_poly_eq(
              formula = y ~ poly(
                x,
                2
              ),
              parse = F,
              aes(
                label = paste(
                  after_stat(
                    p.value.label
                  ),
                  after_stat(
                    adj.rr.label
                  )
                )
              )
            ) +
            labs(
              y = deparse(
                ....formula[[2]]
              ),
              x = deparse(
                ....formula[[3]]
              )
            )
        )
    )
}
