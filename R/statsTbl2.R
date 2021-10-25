library(here)
i_am(
  "R/statsTbl2.R"
)



#' Gather results of quadratic regression w/o assumption or generalized check/options
#'
#' @param ...nestedVarDataTbl
#' @param ...formula2
#'
#' @return large tibble
#' @export
#'
#' @examples
getStatsTbl2 <- function(
  ...nestedVarDataTbl,
  #user's
  ...formula2
) {

  ...nestedVarDataTbl %>%
    statFitTbl(
      ...formula2
    ) %>%
    addStatEval2(
      ...formula2
    )
}


#uniqueSub-funcs

#' Mutates key lm info onto tbl--needs $statPrint
#'
#' @param ...statFitTbl2
#' @param ....formula2
#'
#' @return
#' @export
#' @import tidyverse
#' @examples
addStatEval2 <- function(
  ...statFitTbl2,
  ....formula2
) {

  ...statFitTbl2 %>%

    mutate(
      #diffhere
      "curveName" =
        statPrint %>%
        modify(
          ~ .x %>%
            filter(
              term !=
                "(Intercept)"
            ) %>%
            pull(
              term
            )
        ),
      "pval" =
        statPrint %>%
        modify(
          ~ .x %>%
            filter(
              term !=
                "(Intercept)"
            ) %>%
            pull(
              p.value
            )
        ),
      "isSignif2" =
        pval %>%
        modify(
          ~ if_else(
            .x <
              0.1055,
            T, F
          )
        )
    ) %>%

    mutate(
      "R2adj" =
        statPrint %>%
        modify(
          ~ .x %>%
            filter(
              term !=
                "(Intercept)"
            ) %>%
            pull(
              adj.r.squared
            )
        )
    ) %>%
    unnest(
      R2adj
    )
}


#useSeparate

#' Mutates quadratic plots onto tbl--needs $varData
#'
#' @param ...statEvalNonTbl2
#' @param ....formula
#'
#' @return mutated tbl
#' @export
#' @import tidyverse
#' @examples
addGraph2 <- function(
  ...statEvalNonTbl2,
  ....formula
) {

  ...statEvalNonTbl2 %>%

    mutate(
      graph2 =
        varData %>%
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
