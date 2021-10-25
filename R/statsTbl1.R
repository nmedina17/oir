library(here)
i_am(
  "R/statsTbl1.R"
)



#' Gather results, for already-summarized data, of basic regression, assumptions checks and generalized options
#'
#' @param ...nestedVarDataTbl
#' @param ...formula
#'
#' @return large tibble
#' @export
#'
#' @examples
getStatsTbl1 <- function(
  #has"varData1"col
  ...nestedVarDataTbl,
  #user's
  ...formula
) {

  ...nestedVarDataTbl %>%

    statFitTbl1(
      ...formula
    ) %>%
    addStatEval(
      ...formula
    ) %>%

    addStatFitNon(
      ...formula
    ) %>%
    addStatEvalNon() %>%

    addGraph1(
      ...formula
    )
}


#uniqueSub-funcs


#' Mutates lm and summary output, of already-summarized data, onto tbl--needs $varData1, $statTest
#'
#' @param ...nestedVarDataTbl
#' @param ....formula
#'
#' @return mutated tbl
#' @export
#'
#' @examples
statFitTbl1 <- function(
  ...nestedVarDataTbl,
  ....formula
) {

  ...nestedVarDataTbl %>%

    mutate(
      "statTest" =
        varData1 %>%  #diffhere
        modify(
          ~ .x %>%
            lm(
              formula =
                ....formula
            )
        ),
      "statPrint" =
        statTest %>%
        modify(
          ~ .x %>%
            tidy() %>%
            full_join(
              .,
              .x %>%
                glance()
            ) %>%
            filter(
              !is.na(
                term
              )
            )
        )
    )
}


#' Mutates geom_smooth lm plots, for already-summarized data, onto tbl--needs $varData
#'
#' @param ...statEvalNonTbl
#' @param ....formula
#'
#' @return mutated tbl
#' @export
#'
#' @examples
addGraph1 <- function(
  ...statEvalNonTbl,
  ....formula
) {

  ...statEvalNonTbl %>%

    mutate(
      graph =
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
              method = "lm"
            ) +
            stat_poly_eq(
              formula = y ~ x,
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
