library(here); i_am("R/statsTbl.R")
# source(
#   here("analysis/stats.R")
# ) #...$varData
library(tidymodels) #glance()
library(ggbeeswarm) #quasirandom()
library(ggpmisc) #stat_poly_eq()
library(ggpubr) #ggarrange()
# source(
#   here("analysis/disfit.R")
# ) #disfit()


#main----

#' Gather results of basic regression, assumption checks, & generalized versions
#'
#' @param ...nestedVarDataTbl
#' @param ...formula
#'
#' @return large tibble
#' @export
#' @import tidyverse
#' @examples
getStatsTbl <- function(
  #has"varData"col
  ...nestedVarDataTbl,
  #user's
  ...formula
) {

  ...nestedVarDataTbl %>%

    statFitTbl(...formula) %>%
    addStatEval(...formula) %>%

    #nonnormaldistr
    addStatFitNon(...formula) %>%
    addStatEvalNon() %>%

    #nonParametric,experimental
    addStatFitNonNP(...formula) %>%

    addGraph(...formula)
}




#parts----

#' Mutates lm and summary output onto tbl--needs $varData, $statTest
#'
#' @param ...nestedVarDataTbl
#' @param ....formula
#'
#' @return mutated tbl
#' @export
#' @import tidyverse
#' @examples
statFitTbl <- function(
  ...nestedVarDataTbl,
  ....formula
) {

  #checkLmerForm----
  ....formulaMod <- ....formula;
  ....formulaMod[[3]] <- ifelse(
    #>=3safe...
    test = length(....formula[[3]]) >= 3,
    yes = ....formula[[3]][[2]], # +
           #enterParentheses...
        # deparse(....formula[[3]][[3]][[2]][[3]]),
    no = ....formula[[3]]
  )


  #main----

  ...nestedVarDataTbl %>%

    mutate(
      "statTest" =
        varData %>%
        modify(
          ~ .x %>%
            lm(
              formula =
                ....formulaMod
            )
        ),
      "statPrint" =
        statTest %>%
        modify(
          ~ .x %>%
            tidy() %>%
            full_join(
              .,
              {.x %>% glance()}
            ) %>%
            filter(
              !is.na(
                term
              )
            )
        )
    )
}


#' Mutates lm assumptions eval onto tbl--needs $varData, $statTest, $statPrint
#'
#' @param ...statFitTbl
#' @param ....formula
#'
#' @return mutated tbl
#' @export
#' @import tidyverse
#' @examples
addStatEval <- function(
  ...statFitTbl,
  ....formula
) {

  ...statFitTbl %>%

    mutate(
      normalTest =
        statTest %>%
        modify(
          ~ .x %>%
            residuals() %>%
            shapiro_test()
        ),
      #OGdata
      varyTest =
        ifelse(
          #noLmerYet
          {
            ....formula[[3]] %>%
              length()
            #<3safer...
          } < 3,

          yes = {
            varData %>%
              modify(
                ~ .x %>%
                  levene_test(
                    formula =
                      ....formula[[2]] %>%
                      eval() ~
                      {
                        ....formula[[3]] %>%
                          eval() %>%
                          as_factor()
                      }
                  )
              )
          },

          no = list(
            tibble(
              "p" = NA
            )
          )
        ),
      "isNormal" =
        normalTest %>%
        modify(
          ~ if_else(
            .x$
              p.value >
              0.055,
            T, F
          )
        ),
#     )
# }
      "isHomosced" =
        varyTest %>%
        modify_if(
          !is.na(
              varyTest
          ),
          ~ if_else(
              .x$
                p >
                0.055,
              T, F
          ),
          ~ NA
        )
    ) %>%
    #list2vec
    unnest(
      c(
        isNormal,
        isHomosced
      )
    ) %>%

    mutate(
      "isModelOK" =
        {
          check <- ifelse(
            (
              isNormal &
                isHomosced
            ),
            T,
            F
          )

          #noNAarg4ifelse()...
          ifelse(
            !is.na(
              check
            ),
            check,
            ifelse(
              (
                isNormal &
                  is.na(
                    isHomosced
                  )
              ),
              T,
              F
            )
          )
        }
    ) %>%
    unnest(
      isModelOK
    ) %>%
    # } #de-bug
    mutate(
      "pval" =
        statPrint %>%
        modify_if(
          {
            isModelOK &
              !is.na(
                isModelOK
              )
          },
          ~ .x %>%
            filter(
              term != "(Intercept)"
            ) %>%
            pull(
              p.value
              #orOtherLabel
            ),
          .else = ~ NA
        ),
      "isSignif" =
        statPrint %>%
        modify_if(
          {
            isModelOK &
              !is.na(
                isModelOK
              )
          },
          ~ if_else(
            #mainterm
            .x$
              p.value[2] <
              0.1055,
            T, F
          ),
          .else = ~ NA
        )
    ) %>%
    unnest(
      isSignif
    ) %>%

    mutate(
      "R2adj" =
        #noLmerYet
        ifelse(
          {
            ....formula[[3]] %>%
              length()
          } == 1,
          {
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
          },

          list(
            tibble(
              "adj.r.squared" = NA
            )
          )
        )
    ) %>%
    unnest(
      R2adj
    )
}




#' Mutates geom_smooth lm plots onto tbl--needs $varData
#'
#' @param ...statEvalNonTbl
#' @param ....formula
#'
#' @return mutated tbl
#' @export
#' @import tidyverse
#' @examples
addGraph <- function(
  ...statEvalNonTbl,
  ....formula
) {

  ...statEvalNonTbl %>%

    mutate(
      graph =
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
