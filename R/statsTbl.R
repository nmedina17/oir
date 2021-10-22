library(here);
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


#main

#' Gather results of basic regression, assumption checks, & generalized versions
#'
#' @param ...nestedVarDataTbl
#' @param ...formula
#'
#' @return large tibble
#' @export
#'
#' @examples
getStatsTbl <- function(
  #has"varData"col
  ...nestedVarDataTbl,
  #user's
  ...formula
) {

  ...nestedVarDataTbl %>%

    statFitTbl(
      ...formula
    ) %>%
    addStatEval(
      ...formula
    ) %>%

    #nonnormaldistr
    addStatFitNon(
      ...formula
    ) %>%
    addStatEvalNon() %>%

    addGraph(
      ...formula
    )
}




#sub-funcs

#' Mutates lm and summary output onto tbl--needs $varData, $statTest
#'
#' @param ...nestedVarDataTbl
#' @param ....formula
#'
#' @return mutated tbl
#' @export
#'
#' @examples
statFitTbl <- function(
  ...nestedVarDataTbl,
  ....formula
) {

  ...nestedVarDataTbl %>%

    mutate(
      "statTest" =
        varData %>%
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


#' Mutates lm assumptions eval onto tbl--needs $varData, $statTest, $statPrint
#'
#' @param ...statFitTbl
#' @param ....formula
#'
#' @return mutated tbl
#' @export
#'
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
          } == 1,
          {
            varData %>%
              modify(
                ~ .x %>%
                  levene_test(
                    formula =
                      ....formula[[2]] %>%
                      eval() ~
                      ....formula[[3]] %>%
                      eval() %>%
                      as_factor()
                  )
              )
          },
          list(
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
        if_else(
          isNormal &
            isHomosced,
          T, F
        )
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



#nonnormalstats
#nestedmodifyishard

#' Mutates glm summary output onto tbl--needs $varData
#'
#' @param ...statEvalTbl
#' @param ....formula
#'
#' @return mutated tbl
#' @export
#'
#' @examples
addStatFitNon <- function(
  ...statEvalTbl,
  ....formula
) {

  if_else(
      {
        ....formula[[3]] %>%
          length()
      } == 1,
      {
        ...statEvalTbl %>%

          mutate(
            "statTestPois" =
              varData %>%
              modify_if(
                !isModelOK &
                  !is.na(
                    isModelOK
                  ),
                ~ .x %>%
                  glm(
                    formula = ....formula,
                    family = poisson()
                  ) %>%
                  summary(),
                .else = ~ NA
              ),
            "statTestGamma" =
              varData %>%
              modify_if(
                !isModelOK &
                  !is.na(
                    isModelOK
                  ),
                ~ .x %>%
                  glm(
                    formula = ....formula,
                    family = Gamma(
                      link = "log"
                    )
                  ) %>%
                  summary(),
                .else = ~ NA
              )
          )
      },
      {
        ...statEvalTbl
      }
    )
}



#' Mutates key glm values onto tbl--needs $isModelOK, $statTestPois, $statTestGamma
#'
#' @param ...statFitNonTbl
#'
#' @return mutated tbl
#' @export
#'
#' @examples
addStatEvalNon <- function(
  ...statFitNonTbl
) {

  ...statFitNonTbl %>%

    #getvals
    mutate(
      poisAIC =
        statTestPois %>%
        modify_if(
          !isModelOK &
            !is.na(
              isModelOK
            ),
          ~ .x$
            aic
        ),
      gammaAIC =
        statTestGamma %>%
        modify_if(
          !isModelOK &
            !is.na(
              isModelOK
            ),
          ~ .x$
            aic
        ),
      poisPval =
        statTestPois %>%
        modify_if(
          !isModelOK &
            !is.na(
              isModelOK
            ),
          ~ .x$
            coefficients[8]
        ),
      gammaPval =
        statTestGamma %>%
        modify_if(
          !isModelOK &
            !is.na(
              isModelOK
            ),
          ~ .x$
            coefficients[8]
        )
    ) %>%
    unnest(
      c(
        poisAIC,
        gammaAIC,
        poisPval,
        gammaPval
      )
    ) %>%

    #eval
    mutate(
      "pickAIC" =
        pmin(
          poisAIC,
          gammaAIC
        )
    ) %>%
    unnest(
      pickAIC
    ) %>%
    mutate(
      #morefamilies?
      "pickPval" =
        ifelse(
          pickAIC == poisAIC,
          poisPval,
          if_else(
            pickAIC == gammaAIC,
            gammaPval,
            9
          )
        ),
      "isSignif9" =
        if_else(
          pickPval <
            0.105,
          T, F
        )
    )
}



#' Mutates kruskal_test summary output onto tbl--needs $varData, $isModelOK
#'
#' @param ...statEvalTbl
#' @param ....formula
#'
#' @return mutated tbl
#' @export
#'
#' @examples
addStatFitNonNP <- function(
  ...statEvalTbl,
  ....formula
) {

  ...statEvalTbl %>%

    mutate(
      statTestNP =
        varData %>%
        modify_if(

          !isModelOK &
            !is.na(
              isModelOK
            ),

          ~ .x %>%
            kruskal_test(
              ....formula
            )
        ) %>%
        summary()
    )
}



#' Mutates geom_smooth lm plots onto tbl--needs $varData
#'
#' @param ...statEvalNonTbl
#' @param ....formula
#'
#' @return mutated tbl
#' @export
#'
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
