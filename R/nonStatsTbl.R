library(here)
i_am(
  "R/nonStatsTbl.R"
)



#nonnormalstats
#nestedmodifyishard

#' Mutates glm summary output onto tbl--needs $varData
#'
#' @param ...statEvalTbl
#' @param ....formula
#'
#' @return mutated tbl
#' @export
#' @import tidyverse
#' @examples
addStatFitNon <- function(
  ...statEvalTbl,
  ....formula
) {

  ...statEvalTbl %>%

    mutate(
      "statTestPois" =
        varData %>%
        modify_if(
          !isModelOK &
            !is.na(
              isModelOK
            ) &
            length(
              ....formula[[3]]
            ) == 1,
          ~ .x %>%
            glm(
              formula = ....formula,
              family = poisson()
            ) %>%
            summary(),
          #7digits2replaceNA
          ~ 9999999
        ),
      "statTestGamma" =
        varData %>%
        modify_if(
          !isModelOK &
            !is.na(
              isModelOK
            ) &
            length(
              ....formula[[3]]
            ) == 1,
          ~ .x %>%
            glm(
              formula = ....formula,
              family = Gamma(
                link = "log"
              )
            ) %>%
            summary(),
          #7digits2replaceNA
          ~ 9999999
        )
    )
}



#' Mutates key glm values onto tbl--needs $isModelOK, $statTestPois, $statTestGamma
#'
#' @param ...statFitNonTbl
#'
#' @return mutated tbl
#' @export
#' @import tidyverse
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
            ~ .x %>%
              as_tibble() %>%
              pull(
                aic
              ),
            ~ NA,
          ),
        gammaAIC =
          statTestGamma %>%
          modify_if(
            !isModelOK &
              !is.na(
                isModelOK
              ),
            ~ .x %>%
              as_tibble() %>%
              pull(
                aic
              ),
            ~ NA,
          ),
        poisPval =
          statTestPois %>%
          modify_if(
            !isModelOK &
              !is.na(
                isModelOK
              ),
            ~ .x %>%
              as_tibble() %>%
              pull(
                coefficients[8]
              ),
            ~ NA,
          ),
        gammaPval =
          statTestGamma %>%
          modify_if(
            !isModelOK &
              !is.na(
                isModelOK
              ),
            ~ .x %>%
              as_tibble() %>%
              pull(
                coefficients[8]
              ),
            ~ NA,
          )
      ) %>%

      unnest(
        c(
          poisAIC,
          gammaAIC,
          poisPval,
          gammaPval
        ),
        names_repair = "unique"
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
#' @import tidyverse
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
