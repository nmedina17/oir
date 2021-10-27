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
        modify_if(
          varData,
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
          #replaceNA
          ~ 9999999 %>%
            as_tibble_col()
        ),
      "statTestGamma" =
        modify_if(
          varData,
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
          #replaceNA
          ~ 9999999 %>%
            as_tibble_col()
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
            ~ 2 %>%
              as_tibble_col(),
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
            ~ 2 %>%
              as_tibble_col(),
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
            ~ 2 %>%
              as_tibble_col(),
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
            ~ 2 %>%
              as_tibble_col()
          )
      ) %>%

      unnest(
        cols = c(
          poisAIC,
          gammaAIC,
          poisPval,
          gammaPval
        ),
        #names_repair = "unique"
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
