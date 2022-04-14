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
        .else = ~ tibble(
          "aic" = 9999999,
          "coefficients" = 999
        )
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
        .else = ~ tibble(
          "aic" = 9999999,
          "coefficients" = 999
        )
      )
    # .keep = "used",

    # "glmerPois" = varData %>%
    #   modify_if(
    #     {
    #       {
    #         ....formula[[3]] %>%
    #           length()
    #       } > 1
    #     } &
    #       {
    #         !isModelOK &
    #           !is.na(
    #             isModelOK
    #           )
    #       },
    #     ~ .x %>%
    #       glmer(
    #         formula = ....formula,
    #         family = poisson(
    #           link = "identity"
    #         ),
    #         control = glmerControl(
    #           "bobyqa"
    #         )
    #       ) %>%
    #       summary(),
    #     .else = ~ tibble(
    #       "AICtab" = 9999999,
    #       "coefficients" = 999
    #     )
    #   ),
    # "glmerGamma" = varData %>%
    #   modify_if(
    #     {
    #       {
    #         ....formula[[3]] %>%
    #           length()
    #       } > 1
    #     } &
    #       {
    #         !isModelOK &
    #         !is.na(
    #           isModelOK
    #         )
    #       },
    #     ~ .x %>%
    #       glmer(
    #         formula = ....formula,
    #         family = Gamma(
    #           link = "identity"
    #         ),
    #         control = glmerControl(
    #           c(
    #             "bobyqa",
    #             # "Nelder_Mead"
    #           )
    #         )
    #       ) %>%
    #       summary(),
    #     .else = ~ tibble(
    #       "AICtab" = 9999999,
    #       "coefficients" = 999
    #     )
    #   )
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
            ~ .x$
              aic,
            .else = ~ 9999999,
          ),
        gammaAIC =
          statTestGamma %>%
          modify_if(
            !isModelOK &
              !is.na(
                isModelOK
              ),
            ~ .x$
              aic,
            .else = ~ 9999999,
          ),
        poisPval =
          statTestPois %>%
          modify_if(
            !isModelOK &
              !is.na(
                isModelOK
              ),
            ~ .x$
              coefficients %>%
              #only1fixedVar!
              #[8]
              last(),
            .else = ~ 2,
          ),
        gammaPval =
          statTestGamma %>%
          modify_if(
            !isModelOK &
              !is.na(
                isModelOK
              ),
            ~ .x$
              coefficients %>%
              #only1fixedVar!
              #[8]
              last(),
            .else = ~ 2
          )
      ) %>%

      unnest(
        c(
          poisAIC,
          gammaAIC,
          poisPval,
          gammaPval
        )
        # names_repair = "universal"
      ) %>%

      #eval
      mutate(
        "pickAIC" =
          pmin(
            poisAIC,
            gammaAIC,
            na.rm = T
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
              0.125,
            T, F
          )
      )


    # mutate(
    #   gPoisAIC = glmerPois %>%
    #     modify_if(
    #       {
    #         {
    #           ....formula[[3]] %>%
    #             length()
    #         } > 1
    #       } &
    #         {
    #           !isModelOK &
    #           !is.na(
    #             isModelOK
    #           )
    #         },
    #       ~ .x$
    #         AICtab["AIC"],
    #       .else = ~ 9999999
    #     ),
    #   gGammaAIC = glmerGamma %>%
    #     modify_if(
    #       {
    #         {
    #           ....formula[[3]] %>%
    #             length()
    #         } > 1
    #       } &
    #         {
    #           !isModelOK &
    #           !is.na(
    #             isModelOK
    #           )
    #         },
    #       ~ .x$
    #         AICtab["AIC"],
    #       .else = ~ 9999999
    #     ),
    #   gPoisPval = glmerPois %>%
    #     modify_if(
    #       {
    #         {
    #           ....formula[[3]] %>%
    #             length()
    #         } > 1
    #       } &
    #         {
    #           !isModelOK &
    #           !is.na(
    #             isModelOK
    #           )
    #         },
    #       ~ .x$
    #         coefficients %>%
    #         #only1fixedVar!
    #         last(),
    #       .else = ~ 2
    #     ),
    #   gGammaPval = glmerPois %>%
    #     modify_if(
    #       {
    #         {
    #           ....formula[[3]] %>%
    #             length()
    #         } > 1
    #       } &
    #         {
    #           !isModelOK &
    #           !is.na(
    #             isModelOK
    #           )
    #         },
    #       ~ .x$
    #         coefficients %>%
    #         #only1fixedVar!
    #         last(),
    #       .else = ~ 2
    #     )
    # ) %>%
    #
    # unnest(
    #   c(
    #     gPoisAIC,
    #     gGammaAIC,
    #     gPoisPval,
    #     gGammaPval
    #   )
    #   # names_repair = "universal"
    # ) %>%
    #
    # #eval
    # mutate(
    #   "pickgAIC" =
    #     pmin(
    #       gPoisAIC,
    #       gGammaAIC,
    #       na.rm = T
    #     )
    # ) %>%
    # unnest(
    #   pickgAIC
    # ) %>%
    # mutate(
    #   #morefamilies?
    #   "pickgPval" =
    #     ifelse(
    #       pickgAIC == gPoisAIC,
    #       gPoisPval,
    #       if_else(
    #         pickgAIC == gGammaAIC,
    #         gGammaPval,
    #         9
    #       )
    #     ),
    #   "isSignifg9" =
    #     if_else(
    #       pickgPval <
    #         0.125,
    #       T, F
    #     )
    # )
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
      "statTestNP" =
        varData %>%
        modify_if(

          !isModelOK &
            !is.na(
              isModelOK
            ),

          ~ na.omit(
            .x
          ) %>%
            as.data.frame() %>%
            as.matrix.data.frame() %>%
            kruskal_test(
              ....formula
            ),

          ~ NA
        ) %>%
        summary(),
      "NPp" = statTestNP %>%
        pull(
          p
        )
    ) #%>%
    # unnest(
    #   NPp
    # )
}
