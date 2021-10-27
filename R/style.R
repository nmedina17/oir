library(tidyverse)
#geom_quasirandom()
library(ggbeeswarm)
library(here)
i_am(
  "R/style.R"
)
# source(
#   here(
#     "analysis/statsTbl.R"
#   )
# )
library(gginnards)
#append_layers()
library(glue)


style <- theme_bw() +
  theme()
#dark?



#' Sets cleaner ggplot style, scatter default
#'
#' @param ..varData
#' @param ..var
#' @param ..x
#' @param ..y
#' @param ..xlab
#' @param ..ylab
#' @param ..addGroups
#' @param ..cleanData
#'
#' @return scatter ggplot
#' @export
#' @import tidyverse
#' @examples
dotGraph <- function(
  ..varData,
  # $variable
  # $varData
  # $varData1
  # $pval
  # $pickPval
  ..var,
  ..x,
  ..y,
  ..xlab,
  ..ylab,
  ..addGroups = F,
  ..cleanData = NULL
) {

  theme_set(
    style
  )


  graph <- ..varData %>%

    filter(
      variable == ..var
    ) %>%
    select(
      "varData"
    ) %>%
    unnest(
      everything()
    ) %>%


    ggplot(
      aes(
        x = {
          ..x %>%
            eval()
        },
        y = {
          ..y %>%
            eval()
        }
      )
    ) +

    geom_quasirandom(
      color = "black",
      shape = 21,
      fill = "white",
      size = 2
    ) +

    labs(
      x = {
        ..xlab %>%
          eval()
        },
      y = {
        ..ylab %>%
          eval()
      }
    ) +

    annotate(
      "text",
      label = glue(
        "P = ",
        {
          varResult <- ..varData %>%
            filter(
              variable == ..var
            )

          checkResult <- varResult %>%
            pull(
              isModelOK
            )

          #if_else()2strict
          showP <- ifelse(
            checkResult &
              !is.na(
                checkResult
              ),
            {
              varResult %>%
              pull(
                "pval"
              )
            },
            {
              varResult %>%
              pull(
                "pickPval"
              )
            }
          ) %>%
            last() %>%
            as.double()

          ifelse(
            showP >
              0.001,
            round(
              showP,
              3
            ),
            "< 0.001"
          )
        }
      ),
      x = 1,
      y = 1,
      hjust = 0
    )


  #toggles


  graph <- if(
    is.null(
      ..cleanData
    )
  ) {

    graph

  } else {

    graph <- graph %>%

      #move
      append_layers(

        geom_quasirandom(
          data = ..cleanData,
          aes(
            y = {
              ..var %>%
                eval()
            }
          ),
          color = "gray",
          size = 1
        ),

        position = "bottom"
      )
  }


  graph <- if(
    ..addGroups == T
  ) {

    graph <- graph +

      geom_point(
        data = {
          ..varData %>%
            filter(
              variable == ..var
            ) %>%
            select(
              "varData1"
            ) %>%
            unnest(
              everything()
            )
        },
        color = "black",
        size = 3
      )

  } else {

    graph
  }
}
