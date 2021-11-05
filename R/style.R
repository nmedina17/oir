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
library(ggpmisc)
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
#' @param ..cleanData
#' @param ..addBins
#' @param ..addLines
#' @param ..log10Axes
#' @param ..useGroups
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
  ..addBins = F,
  ..cleanData = NULL,
  ..addLines = F,
  ..log10Axes = F,
  ..useGroups = NULL
) {

  theme_set(
    style
  )


  if(
    !is.null(
      ..useGroups
    )
  ) {

    ..varData <- ..varData %>%
      modify(
        ~ .x %>%
          group_by(
            ..useGroups
          )
      )
  }


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
              0.1055,
            "> 0.10",
            ifelse(
              showP < 0.1055 &
                showP > 0.001,
              round(
                showP,
                3
              ),
              "< 0.001"
            )
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
    ..addBins == T
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


  graph <- if(
    ..log10Axes == T
  ) {

    graph <- graph +

      scale_y_continuous(
        trans = "log10"
      ) +
      scale_x_continuous(
        trans = "log10"
      )

  } else {

    graph
  }


  graph <- if(
    ..addLines == T
  ) {

    graph <- graph +

      stat_smooth(
        se = F,
        color = "black",
        size = 1,
        method = "lm"
      )
  } else {

    graph
  }


}
