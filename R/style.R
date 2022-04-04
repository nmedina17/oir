#refs----


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



#style----


fontSizeMin = 6
# fontFamily = Helvetica

style <- theme_bw() +
  theme(
    axis.text = element_text(
      size = fontSizeMin
    ),
    axis.title = element_text(
      size = fontSizeMin + 1
    ),
    strip.background = element_rect(
      fill = "white"
    )
  )
#dark?



#pcut----
cutoff <- 0.125



#main----



#' Sets cleaner ggplot style, scatter default
#'
#' @param ..varData
#' @param ..var string vec
#' @param ..x quoted col
#' @param ..y quoted col
#' @param ..xlab quoted string
#' @param ..ylab quoted string
#' @param ..cleanData
#' @param ..addBins
#' @param ..addLines
#' @param ..addCurve
#' @param ..log10Axes
#' @param ..useGroups quoted col
#' @param ..groupTitle
#' @param ..addCenters
#' @param ..addP
#' @param ..addPxy short vector
#' @param ..addPsize integer font
#' @param ..dark
#'
#' @return scatter ggplot
#' @export
#' @import tidyverse, ggpmisc
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
  ..xlab = NULL,
  ..ylab = NULL,
  ..addBins = F,
  ..cleanData = NULL,
  ..addLines = F,
  ..addCurve = F,
  ..log10Axes = F,
  ..useGroups = NULL,
  ..groupTitle = NULL,
  ..addCenters = F,
  ..addP = T,
  ..addPxy = c(
    1, 1.1
  ),
  ..addPsize = 2,
  ..dark = F
) {

  theme_set(
    style
  )


  graphData <- ..varData %>%

    filter(
      variable == ..var
        # variable == ..var[2] |
        # variable == ..var[3] |
        # variable == ..var[4] |
        # variable == ..var[5]
    ) %>%
    select(
      "varData"
    ) %>%
    unnest(
      everything()
    )

  # useGroups----
  graphData <- if(
    !is_null(
      ..useGroups
    )
  ) {

    graphData %>%
      group_by(
        {
          ..useGroups %>%
            eval()
        }
      )
  } else {

    graphData
  }



  graph <- if(
    !is_null(
      ..useGroups
    )
  ) {
    #repeat4sameLine
      graphData %>%

        ggplot(
          aes(
            x = {
              ..x %>%
                eval()
            },
            y = {
              ..y %>%
                eval()
            },

            color = {
              ..useGroups %>%
                eval()
            }
          )
        ) +
      labs(
        color = {
          ..groupTitle %>%
            eval()
        }
      ) +
      theme(
        legend.position = "top"
      )

    } else {

      graphData %>%

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
        )

    }


  graph <- graph +

    ggbeeswarm::geom_quasirandom(
      # color = "black",
      shape = 21,
      fill = "white",
      size = 2

      #conditionalNeed?
      # groupOnX = T

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
    )

  # addP----
  graph <- if(
    ..addP == T
  ) {

    graph <- graph +

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

              yes = {
                varResult %>%
                pull(
                  "pval"
                )
              },

              no = {
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
                cutoff,
              "> 0.10",
              ifelse(
                showP < cutoff &
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
        x = ..addPxy[1],
        y = ..addPxy[2],
        hjust = -0.05,
        size = ..addPsize
      )

  } else {
    graph
  }


  #toggles


  # cleanData----
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
          size = 0.5
        ),

        position = "bottom"
      )
  }


  # addBins----
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
        size = 1
      )

  } else {

    graph
  }


  # log10Axes----
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


  # addCurve----
  graph <- if(
    ..addCurve == T
  ) {

    curve <- y ~
      poly(
        x,
        2
      )

    graph <- graph +

      stat_smooth(
        formula = curve,
        se = F,
        color = "black",
        size = 0.5,
        method = "lm"
      ) +
      ggpmisc::stat_poly_eq(
        formula = curve,
        size = 2,
        label.x = "right",
        label.y = 1.1
      )
  } else {

    graph
  }


  # addLines----
  graph <- if(
    ..addLines == T
  ) {

    graph <- graph +

      stat_smooth(
        # formula = curve,
        se = F,
        # color = "black",
        size = 0.5,
        method = "lm",
        # parse = T
      ) +
      ggpmisc::stat_poly_eq(
        size = 2,
        label.x = "right",
        # label.y = 1.1,
        label.y = "top",
        parse = T
      )
  } else {

    graph
  }


  # addCenters----
  graph <- if(
    ..addCenters == T
  ) {

    graph <- graph +

      stat_summary(
        fun.data = "median_mad",
        geom = "pointrange",
        size = 0.25,
        color = if(
          ..dark == T
        ) {
          "white"
        } else {
          "black"
        }
      )
  } else {

    graph
  }


  # dark----
  if(
    ..dark == T
  ) {

    graph +
      theme(
        plot.background = element_rect(
          fill = "black"
        ),
        panel.background = element_rect(
          fill = "black"
        ),
        text = element_text(
          color = "white"
        ),
        legend.background = element_rect(
          fill = "black"
        ),
        legend.key = element_rect(
          fill = "black"
        ),
        panel.grid = element_blank(),
        axis.text = element_text(
          color = "white"
        )
        # panel.grid = element_line(
        #   color = "gray"
        # ),
        # strip.background = element_rect(
        #   fill = "black"
        # )

      )
  } else {

    graph +

      #shrinkleg----
      theme(
        #allsmaller
        legend.text = element_text(
          size = 6
        ),
        legend.key.size = unit(
          0.2, "cm"
        ),
        legend.text.align = 0,
        legend.key.width = unit(
          0.2, "cm"
        ),
        legend.title = element_text(
          size = 6
        ),
        legend.margin = margin(
          l = -5,
          t = -5,
          b = -5,
          r = -5
        )
      )
  }


}
