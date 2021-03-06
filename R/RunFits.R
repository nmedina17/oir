library(
  here
)
i_am(
  "R/RunFits.R"
)
#CheckPowerLaw()
# source(
#   here(
#     "scripts/poweRlawTest.R"
#   )
# )
library(
  poweRlaw
)
library(
  tictoc
)

#parallel--CheckPowerLaw()
library(
  furrr
)
library(
  doParallel
)



#' Runs distribution fits through sample list
#'
#' @param ...nestedTbl
#'
#' @return tbl
#' @export
#' @import tidyverse, furrr, doParallel, future
#' @examples
runFits <- function(
  ...nestedTbl
) {

  cleanNest <- ...nestedTbl


  plan(
    "multisession"
  )


  cluster1 <- makeCluster(
    detectCores()
  )
  cluster1 %>%
    registerDoParallel()


  tic()

  fits <- cleanNest %>%
    mutate(
      "statFit" = varData %>%
        modify(
          #local
          ~ oir::CheckPoweRlaw(
            .x %>%
              pull(
                g
              ) + 2
            #avoidLog0,1
            #fxAbs,keepsRelPattern
          )
        )
    )

  toc()


  stopCluster(
    cluster1
  )


  #cantWriteCSVnestedTbl
  return(
    fits
  )
}
