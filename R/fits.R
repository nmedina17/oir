library(
  here
)
i_am(
  "R/fits.R"
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
  "tictoc"
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
#' @import tidyverse
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
