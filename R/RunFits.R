here::i_am("R/RunFits.R")
#CheckPowerLaw()
# source(here("scripts/poweRlawTest.R"))
library(poweRlaw)
library(tictoc)

#parallel--CheckPowerLaw()
library(furrr); library(doParallel); library(future)



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
  # ...varData = quote(varData)
) {

  cleanNest <- ...nestedTbl


  future::plan("multisession") #inert


  cluster1 <- parallel::makeCluster(
    parallel::detectCores()
  )
  cluster1 %>% doParallel::registerDoParallel()


  tic()

  fits <- cleanNest %>%
    mutate(
      "statFit" = varData %>% #eval() %>%
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


  parallel::stopCluster(cluster1)


  #cantWriteCSVnestedTbl
  return(fits)
}
