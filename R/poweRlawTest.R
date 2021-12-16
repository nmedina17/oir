library(here)
i_am(
  "R/poweRLawTest.R"
)
library(poweRlaw)
#replaces_broom::
#bootstrap::
# library(rsample)
library(parallel)
library(tictoc)



#' Gather results of non-linear distribution fits to validate a power law fit
#'
#' @param RealFreq
#' @param xMin
#' @param nSims
#'
#' @return "results" data frame
#' @export
#' @import poweRlaw, tictoc
#' @examples
CheckPoweRlaw <- function(
  RealFreq,
  xMin = NULL,
  #poweRlaw::bootstrap()Default
  xMax = 100000,
  #poweRlaw::bootstrap()Default
  nSims = 100
) {  #freqvector
  #Clauset2009,Gillespie2015
  #bottleneck==boostrap()

  tic()
  nCores <- detectCores()


  if(
    any(
      is.na(
        RealFreq
      ) |
      all(
        RealFreq == 1
      )
    )
  ) {

    results <- data.frame(
      "PlP" = NA,
      "PlExpP" = NA,
      "PlLognormP" = NA,
      "PlPoisP" = NA,

      "PlPar" = NA,
      "ExpPar" = NA,
      "PoisPar" = NA,
      "LognormPar1" = NA,
      "LognormPar2" = NA

      # "PlVar" = PlVar,
      # "ExpVar" = ExpVar,
      # "PoisVar" = PoisVar,
      # "LognormVar1" = LognormVar1,
      # "LognormVar2" = LognormVar2
    )


    return(
      results
    )



  } else {
    # stopifnot(
    #   all(
    #     is.numeric(
    #       RealFreq
    #     )
    #   )
    # )
    RealFreq <-# if(
    #   !all(
    #     is.numeric(
    #       as_vector(
    #         RealFreq
    #       )
    #     )
    #   )
    # ) {
      round(
        RealFreq
      )
    # } else {
    #   c(
    #     1, 1, 1, 1, 1
    #   )
    # }



    Pl <- displ$new(
      RealFreq
    )
    if(
      is.numeric(
        xMin
      )
    ) {

      Pl$setXmin(
        xMin
      )
    } else {

      Pl$setXmin(
        #initial
        estimate_xmin(
          Pl
        )
      )
    }

    Pl$setPars(
      estimate_pars(
        Pl
      )
    )

    #bottleneck==bootstrap()
    #key4n=1

    #variance
    # PlVar <- var(
    #   poweRlaw::bootstrap(
    #     Pl,
    #     xmins = Pl$getXmin(),
    #     xmax = xMax,
    #     threads = nCores,
    #     no_of_sims = nSims
    #   )$bootstraps$pars
    # )
    #plot(Pl)

    PlP <- bootstrap_p(
      Pl,
      xmins = xMin,,
      xmax = xMax,
      threads = nCores,
      no_of_sims = nSims
    )$p  #>0.1passes


    Exp <- disexp$new(
      RealFreq
    )
    if(
      is.numeric(
        xMin
      )
    ) {

      Exp$setXmin(
        xMin
      )
    } else {

      Exp$setXmin(
        estimate_xmin(
          Exp
        )
      )
    }

    Exp$setPars(
      estimate_pars(
        Exp
      )
    )

    # ExpVar <- var(
    #   poweRlaw::bootstrap(
    #     Exp,
    #     xmins = Exp$getXmin(),
    #     xmax = xMax,
    #     threads = nCores,
    #     no_of_sims = nSims
    #   )$bootstraps$pars
    # )


    Pois <- dispois$new(
      RealFreq
    )
    if(
      is.numeric(
        xMin
      )
    ) {

      Pois$setXmin(
        xMin
      )
    } else {
      Pois$setXmin(
        estimate_xmin(
          Pois
        )
      )
    }

    Pois$setPars(
      estimate_pars(
        Pois
      )
    )

    # PoisVar <- var(
    #   poweRlaw::bootstrap(
    #     Pois,
    #     xmins = Pois$getXmin(),
    #     xmax = xMax,
    #     threads = nCores,
    #     no_of_sims = nSims
    #   )$bootstraps$pars
    # )


    Lognorm <- dislnorm$new(
      RealFreq
    )
    if(
      is.numeric(
        xMin
      )
    ) {

      Lognorm$setXmin(
        xMin
      )
    } else {

      Lognorm$setXmin(
        estimate_xmin(
          Lognorm
        )
      )
    }

    Lognorm$setPars(
      estimate_pars(
        Lognorm
      )
    )
  #
  #   LognormVar <- poweRlaw::bootstrap(
  #     Lognorm,
  #     xmins = Lognorm$getXmin(),
  #     xmax = xMax,
  #     threads = nCores,
  #     no_of_sims = nSims
  #   )$bootstraps
  #   LognormVar1 <- var(
  #     LognormVar$pars1
  #   )
  #   LognormVar2 <- var(
  #     LognormVar$pars2,
  #     na.rm = T
  #   )



    #dis1xmin==dis2xmin

    #null=bothOK #1sided=arg1==arg2
    Exp$setXmin(
      Pl$getXmin()
    )
    PlExpP <- compare_distributions(
      Pl,
      Exp
    )$p_one_sided  #<0.05=arg1better

    Lognorm$setXmin(
      Pl$getXmin()
    )
    PlLognormP <- compare_distributions(
      Pl,
      Lognorm
    )$p_one_sided

    Pois$setXmin(
      Pl$getXmin()
    )
    PlPoisP <- compare_distributions(
      Pl,
      Pois
    )$p_one_sided



    #pivot?==majorBreak!
    results <- data.frame(
      "PlP" = PlP,
      "PlExpP" = PlExpP,
      "PlLognormP" = PlLognormP,
      "PlPoisP" = PlPoisP,

      "PlPar" = Pl$pars,
      "ExpPar" = Exp$pars,
      "PoisPar" = Pois$pars,
      "LognormPar1" = Lognorm$pars[1],
      "LognormPar2" = Lognorm$pars[2]

      # "PlVar" = PlVar,
      # "ExpVar" = ExpVar,
      # "PoisVar" = PoisVar,
      # "LognormVar1" = LognormVar1,
      # "LognormVar2" = LognormVar2
    )

    toc()
  }


  return(
    results
  )
}
