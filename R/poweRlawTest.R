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
#'
#' @return "results" data frame
#' @export
#' @import poweRlaw
#' @examples
CheckPoweRlaw <- function(
  RealFreq
) {  #freqvector
  #Clauset2009,Gillespie2015
  #bottleneck==boostrap()

  tic()
  nCores <- detectCores()


  RealFreq <- round(
    RealFreq
  )

  Pl <- displ$new(
    RealFreq
  )
  Pl$setPars(
    estimate_pars(
      Pl
    )
  )

  #bottleneck==bootstrap()
  PlVar <- var(
    poweRlaw::bootstrap(
      Pl,
      threads = nCores
    )$bootstraps$pars
  ) #variance
  #plot(Pl)

  PlP <- bootstrap_p(
    Pl,
    threads = nCores
  )$p  #>0.1passes


  Exp <- disexp$new(
    RealFreq
  )
  Exp$setPars(
    estimate_pars(
      Exp
    )
  )

  ExpVar <- var(
    poweRlaw::bootstrap(
      Exp,
      threads = nCores
    )$bootstraps$pars
  )


  Pois <- dispois$new(
    RealFreq
  )
  Pois$setPars(
    estimate_pars(
      Pois
    )
  )

  PoisVar <- var(
    poweRlaw::bootstrap(
      Pois,
      threads = nCores
    )$bootstraps$pars
  )


  Lognorm <- dislnorm$new(
    RealFreq
  )
  Lognorm$setPars(
    estimate_pars(
      Lognorm
    )
  )

  LognormVar <- poweRlaw::bootstrap(
    Lognorm,
    threads = nCores
  )$bootstraps
  LognormVar1 <- var(
    LognormVar$pars1
  )
  LognormVar2 <- var(
    LognormVar$pars2,
    na.rm = T
  )



  #null=bothOK #1sided=arg1==arg2
  PlExpP <- compare_distributions(
    Pl,
    Exp
  )$p_one_sided  #<0.05=arg1better
  PlLognormP <- compare_distributions(
    Pl,
    Lognorm
  )$p_one_sided
  PlPoisP <- compare_distributions(
    Pl,
    Pois
  )$p_one_sided



  results <- data.frame(
    "PlP" = PlP,
    "PlExpP" = PlExpP,
    "PlLognormP" = PlLognormP,
    "PlPoisP" = PlPoisP,

    "PlPar" = Pl$pars,
    "ExpPar" = Exp$pars,
    "PoisPar" = Pois$pars,
    "LognormPar1" = Lognorm$pars[1],
    "LognormPar2" = Lognorm$pars[2],

    "PlVar" = PlVar,
    "ExpVar" = ExpVar,
    "PoisVar" = PoisVar,
    "LognormVar1" = LognormVar1,
    "LognormVar2" = LognormVar2
  )

  toc()



  return(
    results
  )
}
