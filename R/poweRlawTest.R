#steup----
here::i_am("R/poweRLawTest.R")
library(poweRlaw)
#replaces_broom::
#bootstrap::
# library(rsample)
library(parallel); library(tictoc)



#' Title
#'
#' @param dataVec
#'
#' @return 4 poweRlaw model objects, e.g. for direct hypothesis testing
#' @export
#'
#' @examples
makeDistObjs <- function(dataVec, xMin = NULL, xMax = 999999) {

  dataVec <- round(as.numeric(dataVec) + 2)


  #power----


  Pl <- poweRlaw::displ$new(dataVec)

  if(is.numeric(xMin)) {
    Pl$setXmin(xMin)
  } else {
    Pl$setXmin(
      #initial
      estimate_xmin(Pl,
                    #setXminsExplicitly2addressGillespie's1:(x - 1)uncertainty...
                    #assumesLeftCumsum()tail!=veryFlat...
                    # xmins = min(RealFreq):RealFreq[min(RealFreq + 2)],
                    xmax = xMax
      )
    )
  }

  Pl$setPars(estimate_pars(Pl))


  #exp----


  Exp <- disexp$new(dataVec)

  if(is.numeric(xMin)) {
    Exp$setXmin(xMin)
  } else { Exp$setXmin(estimate_xmin(Exp, xmax = xMax)) }

  Exp$setPars(estimate_pars(Exp))

  # ExpVar <- var(
  #   poweRlaw::bootstrap(
  #     Exp,
  #     xmins = Exp$getXmin(),
  #     xmax = xMax,
  #     threads = nCores, no_of_sims = nSims
  #   )$bootstraps$pars
  # )


  #pois----


  Pois <- dispois$new(dataVec)

  if(is.numeric(xMin)) {
    Pois$setXmin(xMin)
  } else { Pois$setXmin(estimate_xmin(Pois, xmax = xMax)) }

  Pois$setPars(estimate_pars(Pois))

  # PoisVar <- var(
  #   poweRlaw::bootstrap(
  #     Pois,
  #     xmins = Pois$getXmin(),
  #     xmax = xMax,
  #     threads = nCores, no_of_sims = nSims
  #   )$bootstraps$pars
  # )


  #lognorm----


  Lognorm <- dislnorm$new(dataVec)
  if(is.numeric(xMin)) {
    Lognorm$setXmin(xMin)
  } else { Lognorm$setXmin(estimate_xmin(Lognorm, xmax = xMax)) }

  Lognorm$setPars(estimate_pars(Lognorm))
  #
  #   LognormVar <- poweRlaw::bootstrap(
  #     Lognorm,
  #     xmins = Lognorm$getXmin(),
  #     xmax = xMax,
  #     threads = nCores, no_of_sims = nSims
  #   )$bootstraps
  #   LognormVar1 <- var(LognormVar$pars1)
  #   LognormVar2 <- var(LognormVar$pars2, na.rm = T)



  return(list(Pl, Exp, Pois, Lognorm))
}




#' Gather results of non-linear distribution fits to validate a power law fit
#'
#' @param RealFreq
#' @param xMin
#' @param xMax
#' @param nSims
#'
#' @return "results" data frame
#' @export
#' @import poweRlaw, tictoc
#' @examples
CheckPoweRlaw <- function(

  RealFreq,
  xMin = NULL, #=default4bootstrap_p()

  #poweRlaw::bootstrap()Default
  #<9999999,>slow5000sec
  xMax = 999999,
  # xMax = 100000,

  #poweRlaw::bootstrap()Default
  # nSims = 100
  nSims = 99

) {  #freqvector
  #Clauset2009,Gillespie2015
  #bottleneck==boostrap()

  tic()
  nCores <- parallel::detectCores()


  #na.omit()----


  if(
    any(is.na(RealFreq)) | is.null(RealFreq) |
    all(RealFreq == 1 | RealFreq == 0) |
    (length(RealFreq) == 0) | (length(RealFreq) < 3)
  ) {

    results <- data.frame(
      "PlP" = NA,
      "PlExpP" = NA, "PlLognormP" = NA, "PlPoisP" = NA,

      "PlPar" = NA,
      "ExpPar" = NA, "PoisPar" = NA,
      "LognormPar1" = NA, "LognormPar2" = NA

      # "PlVar" = PlVar,
      # "ExpVar" = ExpVar, "PoisVar" = PoisVar,
      # "LognormVar1" = LognormVar1, "LognormVar2" = LognormVar2
    )


    return(results)



  } else {
    # stopifnot(all(is.numeric(RealFreq)))
    RealFreq <-# if(!all(is.numeric(as_vector(RealFreq)))) {
      #addressPkg1:(x - 1)issue
      round(as.numeric(RealFreq) + 2)
    # } else { c(1, 1, 1, 1, 1) }



    #getDistObjs----
    distObjs <- makeDistObjs(dataVec = RealFreq, xMin = xMin, xMax = xMax)

    ##keepNames----
    Pl <- distObjs[[1]]; Exp <- distObjs[[2]];
    Pois <- distObjs[[3]]; Lognorm <- distObjs[[4]]



    #bottleneck==bootstrap()
    #key4n=1

    #variance
    # PlVar <- var(
    #   poweRlaw::bootstrap(
    #     Pl,
    #     xmins = Pl$getXmin(),
    #     xmax = xMax,
    #     threads = nCores, no_of_sims = nSims
    #   )$bootstraps$pars
    # )
    #plot(Pl)

    #mainTest----

    #BOTTLENECK!
    #alsoSlowBelow...
    # poweRlaw::bootstrap_p(poweRlaw::displ$new(99999999:999999999),
    #                       xmins = ((999999999 - 99999999) / ((99999999 + 1))))
    PlP <- poweRlaw::bootstrap_p(
      Pl,
      # xmins = seq(
      #   from = xMin, to = xMax
      #   # by = ((xMax - xMin) / ((xMin + 1)))
      # ),
      xmins = Pl$getXmin(),
      xmax = xMax,
      threads = nCores, no_of_sims = nSims
    )$p  #>0.1passes



    #compareDists----



    #dis1xmin==dis2xmin

    #null=bothOK #1sided=arg1==arg2
    Exp$setXmin(Pl$getXmin())
    PlExpP <- compare_distributions(Pl, Exp)$p_one_sided  #<0.05=arg1better

    Lognorm$setXmin(Pl$getXmin())
    PlLognormP <- compare_distributions(Pl, Lognorm)$p_one_sided

    Pois$setXmin(Pl$getXmin())
    PlPoisP <- compare_distributions(Pl, Pois)$p_one_sided



    #org----



    #pivot?==majorBreak!
    results <- data.frame(
      "PlP" = PlP,
      "PlExpP" = PlExpP, "PlLognormP" = PlLognormP, "PlPoisP" = PlPoisP,

      "PlPar" = Pl$pars,
      "ExpPar" = Exp$pars, "PoisPar" = Pois$pars,
      "LognormPar1" = Lognorm$pars[1], "LognormPar2" = Lognorm$pars[2]

      # "PlVar" = PlVar,
      # "ExpVar" = ExpVar, "PoisVar" = PoisVar,
      # "LognormVar1" = LognormVar1, "LognormVar2" = LognormVar2
    )

    toc()
  }



  #return----



  return(results)
}
