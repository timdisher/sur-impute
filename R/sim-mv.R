#' .sim.mv
#'
#' simulate multivariate NMA data following JAGS model being used to evaluate it
#'
#' Simulate multivariate NMA data following the JAGS model being used to evaluate
#' it. This will be used to confirm that the model is working as intended
#' and converging to posterior in reasonable amount of time. This will be intended
#' to work with any arbitrary network and thus allow testing that the model
#' can be expected to work in any given dataset.
#'
#' @param out.dat a dataframe with variables y1, y2, se1, se2, t1, t2. y1 and y2
#'   should be expressed as treatment differences on a linear scale (eg log odds)
#'   for binomial outcomes or log hazards for hazard ratios. se1 and se2 should
#'   be the standard error for the difference, and t1 and t2 numeric indicators
#'   for the treatments in the baseline and comparator group.
#' @param brma if true sets the surrogate relationship to be the same for
#'   for all treatment contrasts thus allowing a standard BRMA to be fit.
#' @return  The function takes a data frame with the anticipated design
#'   characteristics and simulates data that would be consistent with the
#'   Daniels and Hughes surrogate model . This allows
#'   for simple testing that the model is working as intended with a realistic
#'   dataset.
#'
#' @examples
#'  Put some examples of how it works or just delete this
#'
#' @export
.sim.mv <- function(out.dat,
                    trt.dict,
                    brma = FALSE){

  ############################################################################## #
  ############################################################################## #
  #
  # 1. Error catch ----
  #
  #     Section Notes
  #
  ############################################################################## #
  ############################################################################## #
  test.out.dat <- all(c("StudyName",
    "y1",
    "y2",
    "se1",
    "se2",
    "t1",
    "t2") %in% colnames(out.dat))

  if(!test.out.dat){
    stop("Out.dat should have colnames studyid, y1, y2, se1, se2, t1, and t2")
  }

  ############################################################################## #
  ############################################################################## #
  #
  # 2. Create required data list ----
  #
  #     Section Notes
  #
  ############################################################################## #
  ############################################################################## #
  dat.list <- .prep.brma.dat(out.dat, trt.dict)
  NS <- dat.list$NS
  t <- dat.list$t
  NT <- dat.list$NT
  se <- dat.list$se

  rho_w <- runif(1, 0, 0.999)
  psi.2 <- runif(1, 0, 2)

  lambda20 <- lambda21 <- matrix(NA, nrow = NT-1,
         ncol = NT)


  if(brma){
    B20 <- rnorm(1, 0, 0.11^-0.5)
    B21 <-  rnorm(1, 0, 0.11^-0.5)


    for(c in 1:(NT - 1)){
      for(k in (c+1):NT){

         lambda20[c,k] <- B20
        lambda21[c,k] <- B21


      }
    }

  } else{
    for(c in 1:(NT - 1)){
      for(k in (c+1):NT){


        lambda20[c,k] <- rnorm(1, 0, 0.11^-0.5)
        lambda21[c,k] <- rnorm(1, 0, 0.11^-0.5)

      }
    }
  }




  delta <- matrix(NA, nrow = NS, ncol = 2)

  for(i in 1:NS){
    delta[i,1] <- rnorm(1, 0, 0.11^-0.5)
  eta2 <- lambda20[t[i,1], t[i,2]] + lambda21[t[i,1], t[i,2]]*delta[i,1]
  delta[i,2] <- rnorm(1, eta2, psi.2)
  }


  sigma <- array(NA, dim = c(2,2, NS)) # Note we can't force the same array dimension indexing as JAGS
  Prec_w <- array(NA, dim = c(2,2, NS))
  y <- matrix(NA, nrow = NS, ncol = 2)
  se[,1] <- abs(rnorm(NS))
  se[,2] <- abs(rnorm(NS, se[,1]*0.8, 0.5))


  for(i in 1:NS){


  sigma[1,1,i] <- se[i,1]^2
  sigma[2,2,i] <- se[i,2]^2
  sigma[1,2,i] <- sqrt(sigma[1,1,i])*sqrt(sigma[2,2,i])*rho_w
  sigma[2,1,i] <- sqrt(sigma[1,1,i])*sqrt(sigma[2,2,i])*rho_w

  y[i,] <- MASS::mvrnorm(1, mu = c(delta[i,]), Sigma = sigma[1:2,1:2,i])

  }


  dplyr::lst(
    y,
    se,
    t,
    NS,
    NT,
    rho_w,
    psi.2,
    lambda20, # surrogate intercept
    lambda21 # surrogate slope
  )
}

#' .prep.brma.dat
#'
#' Prepare wide contrast format data for use with brma code
#'
#' Generate the required data list for use with brma code
#'

#' @param  dat contrast data frame with columns: StudyName, t1, t2, y1, y2,
#'   se1,se2 where t1 and t2 correspond to either TreatmentNumber or
#'   Name in the trt.dict data frame and y1 and y2 are treatment
#'   differences of treatment 2 - treatment 1.
#' @param  trt.dict Treatment dictionary with columns Name, Abbreviation, and
#'   TreatmentNumber. Name and Abbreviation can be (potentially the same) string
#'   name. TreatmentNumber should be a numeric indicator for the treatment used
#'   to replace text with codes.
#'
#' @return  What does the function return?
#'
#' @examples
#'  Put some examples of how it works or just delete this
#'
#' @export
.prep.brma.dat <- function(dat, trt.dict){



  ############################################################################## #
  ############################################################################## #
  #
  # 1. Input error catches----
  #
  #     Section Notes
  # Tests for:
  #  - Column names
  #  - Entry types
  #
  ############################################################################## #
  ############################################################################## #


  ############################################################################## #
  ############################################################################## #
  #
  # 2. Expand outcome column----
  #
  #     Section Notes
  #
  # Include an warning and correction if treatment order isn't ascending
  ############################################################################## #
  ############################################################################## #

   dat <- dat %>%
    dplyr::mutate(t1 = trt.dict$TreatmentNumber[match(t1, trt.dict$Name)],
                  t2 = trt.dict$TreatmentNumber[match(t2, trt.dict$Name)]) %>%
      dplyr::mutate(flip = t2 < t1) %>%
    dplyr::mutate(dplyr::across(.cols = c(y1, y2), ~ ifelse(flip, .*-1, .)),
                  temp = t2,
                  t1 = ifelse(flip, t2, t1),
                  t2 = ifelse(flip, temp, t2)) %>%
      dplyr::select(-temp)


  if(any(dat$flip)){
    trials <- dat %>%
      dplyr::filter(flip) %>%
      dplyr::pull(StudyName)
    warning(glue::glue("Some trials had t1 > t2. This was corrected and treatment estimates flipped. Trials: {trials} "))
  }

  y <- dat %>% dplyr::select(y1,y2) %>% as.matrix()
  se <- dat %>% dplyr::select(se1,se2) %>% as.matrix()
  t <- dat %>% dplyr::select(t1,t2) %>% as.matrix()
  NS <- nrow(dat)
  NT <- nrow(trt.dict)

  return(dplyr::lst(y, se, t, NS, NT))
  }
