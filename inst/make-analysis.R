############################################################################## #
############################################################################## #
#
# 1. Simulate some example data ----
#
#     Section Notes
#
############################################################################## #
############################################################################## #

devtools::load_all()
data("test.dat.cont")
data("test.trt.dict")

# Make 5 replicates of the dataset
dat <-
  purrr::map(1:5, ~{
    test.dat.cont
  }) %>% dplyr::bind_rows() %>%
  dplyr::mutate(StudyName = as.character(1:dplyr::n()))

#---------------------------------------------------------------------- -
# 2. Simulate from BUGS model ----
#
# Simulate two sets of data: One assuming homogeneous slopes and one assuming
# heterogeneous slopes. Note that actual values may not
# make much sense because parameters are simulated from relatively uninformative
# prior
#---------------------------------------------------------------------- -

set.seed(1234) # Set seed for reasonable illustration
s.dat.shared <- .sim.mv(out.dat = dat,
                        trt.dict = test.trt.dict,
                        brma = TRUE)


# Simulate data where classes have different contrasts
s.dat.indiv <- .sim.mv(out.dat = dat,
                       trt.dict = test.trt.dict,
                       brma = FALSE)


#---------------------------------------------------------------------- -
# ..2.1. Create a merged dataset for plotting ----
#---------------------------------------------------------------------- -
d1 <- cbind(s.dat.shared$y, s.dat.shared$t) %>%
  as.data.frame() %>%
  dplyr::mutate(panel = "Simulated shared slope",
                V1 = V1 - mean(V1),
                V2 = V2 - mean(V2))
d2 <-  cbind(s.dat.indiv$y, s.dat.indiv$t) %>%
  as.data.frame() %>%
  dplyr::mutate(panel = "Simulated individual slope",
                V1 = V1 - mean(V1),
                V2 = V2 - mean(V2))

pdat <- rbind(d1, d2)

#---------------------------------------------------------------------- -
# ..2.2. Create an NMA dataset with missing observations ----
#---------------------------------------------------------------------- -
m.dat <- s.dat.shared[1:5]
ind <- 2
m.dat$y[ind,2] <- NA
m.dat$se[ind,2] <- NA


############################################################################## #
############################################################################## #
#
# 2. Run surrogate MA----
#
#     Section Notes
#
# JAGS does not allow mixtures of missing and non missing data in multivariate
# likelihoods so we have to use BUGS here. WinBUGS will not work on linux systems
# so if this is being run there your best bet is OpenBUGS.
############################################################################## #
############################################################################## #

iter <- 40000
burnin <- 20000
model <- here::here("inst/models/mvprog-shared")


samp <- R2WinBUGS::bugs(
  data = m.dat,
  parameters.to.save = c("y"), # Will give us back the imputed missing y
  model.file = model,
  n.burnin = burnin,
  n.iter = iter,
  inits = NULL,
  DIC = FALSE
)

############################################################################## #
############################################################################## #
#
# 3. Render report----
#
#     Section Notes
#
############################################################################## #
############################################################################## #


rmarkdown::render(
  here::here("inst","reports","tl-post.Rmd"),
   output_file = here::here("inst","reports",glue::glue("{Sys.Date()}_tl-post.html")))

