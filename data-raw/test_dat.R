############################################################################## #
############################################################################## #
#
# 1. Simulate test data----
#
#     Section Notes
#
# Simulate some simple test data that can be used with the data simulation
# functions.
############################################################################## #
############################################################################## #

test.dat.cont <-  tibble::tribble(
                    ~StudyName,  ~y1, ~y2, ~se1, ~se2, ~t1, ~t2,
                           "a",   1,  1,   NA,   NA, "C", "A",
                           "b",   NA,  NA,   NA,   NA, "A", "B",
                           "c",   NA,  NA,   NA,   NA, "A", "C",
                           "d",   NA,  NA,   NA,   NA, "A", "B",
                           "e",   NA,  NA,   NA,   NA, "A", "C",
                           "f",   NA,  NA,   NA,   NA, "A", "B"
                    )




test.trt.dict <- tibble::tribble(
                   ~TreatmentNumber, ~Name, ~Abbreviation,
                                 1L,   "A",           "A",
                                 2L,   "B",           "B",
                                 3L,   "C",           "C"
                   )

usethis::use_data(test.trt.dict, overwrite = TRUE)
usethis::use_data(test.dat.cont, overwrite = TRUE)
