


# Code --------------------------------------------------------------------
t1 <- Sys.time()

source(file = "VAR.R")
t2 <- Sys.time()
t2 - t1

source(file = "FAVAR.R")
t3 <- Sys.time()
t3 - t2

source(file = "COMPLEX.R")
t4 <- Sys.time()
t4 - t3

source(file = "EXTRA.R")
t5 <- Sys.time()
t5 - t4
# Extra -------------------------------------------------------------------


