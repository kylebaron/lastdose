
#'
#' # Examples
#'
library(dplyr)
library(mrgsolve)
library(lastdose)
library(here)

csv <- function(x, file) {
  file <- here("inst/data/occ/", file)
  x$CP <- NULL
  x$cp <- NULL
  write.csv(x, file, quote = FALSE, na = ".", row.names=FALSE)
}

mod <- house(delta = 2, end = 24, outvars = "CP")

#' ## Single dose
dose <- evd(amt = 100)
data <- mrgsim_df(mod, dose, carry_out = "AMT,EVID,CMT")
#' - `OCC` starts at zero
#' - `OCC` increments to 1 at the time of the first dose
csv(data, file = "single-dose.csv")

data <- mrgsim_df(mod, dose, carry_out = "AMT,EVID,CMT", recsort = 3)
#' Here, `OCC` starts at 1 because it was the first record
csv(data, file = "single-dose-recsort3.csv")

#' ## Multi-dose, with doses explicit in the data set
dose <- evd(amt = 100, ii = 12, addl = 1) %>% realize_addl()
data <- mrgsim_df(mod, dose, carry_out = "AMT,EVID,CMT")
#' - `OCC` starts at 0 again, increments with the first dose
#' - `OCC` increments at the time of the second dose because we have obserations following
csv(data, file = "multi-dose-explicit.csv")

#' ## Multi-dose, with doses coded via addl
dose <- evd(amt = 100, ii = 12, addl = 1)
data <- mrgsim_df(mod, dose, carry_out = "AMT,EVID,CMT,ADDL,II")

#' - `OCC` increments at 12 hours, the time of the second dose
csv(data, file = "multi-dose-addl.csv")


#' ## Multi-dose via addl, but no observations after the doses
dose <- evd(amt = 100, ii = 1, addl = 3)
data <- mrgsim_df(mod, dose, end = -1, add = c(0, seq(6,12)), carry_out = "AMT,EVID,CMT,ADDL,II")
#' - In this case, `OCC` doesn't increment at the time of the dose at `TIME==0`;
#' - This is because there wasn't an observation after the first dose, so we
#'   (intentionally) don't increment `OCC` at that point
csv(data, file = "multi-dose-addl-no-obs.csv")


#' We can see this explicitly here
dose <- evd(amt = 100, ii = 1, addl = 3) %>% realize_addl()
data <- mrgsim_df(mod, dose, end = -1, add = c(0, seq(6,12)), carry_out = "AMT,EVID,CMT,ADDL,II")
#' The rule is: `OCC` doesn't increment unless there are observations after the dose
csv(data, file = "multi-dose-explicit-no-obs.csv")


#' # EVID 2 or 3
#'
#' These currently don't count for establishing an occasion dose
#'
data <- data.frame(
  ID = 1,
  TIME = c(0, 1, 2, 3, 4, 5, 6, 7),
  EVID = c(0, 1, 0, 1, 2, 1, 1, 0),
  AMT = 100
)

csv(data, file = "evid-2-3-a.csv")


data <- data.frame(
  ID = 1,
  TIME = c(0, 1, 2, 3, 4, 5, 6, 7),
  EVID = c(0, 1, 0, 1, 3, 1, 1, 0),
  AMT = 100
)

csv(data, file = "evid-2-3-b.csv")

#' But if we find an observation before the next dose, we start the occasion
#' there
data <- data.frame(
  ID = 1,
  TIME = c(0, 1, 2, 3, 4, 5, 6, 7),
  EVID = c(0, 1, 0, 1, 3, 0, 1, 0),
  AMT = 100
)

csv(data, file = "evid-2-3-c.csv")
