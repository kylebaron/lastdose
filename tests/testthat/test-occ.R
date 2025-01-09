library(testthat)
library(lastdose)


occdata <- function(file) {
  file <- system.file("test-data", "occ", file, package = "lastdose")
  read.csv(file, stringsAsFactors = FALSE)
}

test_that("OCC single dose", {
  data <- occdata("single-dose.csv")
  data <- lastdose(data)
  expect_equal(data$OCC[1], 0)
  expect_true(all(data$OCC[-1] ==1))
})

test_that("OCC single dose, recsort==3", {
  data <- occdata("single-dose-recsort3.csv")
  data <- lastdose(data)
  expect_equal(unique(data$OCC), 1L)
  expect_equal(data$EVID[1], 1L)
})

test_that("OCC multi-dose, explicit", {
  data <- occdata("multi-dose-explicit.csv")
  data <- lastdose(data)
  expect_equal(unique(data$OCC), c(0,1,2))
  sp <- split(data, data$OCC)
  # Single observation prior to first dose
  expect_equal(sp[[1]]$OCC, 0)
  # First occasion
  expect_equal(sp[[2]]$OCC[1], 1)
  expect_equal(sp[[2]]$EVID[1], 1)
  expect_equal(range(sp[[2]]$TIME), c(0,12))
  # Second occasion
  expect_equal(sp[[3]]$OCC[1], 2)
  expect_equal(sp[[3]]$EVID[1], 1)
  expect_equal(range(sp[[3]]$TIME), c(12,24))
})

test_that("OCC multi-dose, addl", {
  data <- occdata("multi-dose-addl.csv")
  data <- lastdose(data)
  expect_equal(unique(data$OCC), c(0,1,2))
  sp <- split(data, data$OCC)
  # Single observation prior to first dose
  expect_equal(sp[[1]]$OCC, 0)
  # First occasion
  expect_equal(sp[[2]]$OCC[1], 1)
  expect_equal(sp[[2]]$EVID[1], 1)
  expect_equal(range(sp[[2]]$TIME), c(0,10))
  # Second occasion
  expect_equal(sp[[3]]$OCC[1], 2)
  expect_equal(sp[[3]]$EVID[1], 0)
  expect_equal(range(sp[[3]]$TIME), c(12,24))
})

test_that("OCC multi-dose, addl dose_first", {
  data <- occdata("multi-dose-addl.csv")
  data1 <- lastdose(data)
  data2 <- lastdose(data, addl_ties = "dose_first")
  expect_identical(data1$OCC, data2$OCC)
  expect_false(all(data1$TAD==data2$TAD))
})

test_that("OCC multi-dose, addl", {
  data <- occdata("multi-dose-addl-no-obs.csv")
  data <- lastdose(data)
  expect_equal(unique(data$OCC), c(0,1))
  sp <- split(data, data$OCC)
  # The first dose is not actually an OCC
  expect_equal(sp[[1]]$OCC, c(0,0))
  expect_equal(sp[[1]]$AMT, c(0,100))
  # First and only occasion
  expect_true(all(sp[[2]]$OCC==1))
  expect_equal(sp[[2]]$EVID[1], 0)
})

test_that("OCC multi-dose, explicit", {
  data <- occdata("multi-dose-explicit-no-obs.csv")
  data <- lastdose(data)
  expect_equal(unique(data$OCC), c(0,1))
  sp <- split(data, data$OCC)
  # The first several doses are not actually an OCCs
  expect_equal(sp[[1]]$OCC, c(0,0,0,0))
  expect_equal(sp[[1]]$AMT, c(0,100,100,100))
  # First and only occasion
  expect_true(all(sp[[2]]$OCC==1))
  expect_equal(sp[[2]]$EVID[1], 1)
})

test_that("OCC handle EVID 2 and 3", {
  # Check EVID 2
  data <- occdata("evid-2-3-a.csv")
  data <- lastdose(data)
  expect_equal(unique(data$OCC), c(0,1,2))
  sp <- split(data, data$OCC)
  # Pre-dose observation
  expect_equal(sp[[1]]$OCC, 0)
  # First occasion
  expect_true(all(sp[[2]]$OCC==1))
  expect_equal(sp[[2]]$EVID[1], 1)
  expect_equal(sp[[2]]$EVID[2], 0)
  expect_true(all(sp[[2]]$EVID[c(3,4,5)] > 0))
  # Second occasion
  expect_true(all(sp[[3]]$OCC==2))
  expect_equal(sp[[3]]$EVID[1], 1)
  expect_equal(sp[[3]]$EVID[2], 0)

  # Check EVID 3
  data <- occdata("evid-2-3-b.csv")
  data <- lastdose(data)
  expect_equal(unique(data$OCC), c(0,1,2))
  sp <- split(data, data$OCC)
  # Pre-dose observation
  expect_equal(sp[[1]]$OCC, 0)
  # First occasion
  expect_true(all(sp[[2]]$OCC==1))
  expect_equal(sp[[2]]$EVID[1], 1)
  expect_equal(sp[[2]]$EVID[2], 0)
  expect_true(all(sp[[2]]$EVID[c(3,4,5)] > 0))
  # Second occasion
  expect_true(all(sp[[3]]$OCC==2))
  expect_equal(sp[[3]]$EVID[1], 1)
  expect_equal(sp[[3]]$EVID[2], 0)

  # Verify that EVID 0 triggers new OCC
  data <- occdata("evid-2-3-c.csv")
  data <- lastdose(data)
  expect_equal(unique(data$OCC), c(0,1,2,3))
  sp <- split(data, data$OCC)
  # Pre-dose observation
  expect_equal(sp[[1]]$OCC, 0)
  # First occasion
  expect_true(all(sp[[2]]$OCC==1))
  expect_equal(sp[[2]]$EVID[1], 1)
  expect_equal(sp[[2]]$EVID[2], 0)
  # Second occasion
  expect_true(all(sp[[3]]$OCC==2))
  expect_equal(sp[[3]]$EVID[1], 1)
  expect_equal(sp[[3]]$EVID[2], 3)
  expect_equal(sp[[3]]$EVID[3], 0)
  # Third occasion
  expect_true(all(sp[[4]]$OCC==3))
  expect_equal(sp[[4]]$EVID[1], 1)
  expect_equal(sp[[4]]$EVID[2], 0)
})

test_that("OCC resets for multiple subjects", {
  data1 <- occdata("evid-2-3-a.csv")
  data1$ID <- 1
  data2 <- occdata("evid-2-3-b.csv")
  data2$ID <- 2
  data3 <- occdata("evid-2-3-c.csv")
  data3$ID <- 3

  data <- rbind(data1,data2)
  data <- rbind(data, data3)
  data$CMT <- 1

  data <- lastdose(data)

  expect_equal(unique(data$OCC), c(0,1,2,3))

  start <- subset(data, TIME==0)
  expect_true(all(start$OCC==0))

  a <- subset(data, TIME==6)
  expect_equal(a$ID, c(1,2,3))
  expect_equal(a$OCC, c(2,2,3))

  b <- subset(data, TIME==1)
  expect_equal(b$ID, c(1,2,3))
  expect_equal(unique(b$OCC), 1)

  d <- subset(data, TIME==3)
  expect_equal(d$ID, c(1,2,3))
  expect_equal(d$OCC, c(1,1,2))
})

test_that("OCC handles commented dose records", {
  data <- data.frame(
    ID = 1,
    C =    c(NA, NA, "C", NA, NA, NA, NA),
    TIME = c( 1,  2,  3,   4,  5,  6,  7),
    AMT = 1,
    EVID = c(0, 0, 1, 0, 0, 1, 0)
  )
  #
  # ID    C TIME AMT EVID
  # 1 <NA>    1   1    0
  # 1 <NA>    2   1    0
  # 1    C    3   1    1
  # 1 <NA>    4   1    0
  # 1 <NA>    5   1    0
  # 1 <NA>    6   1    1
  # 1 <NA>    7   1    0
  data <- lastdose(data)
  sp <- split(data, data$OCC)
  expect_equal(length(sp), 2)
  expect_equal(nrow(sp[[1]]), 5)
  expect_true(all(sp[[1]]$OCC==0))
  expect_equal(nrow(sp[[2]]), 2)
  expect_true(all(sp[[2]]$OCC==1))
})

test_that("OCC handles commented observation records", {

  data <- data.frame(
    ID = 1,
    C    = c(NA, NA, "C", NA, NA, NA, "C", NA),
    TIME = c( 1,  2,  3,   4,  5,  6,  7,   8),
    AMT  = 1,
    EVID = c( 1,  0,  1,  0,  0,  1,  0, 2)
  )
  # Start with a dose
  # Skip past a commented dose
  # Another dose at TIME==6
  # The following observation record is commented (TIME==7)
  # EVID==2 won't increment OCC
  # ID    C TIME AMT EVID
  # 1 <NA>    1   1    1
  # 1 <NA>    2   1    0
  # 1    C    3   1    1
  # 1 <NA>    4   1    0
  # 1 <NA>    5   1    0
  # 1 <NA>    6   1    1
  # 1    C    7   1    0
  # 1 <NA>    8   1    2
  data <- lastdose(data)
  expect_true(all(data$OCC==1))
})
