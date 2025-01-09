library(testthat)
library(lastdose)

context("basic functionality")
set_file <- system.file("csv", "setn.csv", package = "lastdose")
df <- read.csv(set_file)
df$TIME <- df$time
df$time <- NULL
set1 <- subset(df, set==1)
set2 <- subset(df, set==2 & ID==1 & TIME <= 12)
set4 <- subset(df, set==4 & ID==1 & TIME <= 24)


test_that("doses at time zero [LSD-TEST-001]", {
  x <- lastdose(set1)
  expect_true(exists("TAD", x))
  expect_true(exists("LDOS", x))
  expect_identical(unique(x[["LDOS"]]), c(0,100))
  a <- c(0,0,4,8,12,4,8,12)
  expect_identical(x[["TAD"]],c(a,a))
  a <- c(0,rep(100,7))
  expect_identical(x[["LDOS"]],c(a,a))
})

test_that("time after first dose [LSD-TEST-002]", {
  x <- lastdose(set4)
  expect_false(exists("TAFD", x))
  x <- lastdose(set4, include_tafd = TRUE)
  expect_true(exists("TAFD", x))
  dose_rows <- which(set4$evid==1)
  time_of_first_dose <- set4$TIME[dose_rows[1]]
  expect_equal(x$TIME, x$TAFD + time_of_first_dose)
})

test_that("time ties (q12h dosing) [LSD-TEST-003]", {
  x <- lastdose(set1)
  z <- lastdose(set1, addl_ties = "dose_first")
  expect_false(identical(x,z))
  ax <- subset(x, TIME==12)
  az <- subset(z, TIME==12)
  expect_true(all(ax[["TAD"]] == 12))
  expect_true(all(az[["TAD"]] == 0))
})

test_that("don't fill back [LSD-TEST-004]", {
  x <- lastdose(set1, back_calc = FALSE)
  a <- c(-99,0,4,8,12,4,8,12)
  expect_identical(x[["TAD"]],c(a,a))
})

test_that("customize fill [LSD-TEST-005]", {
  x <- lastdose(set1, back_calc = FALSE, fill = NA_real_)
  a <- c(NA_real_,0,4,8,12,4,8,12)
  expect_identical(x[["TAD"]],c(a,a))
})

test_that("doses don't start at time zero [LSD-TEST-006]", {
  x <- lastdose(set2)
  a <- c(seq(-6,0),seq(0,6))
  expect_identical(x[["TAD"]],as.double(a))
})

test_that("lastdose_df [LSD-TEST-007]", {
  x <- lastdose_df(set1)
  y <- lastdose_list(set1)
  expect_identical(x[["tad"]], y[["tad"]])
})

test_that("lastdose_list [LSD-TEST-008]", {
  y <- lastdose_list(set1)
  expect_is(y,"list")
  expect_identical(names(y), c("tad", "tafd","ldos", "occ"))
})

test_that("required columns [LSD-TEST-009]", {
  x <- set1
  x[["amt"]] <- NULL
  expect_error(lastdose(x))
  x <- set1
  x[["time"]] <- NULL
  x[["TIME"]] <- NULL
  expect_error(lastdose(x))
  x <- set1
  x[["ID"]] <- NULL
  x[["id"]] <- NULL
  expect_error(lastdose(x))
  x <- set1
  x[["evid"]] <- NULL
  expect_error(lastdose(x))
})

test_that("non-numeric data throws error [LSD-TEST-010]", {
  for(col in c("ID","time", "addl", "ii", "evid", "amt")) {
    dd <- set1[seq(10), ]
    dd$ID <- NULL
    dd$TIME <- NULL
    dd <- dd[seq(10),]
    dd[[col]] <- "A"
    expect_error(lastdose(dd))
  }
})

test_that("records out of order throws error [LSD-TEST-011]", {
  set1$TIME[12] <- 1E6
  expect_error(lastdose(set1))
})

test_that("tad and ldos are NA when time is NA [LSD-TEST-012]", {
  set1$TIME[12] <- NA_real_
  ans <- lastdose(set1)[12,]
  expect_true(is.na(ans[["TAD"]]))
  expect_true(is.na(ans[["LDOS"]]))
  expect_true(is.na(ans[["TIME"]]))
})

test_that("error for missing values in ID,evid,ii,addl [LSD-TEST-013]", {
  for(col in c("ID", "evid", "ii", "addl")) {
    dd <- set1[seq(10),]
    dd[[col]] <- NA_real_
    expect_error(lastdose(dd))
  }
})

test_that("handle missing values in time colunn [LSD-TEST-014]", {
  dd <- df[df$set==4,]
  dd$TIME <- as.numeric(dd$TIME)
  dd0 <- dd
  set.seed(1010)
  i <- sample(seq(nrow(dd)), size = 18)
  i <- i[!dd$evid[i]==1]
  dd$TIME[i] <- NA_real_
  ans1 <- lastdose(dd)
  ans2 <- lastdose(dd0)
  w <- setdiff(seq(nrow(dd)),i)
  for(col in names(ans1)) {
    expect_identical(ans1[w,col], ans2[w,col])
  }
  ans3 <- ans1[i,]
  expect_true(all(is.na(ans3$TAD)))
  expect_true(all(is.na(ans3$LDOS)))
  expect_true(all(is.na(ans3$TAFD)))
  ans4 <- ans1[w,]
  expect_false(any(is.na(ans4$TAD)))
  expect_false(any(is.na(ans4$LDOS)))
  expect_false(any(is.na(ans4$TAFD)))

  file <- system.file("csv", "data_big.RDS", package = "lastdose")
  data <- readRDS(file)
  set.seed(21032)
  x <- sample(seq(nrow(data)), 1000)
  x <- x[data$EVID[x] ==0]
  data2 <- data
  data2$TIME[x] <- NA_real_
  out1 <- lastdose(data,  include_tafd = TRUE)
  out2 <- lastdose(data2, include_tafd = TRUE)
  ans <- as.numeric(c(0,  length(x)))
  smr <- function(x,y) {
    x <- x-y
    c(sum(x, na.rm = TRUE), sum(is.na(x)))
  }
  a <- smr(out1$TAD,  out2$TAD)
  b <- smr(out1$TIME, out2$TIME)
  c <- smr(out1$LDOS, out2$LDOS)
  d <- smr(out1$TAFD, out2$TAFD)
  expect_identical(a, ans)
  expect_identical(b, ans)
  expect_identical(c, ans)
  expect_identical(d, ans)
})

test_that("NA amt is error for dosing record, ok otherwise [LSD-TEST-015]", {
  dd <- set1
  dd$amt[5] <- NA_real_
  expect_is(lastdose(dd), "data.frame")
  dd <- set1
  dd$amt[10] <- NA_real_
  expect_error(lastdose(dd))
})

test_that("commented records [LSD-TEST-016]", {
  com <- c(".", NA ,"C", "A","Comment")
  ans <- find_comments(com)
  expect_identical(ans, c(FALSE,FALSE,TRUE,TRUE,TRUE))
  df <- data.frame(C = com, DV=stats::rnorm(length(com)))
  ans2 <- find_comments(com)
  expect_identical(ans,ans2)
  df2 <- df
  df2[["C"]] <- sample(c(0,1),nrow(df),replace=TRUE)
  expect_warning(find_comments(df2))
  set1[["TIME"]] <- ifelse(set1[["ID"]]==2, set1[["TIME"]] + 25, set1[["TIME"]])
  set1[["C"]] <- NA_character_
  set1[["ID"]] <- 1
  set1[["ii"]] <- 0
  set1[["addl"]] <- 0
  set1[["C"]][10] <- "C"
  ans <- lastdose(set1)
  diff <- ans[["TIME"]] - ans[["TAD"]]
  expect_equal(sum(diff),0)
  expect_error(lastdose(set1, comments = c(FALSE, TRUE,FALSE)))
})

test_that("undefined behavior when checking ADDL and II issue-11 [LSD-TEST-017]", {
  no_addl <- subset(set1, ID==1)
  no_addl[["addl"]] <- NULL
  no_addl[["ii"]] <- NULL
  ans <- lapply(1:10, function(i) lastdose(data = no_addl))
  ans <- sapply(ans,inherits,what="data.frame")
  expect_true(all(ans))
})

test_that("user-named time and id columns [LSD-TEST-018]", {
  d1 <- subset(set1, ID==1)
  d2 <- d1
  d2$xTAFD <- d2$TIME
  d2$TIME <- NULL
  expect_identical(
    lastdose_df(d1),
    lastdose_df(d2, time_col = "xTAFD")
  )
  expect_error(lastdose(d2), msg = "did not find time column")
  d2 <- d1
  d2$xUSUBJID <- "A"
  d2$ID <- NULL
  expect_identical(
    lastdose_df(d1),
    lastdose_df(d2, id_col = "xUSUBJID")
  )
  if(requireNamespace("withr")) {
    expect_identical(
      lastdose_df(d2, id_col = "xUSUBJID"),
      withr::with_options(
        list(lastdose.id_col = "xUSUBJID"),
        lastdose_df(d2)
      )
    )
  }
  expect_error(lastdose(d2))
})

test_that("find time column from candidate list [LSD-TEST-019]", {
  dd <- subset(set1, ID==1)
  time <- dd$TIME
  dd$TIME <- NULL
  tr <- c("TIME", "DATETIME")
  for(col in tr) {
    dd[[col]] <- time
    expect_is(lastdose(dd), "data.frame")
    dd[[col]] <- NULL
  }
})

test_that("find ID column from candidate list [LSD-TEST-020]", {
  dd <- subset(set1, ID==1)[1:3,]
  ID <- dd$ID
  dd$ID <- NULL
  tr <- c("ID", "USUBJID", "SUBJID", "PTNO", "SUBJ")
  for(col in tr) {
    dd[[col]] <- ID
    expect_is(lastdose(dd), "data.frame")
    dd[[col]] <- NULL
  }
  if(requireNamespace("withr")) {
    dd[["i_d"]] <- ID
    ans1 <- withr::with_options(
      list(lastdose.id_col = "i_d"),
      lastdose(dd)
    )
    expect_is(ans1, "data.frame")
  }
})

test_that("POSIXct datetime is converted to numeric time [LSD-TEST-021]", {
  d1 <- subset(set1, ID <= 2)
  d2 <- d1
  base <- as.POSIXct(0, origin = "2020-01-01", tz = "UTC")
  d1$TIME <- d1$TIME * 60 * 60 + base
  expect_identical(
    lastdose_df(d2),
    lastdose_df(d1, time_units = "hours")
  )
  expect_error(lastdose(d1), msg="is required when time column is")
  expect_error(
    lastdose(d1, time_units = "seconds")
  )
  if(requireNamespace("withr")) {
    ans1 <- withr::with_options(
      list(lastdose.time_units = "hours"),
      lastdose(d1)
    )
    ans2 <- lastdose(d1, time_units = "hours")
    expect_identical(ans1, ans2)
  }
})

test_that("logical comment column is ok [LSD-TEST-022]", {
  d <- subset(set1, ID ==1)
  d$C <- sample(c(FALSE, TRUE), nrow(d), replace = TRUE)
  expect_silent(lastdose(d))
  d$C <- sample(c(1, 2), nrow(d), replace = TRUE)
  expect_warning(lastdose(d), msg = "but it wasn't character or logical")
})

test_that("ii detection issue-21 [LSD-TEST-023]", {
  data <- data.frame(
    TIME = c(0,1,2,3,4,5,6,7,8),
    AMT  = c(0,1,0,0,0,0,0,0,0),
    EVID = c(0,1,0,0,0,0,0,0,0),
    II   = c(0,2,0,0,0,0,0,0,0),
    ADDL = c(0,2,0,0,0,0,0,0,0),
    ID = 1
  )
  out <- lastdose(data, addl_ties = "dose_first")
  expect_true(all(out$LDOS[-1]==1))
  expect_equal(out$TAD[1],-1)
  doses <- subset(out, TIME %in% c(1,3,5))
  expect_true(all(doses$TAD==0))
  ones <- subset(out, TIME %in% c(2,4,6))
  expect_true(all(ones$TAD==1))
  term <- subset(out, TIME >=5)
  expect_equal(term$TAD, c(0,1,2,3))
  data2 <- data
  II <- data2$II
  data2$II <- NULL
  data2$ii <- II
  out <- lastdose(data2, addl_ties = "dose_first")
  expect_true(all(out$LDOS[-1]==1))
  expect_equal(out$TAD[1],-1)
  doses <- subset(out, TIME %in% c(1,3,5))
  expect_true(all(doses$TAD==0))
  ones <- subset(out, TIME %in% c(2,4,6))
  expect_true(all(ones$TAD==1))
  term <- subset(out, TIME >=5)
  expect_equal(term$TAD, c(0,1,2,3))
})

test_that("error if ADDL requested by II le 0 [LSD-TEST-024]", {
  data <- data.frame(
    TIME = c(0,1,2,3),
    AMT  = c(0,1,0,0),
    EVID = c(0,1,0,0),
    ADDL = c(0,2,0,0),
    II   = 0,
    ID = 1
  )
  expect_error(
    lastdose(data),
    msg = "ADDL doses requested, but II not positive at row 2"
  )
})

test_that("comments vector is subset for NA time #38 [LSD-TEST-025]", {
  data <- data.frame(
    ID = c(1,1,1,2,2,2),
    C = c("C", NA, NA, "C", NA, NA),
    TIME = c(-1, 0, NA, -1, 0, 0.25),
    AMT = c(0, 100, 0, 0, 200, 0),
    EVID = c(0, 1, 0, 0, 1, 0)
  )
  data <- lastdose(data)
  expect_equal(data$TAD, c(-1, 0, NA, -1, 0, 0.25))
})

test_that("data frame is not modified", {
  data <- data.frame(
    ID = 1,
    AMT = c(0, 1, NA, 0, 0),
    TIME = c(1, 2, 3, 4, 5),
    EVID = c(0, 1, 0, 0, 0)
  )
  data2 <- data.frame(
    ID = 1,
    AMT =  c(0, 1, NA, 0, 0),
    TIME = c(1, 2, 3, 4, 5),
    EVID = c(0, 1, 0, 0, 0)
  )
  expect_identical(data, data2)
  ld <- lastdose(data)
  expect_identical(data, data2)
})

test_that("TAD is the same for records with the same time", {
  file <- system.file("csv", "temp-tad.csv", package = "lastdose")
  data <- read.csv(file, na.strings = ".", stringsAsFactors = FALSE)

  ans1 <- lastdose(data, addl_ties = "obs_first")
  expect_true(all(ans1[21:24,]$TAD==0))

  ans2 <- lastdose(data, addl_ties = "dose_first")
  expect_true(all(ans2[21:24,]$TAD==0))

  data <- data.frame(
    ID = 1,
    TIME = c(1, 2, 3, 4, 4,  4, 4),
    AMT =  c(0, 0, 1, 0, 0, 10, 0),
    EVID = c(0, 0, 1, 0, 0,  1, 0)
  )

  ans3 <- lastdose(data)
  expect_equal(ans3$TAD,  c(-2, -1,  0, 1, 1,  0,  0))
  expect_equal(ans3$LDOS, c( 0,  0,  1, 1, 1, 10, 10))
})
