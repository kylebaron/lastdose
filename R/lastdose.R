VALID_TIME_UNITS <- c("auto", "secs", "mins", "hours", "days", "weeks")
#' @useDynLib lastdose, .registration=TRUE
#' @importFrom Rcpp evalCpp
NULL

#' Calculate last dose amount and times since previous doses
#'
#' This function calculates the last dose amount (`LDOS`), the time after
#' last dose (`TAD`), and time after first dose (`TAFD`). Use [lastdose()]
#' to add (or potentially replace) columns to the input data frame;
#' [lastdose_list()] and [lastdose_df()] returns calculated information
#' as either `list` or `data.frame` format without modifying the input data.
#'
#' @param data data set as data frame; see `details`
#' @param time_col character name for the `TIME` column; this could be time after
#' first dose or time after first record or time relative to any origin; input
#' may be `numeric` or `POSIXct` (e.g. `DATETIME`); if `POSIXct`, a numeric
#' value will be calculated based on the value of `time_units`. The data frame
#' will be searched for the first matching candidate time column using
#' [find_time_col()]; if you don't want `lastdose` to search, you should pass
#' in the name of the column to use for `TIME`.
#' @param time_units for calculating time when the time column inherits
#' `POSIXct`; you may use any value that is valid for [difftime()]
#' @param id_col character name for the subject `ID` column; may be numeric
#' or character; if character, a numeric value is derived. The data frame
#' will be searched for the first matching candidate `ID` column using
#' [find_id_col()]; if you don't want `lastdose` to search, you should pass
#' in the name of the column to use for `ID`.
#' @param back_calc if `TRUE`, then the time before the first dose
#' is calculated for records prior to the first dosing record when
#' at least one dosing record is found in the data set.  Records before
#' the first dosing record will have negative values.
#' @param fill the value for `TAD` and `TAFD` that is used for records when no
#' doses are found for an individual or when `back_calc` is `FALSE`.
#' @param addl_ties what to do when doses scheduled through `ADDL` happen at
#' the same time as observation records; if `obs_first` then the observation
#' is assumed to happen before the dose and the observation is a trough
#' concentration; if `dose_first` then the dose is assumed to be administered
#' and the observation made immediately after (with no advance in time). See
#' details.
#' @param comments a logical vector with length equal to the number of rows
#' in `data` indicating which records are to be ignored when looking for `TAD`
#' and `LDOS`.
#' @param ... arguments passed to [lastdose_list()]
#' @param include_ldos `logical`; if `FALSE` then the `LDOS` data is not
#' appended to the data set.  Only used for the [lastdose()] function.
#' @param include_tafd `logical`; if `FALSE`, then `TAFD` data is not appended
#' to the data set.  Only used for the [lastdose()] function.
#' @param include_occ `logical`; if `FALSE` then `OCC` is not appended to the
#' data set. Only used for the [lastdose()] function.
#'
#' @section Options:
#'
#' These are options that can be set to customize `lastdose` behavior
#' for the current context.  See `?options` for how to set an option.
#'
#' - `lastdose.time_units`: sets the default time unit that is used to calculate
#'   relative times when the time column is represented as date-time data
#'   (`POSIXct`)
#' - `lastdose.id_col`: sets the default value for the `id_col` argument
#'   to last dose; this identifies the column that is to be used to distinguish
#'   individuals; the data in this column may be numeric or character
#' - `lastdose.include_tafd`: sets default value for `include_tafd`; if `TRUE`
#'   then the time since the first dose record (EVID 1 or EVID 4) in the data
#'   set will be automatically appended to the output data frame when
#'   calling `lastdose()`; `tafd` is always included when calling
#'   `lastdose_df()` and `lastdose_list()`
#'
#' @details
#'
#' When calling [lastdose()] to modify the data frame, two columns will be
#' added (by default): `TAD` indicating the time after the most-recent dose,
#' and `LDOS` indicating the amount of the most recent dose. `TAFD` indicating
#' the time after the first dose record (`EVID` 1 or 4) can be added via the
#' `include_tafd` argument and users can opt out from adding `LDOS` with the
#' `include_ldos` argument.
#'
#'
#' When calling [lastdose_list()] or [lastdose_df()], the respective items are
#' accessible with `tad`,  `tafd`, and `ldos` (note the lower case form here to
#' distinguish from the columns that might be added to the data frame).
#'
#' **Time after first dose**: note that time after first dose (`TAFD`) is the
#' time after the first dosing record (`EVID` 1 or 4) in the data frame that
#' you pass in. If you don't have a dosing record for the first dose to
#' anchor this calculation, you should opt out.
#'
#' **Handling of commented records**: Dosing records that have been "commented"
#' (as indicated with the `comments` argument) will never be considered as
#' actual doses when determining `TAD`, `TAFD`, and `LDOS`.  But commented
#' records (doses and non-doses) will be assigned `TAD`, `TAFD`, and `LDOS`
#' according to the last non-commented dosing record.
#'
#' **Additional notes**:
#'
#' - All functions require an input data set as a data frame
#' - The data set should be formatted according to `NMTRAN` type
#'   conventions
#' - Required columns
#'   - A subject ID column (either `ID` or user-specified)
#'   - A record time column (either `TIME` or user-specified)
#'   - `AMT` or `amt`: dose amount for dosing records
#'   - `EVID` or `evid`: event ID; records with `EVID` or 1 or 4
#'     are considered dosing records
#' - Optional columns
#'   - `ADDL` or `addl`: additional doses to administer
#'   - `II` or `ii`: dosing interval
#' - An error is generated if required columns are not found; no error
#'   or warning if optional columns are not found
#' - All required and optional columns are required to be numeric
#' - Missing values are not allowed in: `ID`, `EVID`, `ADDL`, `II`
#' - When missing values are found in `TIME`, both `TAD` and `LDOS` are set to
#'   missing
#' - An error is generated for missing `AMT` in dosing records (evid 1 or 4)
#' - No error is generated for missing `AMT` in non-dosing records
#'
#' An example illustrating the `addl_ties` argument: when there is `Q24h`
#' dosing and both an an additional dose and an observation happen at 24 hours,
#' `obs_first` will set the observation `TAD` to 24 and `dose_first` will set
#' the observation `TAD` to 0.
#'
#' @examples
#' file <- system.file("csv/data1.csv", package="lastdose")
#'
#' require("Rcpp")
#'
#' data <- read.csv(file)
#'
#' a <- lastdose(data)
#'
#' b <- lastdose_df(data)
#'
#' c <- lastdose_list(data)
#'
#'
#' @export
lastdose <- function(data, ..., include_ldos = TRUE,
                     include_tafd = getOption("lastdose.include_tafd", FALSE),
                     include_occ = getOption("lastdose.include_occ", TRUE)) {
  ans <- lastdose_list(data, ...)
  data[["TAD"]] <- ans[["tad"]]
  if(include_tafd) data[["TAFD"]] <- ans[["tafd"]]
  if(include_ldos) data[["LDOS"]] <- ans[["ldos"]]
  if(include_occ)  data[["OCC"]] <- ans[["occ"]]
  data
}

#' @rdname lastdose
#' @export
lastdose_list <- function(data,
                          time_col = find_time_col(data),
                          time_units = getOption("lastdose.time_units", NULL),
                          id_col = find_id_col(data),
                          fill = -99,
                          back_calc = TRUE,
                          addl_ties = c("obs_first", "dose_first"),
                          comments = find_comments(data),
                          include_occ = getOption("lastdose.include_occ", TRUE)) {

  if(length(comments) == 1) {
    comments <- rep(comments,nrow(data))
  }
  if(length(comments) != nrow(data)) {
    stop(
      "'comments' must be have length equal to the number of rows in 'data'",
      call. = FALSE
    )
  }
  back_calc <- isTRUE(back_calc)
  include_occ <- isTRUE(include_occ)
  addl_ties <- match.arg(addl_ties)
  sort1 <- addl_ties == "obs_first"
  lower_names <- tolower(names(data))
  wtime <- match(time_col, names(data))
  if(is.na(wtime)) {
    stop("did not find time column `", time_col, "` in `data`", call. = FALSE)
  }
  has_na_time <- anyNA(data[[wtime]])
  if(has_na_time) {
    na_time <- is.na(data[[wtime]])
    data <- data[!na_time,, drop = FALSE]
    comments <- comments[!na_time]
  }
  col_time <- data[[wtime]]
  if(inherits(col_time, "POSIXct")) {
    if(is.null(time_units)) {
      stop(
        "`time_units` is required when time column inherits `POSIXct`",
        call.=FALSE
      )
    }
    if(!is.element(time_units, VALID_TIME_UNITS)) {
      stop(
        "`time_units` has invalid value; see `lastdose:::VALID_TIME_UNITS`",
        call. = FALSE
      )
    }
    col_time <- difftime(col_time, min(col_time), units = time_units)
    col_time <- as.numeric(col_time)
  }
  if(!is.numeric(col_time)) {
    stop("time column is required to be numeric", call.=FALSE)
  }
  wid <- match(id_col, names(data))
  if(is.na(wid)) {
    stop("did not find id column `", id_col, "` in `data`", call.=FALSE)
  }
  col_id <- data[[wid]]
  if(is.character(col_id)) {
    col_id <- match(col_id, unique(col_id))
  }
  if(!is.numeric(col_id)) {
    stop("id column is required to be numeric", call.=FALSE)
  }
  wamt <- match("amt", lower_names)
  if(is.na(wamt)) {
    stop("column AMT or amt is required in the data set", call.=FALSE)
  }
  col_amt <- data[[wamt]]
  if(!is.numeric(col_amt)) {
    stop("column AMT/amt is required to be numeric", call.=FALSE)
  }
  wevid <- match("evid", lower_names)
  if(is.na(wevid)) {
    stop("column EVID or evid is required in the data set.", call.=FALSE)
  }
  col_evid <- data[[wevid]]
  if(!is.numeric(col_evid)) {
    stop("column EVID/evid is required to be numeric", call.=FALSE)
  }
  waddl <- match("addl", lower_names)
  if(is.na(waddl)) {
    col_addl <- vector(mode = "numeric", length=nrow(data))
    wii <- NULL
  } else {
    col_addl <- data[[waddl]]
  }
  if(!is.numeric(col_addl)) {
    stop("column ADDL/addl is required to be numeric", call.=FALSE)
  }
  wii <- match("ii", lower_names)
  if(is.na(wii)) {
    col_ii <- vector(mode = "numeric", length=nrow(data))
    wii <- NULL
  } else {
    col_ii <- data[[wii]]
  }
  if(!is.numeric(col_ii)) {
    stop("column II/ii is required to be numeric", call.=FALSE)
  }
  fill <- as.double(fill)
  if(length(fill)==0) fill <- 0
  ans <- lastdose_impl(
    col_id,
    col_time,
    col_amt,
    col_evid,
    col_addl,
    col_ii,
    fill,
    back_calc,
    sort1,
    comments,
    include_occ
  )
  if(has_na_time) {
    re_order <- order(c(which(!na_time), which(na_time)))
    for(j in seq_along(ans)) {
      ans[[j]] <- ans[[j]][re_order]
    }
  }
  ans
}

#' @rdname lastdose
#' @export
lastdose_df <- function(data, ...) {
  ans <- lastdose_list(data, ...)
  out <- data.frame(
    tad = ans[["tad"]],
    tafd = ans[["tafd"]],
    ldos = ans[["ldos"]],
    stringsAsFactors = FALSE, check.names = FALSE,
    fix.empty.names = FALSE, row.names = NULL
  )
  if(!is.null(ans[["occ"]])) {
    out$occ <- ans[["occ"]]
  }
  out
}

#' Find commented records
#'
#' This function uses specific criteria for finding comment records that may
#' not match up with your coding conventions.  See details below as well as
#' unit tests.
#'
#' @param x a data frame or character vector
#' @param ... not used
#'
#' @return
#' A logical vector
#'
#' @details
#' For the data frame method, comments are found in a column with name `C`
#' with type `character`.  If a `C` column is found that is not character,
#' a warning is generated and no comments are found.
#'
#' For the character method or when an appropriate `C` column is found when
#' using the data frame method, a position or row is considered a comment
#' when it is either `NA` or when it is equal to `.`.
#'
#' @examples
#' comment <- c(NA, "C", "C", NA, ".", NA, "Comment")
#' dv <- rnorm(length(comment))
#' df <- data.frame(C = comment , DV = dv, stringsAsFactors = FALSE)
#'
#' find_comments(df)
#'
#'
#' @export
find_comments <- function(x, ...) UseMethod("find_comments")
#' @rdname find_comments
#'
#' @export
find_comments.data.frame <- function(x, ...) {
  if(!inherits(x[["C"]], c("logical", "character"))) {
    if(exists("C", x)) {
      warning(
        "looking for comment records; found column `C` but it wasn't ",
        "character or logical"
      )
    }
    return(vector(mode="logical", nrow(x)))
  }
  find_comments(x[["C"]])
}

#' @rdname find_comments
#' @export
find_comments.character <- function(x, ...) {
  !(is.na(x)|x=='.')
}

#' @rdname find_comments
#' @export
find_comments.logical <- function(x, ...) {
  x & !is.na(x)
}

#' Find TIME column
#'
#' Search data frame names for the first matching candidate TIME column name.
#' See `details`.
#'
#' @param data a data.frame to search
#' @details
#' Column names will be searched against the following candidates
#'
#' - `TIME`
#' - `DATETIME`
#'
#' The first the first candidate to be matched will be returned. If there
#' are no matches, an error is generated.
#'
#' @examples
#' data <- data.frame(A = 1, DATETIME = 2, TIME = 3, Z = 99)
#' lastdose:::find_time_col(data)
#'
find_time_col <- function(data) {
  stopifnot(is.data.frame(data))
  ans <- intersect(c("TIME", "DATETIME"), names(data))
  if(length(ans)==0) {
    stop("could not find a TIME column in `data`", call. = FALSE)
  }
  ans[1]
}

#' Find ID column
#'
#' Search data frame names for the first matching candidate ID column name.
#' See `details`.
#'
#' @param data a data.frame to search
#' @details
#' Column names will be searched against the following candidates
#'
#' - `getOption("lastdose.id_col")`
#' - `ID`
#' - `USUBJID`
#' - `SUBJID`
#' - `PTNO`
#' - `SUBJ`
#'
#' The first the first candidate to be matched will be returned. If there
#' are no matches, an error is generated.
#'
#' @examples
#' data <- data.frame(A = 1, B = 2, PTNO = 3, ID = 4, Z = 99)
#' lastdose:::find_id_col(data)
#'
find_id_col <- function(data) {
  stopifnot(is.data.frame(data))
  op <- getOption("lastdose.id_col", NULL)
  can <- c(op, "ID", "USUBJID", "SUBJID", "PTNO", "SUBJ")
  ans <- intersect(can, names(data))
  if(length(ans)==0) {
    stop("could not find a subject identifier column in `data`", call. = FALSE)
  }
  ans[1]
}

