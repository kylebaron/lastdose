# lastdose (development version)

# lastdose 0.4.3

## New features and changes

- Updated `.Rbuildignore` to include NEWS in the build. (#44) 

# lastdose 0.4.2

## Bug fixes

- Fixed bug where `amt` was getting modified in source data frame 
  when it contained missing values (#40, #41).

# lastdose 0.4.1

- Fix bug where comments vector wasn't getting adjusted when `TIME` contained
  missing values (#37, #38).
  
- Put stories in yaml format; add script to build validation docs from the 
  yaml file (#35, #36). 

# lastdose 0.4.0

- Change default value for `include_tafd` to FALSE (#29)
- Add option `lastdose.include_tafd` (#29)
- Add option `lastdose.id_col`  (#27)
- Add option `lastdose.time_units` (#27)
- Search column names for candidate ID columns using `find_id_col()`; 
  this function is unexported (#25)
- Search column names for candidate time columns using `find_time_col()`;
  this function is unexported (#25)
- Handle missing values (`NA`) in the time column; these records will 
  stay in place and `NA` will be inserted for all outputs (#30)

# lastdose 0.3.2

- Fix bug where `II` column was not properly detected resulting in incorrect 
  `TAD` calculation when doses were also scheduled via `ADDL` #21
- Append `TAFD` (time after first dose) to data data frame on `lastdose()`
  and `lastdose_df()` call and add `tafd` to return from `lastdose_list()` #19
  
# lastdose 0.3.0

- Add `time_col` and `id_col` arguments to let user name columns containing 
  time and user id information #14
- ID column is allowed to be character; data will be converted to numeric if 
  it is character #14
- Time column is allowed to be `POSIXct`; data will be converted to numeric 
  according to value of `time_units` argument #9

# lastdose 0.2.1
- Fix bug where behavior is undefined when ADDL or II columns are omitted
  (#11)

# lastdose 0.2.0
- Commented records are not considered when looking for doses in the data set, 
  but `TAD` and `LDOS` are filled like any other observation record (relative
  to the last non-commented dose) (#9)

# lastdose 0.1.0

- Update git origin

