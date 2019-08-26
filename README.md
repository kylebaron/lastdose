
# lastdose

Calculate the time since and amount of the last dose. Additional
(`ADDL`) dosing records are expanded and included in the calculation.

See [inst/doc/about.md (on GitHub only)](inst/doc/about.md) for more
details.

<hr>

``` r
library(lastdose)
library(tidyverse)
theme_set(theme_bw())

file <- system.file("csv/data1.csv", package = "lastdose")

df <- read.csv(file) 

head(df)
```

    .   ID TIME EVID  AMT CMT II ADDL   DV
    . 1  1    0    0    0   0  0    0  0.0
    . 2  1    0    1 1000   1 24   27  0.0
    . 3  1    4    0    0   0  0    0 42.1
    . 4  1    8    0    0   0  0    0 35.3
    . 5  1   12    0    0   0  0    0 28.9
    . 6  1   16    0    0   0  0    0 23.6

## A PK profile

``` r
df %>% filter(EVID==1) %>% count(TIME,AMT,ADDL)
```

    . # A tibble: 3 x 4
    .    TIME   AMT  ADDL     n
    .   <int> <int> <int> <int>
    . 1     0  1000    27     1
    . 2   672   500    27     1
    . 3  1344   300    27     1

``` r
ggplot(df, aes(TIME,DV)) + geom_line() + theme_bw()
```

![](man/figures/readme-unnamed-chunk-3-1.png)<!-- -->

# Plot last dose versus time

``` r
df <- lastdose(df)

ggplot(df, aes(TIME,LDOS)) + geom_line()
```

![](man/figures/readme-unnamed-chunk-4-1.png)<!-- -->

# Plot time after dose versus time

``` r
ggplot(df, aes(TIME,TAD)) + geom_line()
```

![](man/figures/readme-unnamed-chunk-5-1.png)<!-- -->

``` r
ggplot(df, aes(TIME,TAD)) + geom_line() + 
  scale_x_continuous(breaks = seq(0,72,4), limits=c(0,72)) + 
  scale_y_continuous(breaks = seq(0,24,4), limits=c(0,24)) 
```

![](man/figures/readme-unnamed-chunk-5-2.png)<!-- -->

# All doses explicit in the data set

``` r
df2 <- mrgsolve::realize_addl(df) 

df2 <- lastdose(df2)

ggplot(df2, aes(TIME,TAD)) + geom_line()
```

![](man/figures/readme-unnamed-chunk-6-1.png)<!-- -->

``` r
ggplot(df2, aes(TIME,TAD)) + geom_line() + 
  scale_x_continuous(breaks = seq(0,72,4), limits = c(0,72)) + 
  scale_y_continuous(breaks = seq(0,24,4))
```

![](man/figures/readme-unnamed-chunk-6-2.png)<!-- -->

# How does it perform on bigger data?

Same setup as the previous profile, but more individuals.

We have 500K rows and 1000 individuals

``` r
file <- system.file("csv/data_big.RDS", package = "lastdose")

big <- readRDS(file)

dim(big)
```

    . [1] 508000      8

``` r
length(unique(big$ID))
```

    . [1] 1000

Timing result

``` r
system.time(x2 <- lastdose(big))
```

    .    user  system elapsed 
    .   0.175   0.002   0.176

## Compare against the single profile

``` r
system.time(x1 <- lastdose(df))
```

    .    user  system elapsed 
    .       0       0       0

``` r
x3 <- filter(x2, big[["ID"]]==1) %>% as.data.frame()

all.equal(x1,x3)
```

    . [1] TRUE

# Observations prior to the first dose

When non-dose records happen prior to the first dose, lastdose
calculates the time before the first dose (a negative value) for these
records.

``` r
file <- system.file("csv/data2.csv", package = "lastdose")

df <- read_csv(file)

lastdose(df) %>% head()
```

    . # A tibble: 6 x 10
    .      ID  TIME  EVID   AMT   CMT    II  ADDL    DV   TAD  LDOS
    .   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    . 1     1     0     0     0     0     0     0   0     -12     0
    . 2     1     4     0     0     0     0     0  42.1    -8     0
    . 3     1     8     0     0     0     0     0  35.3    -4     0
    . 4     1    12     0     0     0     0     0  28.9     0     0
    . 5     1    12     1  1000     1    24    27   0       0  1000
    . 6     1    16     0     0     0     0     0  23.6     4  1000

The user can alternatively control what happens for these records

``` r
lastdose(df, fill = NA_real_, back_calc=FALSE) %>% head()
```

    . # A tibble: 6 x 10
    .      ID  TIME  EVID   AMT   CMT    II  ADDL    DV   TAD  LDOS
    .   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    . 1     1     0     0     0     0     0     0   0      NA     0
    . 2     1     4     0     0     0     0     0  42.1    NA     0
    . 3     1     8     0     0     0     0     0  35.3    NA     0
    . 4     1    12     0     0     0     0     0  28.9    NA     0
    . 5     1    12     1  1000     1    24    27   0       0  1000
    . 6     1    16     0     0     0     0     0  23.6     4  1000