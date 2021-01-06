
### Overview

"roller" is a R package that provides functions to simulate rolling an object (e.g. a coin or a die) multiple times.

-   device() creates an object to be rolled with class "device".
-   roll() creates an object with class "rolls" (containing the rolls of the "device").
-   summary() method for object "rolls".
-   plot() method for object "rolls" to graph a barchart of relative frequencies.

### Installation

Install the development version from GitHub via the package "devtools":

``` r
# development version from GitHub:
# install.packages("devtools") 

# install "roller" (without vignettes):
# devtools::install_github("yulanrong/roller")

# install "roller" (with vignettes):
# devtools::install_github("yulanrong/roller", build_vignettes = TRUE)
```

### Usage

``` r
library(roller)

# default device
fair_coin <- device() 
fair_coin
#> object "device"
#> 
#>   side prob
#> 1    1  0.5
#> 2    2  0.5

# roll fair_coin 1 time
roll(fair_coin)
#> object "rolls"
#> 
#> $rolls
#> [1] 2

# roll fair_coin 20 times
roll20 <- roll(fair_coin, times = 20)
roll20
#> object "rolls"
#> 
#> $rolls
#>  [1] 2 2 2 1 1 1 1 1 1 1 2 2 1 2 2 1 2 1 2 1

# summary
summary(roll20)
#> summary "rolls"
#> 
#>   side count prop
#> 1    1    11 0.55
#> 2    2     9 0.45

# plot
# plot(roll20)
```
