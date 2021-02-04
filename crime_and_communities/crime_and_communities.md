Crime and Communities
================
Yulan

## Introduction

The crime and communities dataset contains crime data from communities
in the United States. The data combines socio-economic data from the
1990 US Census, law enforcement data from the 1990 US LEMAS survey, and
crime data from the 1995 FBI UCR. More details can be found at
<https://archive.ics.uci.edu/ml/datasets/Communities+and+Crime+Unnormalized>.

The dataset contains 125 columns total; \(p=124\) predictive and 1
target (ViolentCrimesPerPop). There are \(n=1994\) observations. These
can be arranged into an \(n \times p = 1994 \times 127\) feature matrix
\(\mathbf{X}\), and an \(n\times 1 = 1994 \times 1\) response vector
\(\mathbf{y}\) (containing the observations of ViolentCrimesPerPop).

## Set up the dataset

``` r
library(readr)
CC <- read_csv("crime_and_communities_data.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
print(dim(CC))
```

    ## [1] 1994  125

``` r
y <- CC$ViolentCrimesPerPop
X <- subset(CC, select = -c(ViolentCrimesPerPop))
```

## Dataset exploration

  - Which variables are categorical versus
    numerical?

<!-- end list -->

``` r
str(CC)
```

    ## Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame': 1994 obs. of  125 variables:
    ##  $ population           : num  11980 23123 29344 16656 140494 ...
    ##  $ householdsize        : num  3.1 2.82 2.43 2.4 2.45 2.6 2.45 2.46 2.62 2.54 ...
    ##  $ racepctblack         : num  1.37 0.8 0.74 1.7 2.51 ...
    ##  $ racePctWhite         : num  91.8 95.6 94.3 97.3 95.7 ...
    ##  $ racePctAsian         : num  6.5 3.44 3.43 0.5 0.9 1.47 0.4 1.25 0.92 0.77 ...
    ##  $ racePctHisp          : num  1.88 0.85 2.35 0.7 0.95 ...
    ##  $ agePct12t21          : num  12.5 11 11.4 12.6 18.1 ...
    ##  $ agePct12t29          : num  21.4 21.3 25.9 25.2 32.9 ...
    ##  $ agePct16t24          : num  10.9 10.5 11 12.2 20 ...
    ##  $ agePct65up           : num  11.3 17.2 10.3 17.6 13.3 ...
    ##  $ numbUrban            : num  11980 23123 29344 0 140494 ...
    ##  $ pctUrban             : num  100 100 100 0 100 100 100 100 100 100 ...
    ##  $ medIncome            : num  75122 47917 35669 20580 21577 ...
    ##  $ pctWWage             : num  89.2 79 82 68.2 75.8 ...
    ##  $ pctWFarmSelf         : num  1.55 1.11 1.15 0.24 1 0.39 0.67 2.93 0.86 1.54 ...
    ##  $ pctWInvInc           : num  70.2 64.1 55.7 39 41.1 ...
    ##  $ pctWSocSec           : num  23.6 35.5 22.2 39.5 29.3 ...
    ##  $ pctWPubAsst          : num  1.03 2.75 2.94 11.71 7.12 ...
    ##  $ pctWRetire           : num  18.4 22.9 14.6 18.3 14.1 ...
    ##  $ medFamInc            : num  79584 55323 42112 26501 27705 ...
    ##  $ perCapInc            : num  29711 20148 16946 10810 11878 ...
    ##  $ whitePerCap          : num  30233 20191 17103 10909 12029 ...
    ##  $ blackPerCap          : num  13600 18137 16644 9984 7382 ...
    ##  $ indianPerCap         : num  5725 0 21606 4941 10264 ...
    ##  $ AsianPerCap          : num  27101 20074 15528 3541 10753 ...
    ##  $ OtherPerCap          : num  5115 5250 5954 2451 7192 ...
    ##  $ HispPerCap           : num  22838 12222 8405 4391 8104 ...
    ##  $ NumUnderPov          : num  227 885 1389 2831 23223 ...
    ##  $ PctPopUnderPov       : num  1.96 3.98 4.75 17.23 17.78 ...
    ##  $ PctLess9thGrade      : num  5.81 5.61 2.8 11.05 8.76 ...
    ##  $ PctNotHSGrad         : num  9.9 13.72 9.09 33.68 23.03 ...
    ##  $ PctBSorMore          : num  48.2 29.9 30.1 10.8 20.7 ...
    ##  $ PctUnemployed        : num  2.7 2.43 4.01 9.86 5.72 4.85 8.19 4.18 8.39 7.19 ...
    ##  $ PctEmploy            : num  64.5 62 69.8 54.7 59 ...
    ##  $ PctEmplManu          : num  14.7 12.3 15.9 31.2 14.3 ...
    ##  $ PctEmplProfServ      : num  28.8 29.3 21.5 27.4 26.8 ...
    ##  $ PctOccupManu         : num  5.49 6.39 8.79 26.76 14.72 ...
    ##  $ PctOccupMgmtProf     : num  50.7 37.6 32.5 22.7 23.4 ...
    ##  $ MalePctDivorce       : num  3.67 4.23 10.1 10.98 11.4 ...
    ##  $ MalePctNevMarr       : num  26.4 28 25.8 28.1 33.3 ...
    ##  $ FemalePctDiv         : num  5.22 6.45 14.76 14.47 14.46 ...
    ##  $ TotalPctDiv          : num  4.47 5.42 12.55 12.91 13.04 ...
    ##  $ PersPerFam           : num  3.22 3.11 2.95 2.98 2.89 3.14 2.95 3 3.11 2.99 ...
    ##  $ PctFam2Par           : num  91.4 86.9 78.5 64 71.9 ...
    ##  $ PctKids2Par          : num  90.2 85.3 78.8 62.4 69.8 ...
    ##  $ PctYoungKids2Par     : num  95.8 96.8 92.4 65.4 79.8 ...
    ##  $ PctTeen2Par          : num  95.8 86.5 75.7 67.4 75.3 ...
    ##  $ PctWorkMomYoungKids  : num  44.6 51.1 66.1 59.6 63 ...
    ##  $ PctWorkMom           : num  58.9 62.4 74.2 70.3 70.5 ...
    ##  $ NumKidsBornNeverMar  : num  31 43 164 561 1511 ...
    ##  $ PctKidsBornNeverMar  : num  0.36 0.24 0.88 3.84 1.58 1.18 4.66 1.64 4.71 2.47 ...
    ##  $ NumImmig             : num  1277 1920 1468 339 2091 ...
    ##  $ PctImmigRecent       : num  8.69 5.21 16.42 13.86 21.33 ...
    ##  $ PctImmigRec5         : num  13 8.65 23.98 13.86 30.56 ...
    ##  $ PctImmigRec8         : num  21 13.3 32.1 15.3 38 ...
    ##  $ PctImmigRec10        : num  30.9 22.5 35.6 15.3 45.5 ...
    ##  $ PctRecentImmig       : num  0.93 0.43 0.82 0.28 0.32 1.05 0.11 0.47 0.72 0.53 ...
    ##  $ PctRecImmig5         : num  1.39 0.72 1.2 0.28 0.45 1.49 0.2 0.67 1.07 1.05 ...
    ##  $ PctRecImmig8         : num  2.24 1.11 1.61 0.31 0.57 2.2 0.25 0.93 1.63 1.66 ...
    ##  $ PctRecImmig10        : num  3.3 1.87 1.78 0.31 0.68 2.55 0.29 1.07 2.31 1.94 ...
    ##  $ PctSpeakEnglOnly     : num  85.7 87.8 93.1 95 96.9 ...
    ##  $ PctNotSpeakEnglWell  : num  1.37 1.81 1.14 0.56 0.6 0.6 0.28 0.43 2.51 0.81 ...
    ##  $ PctLargHouseFam      : num  4.81 4.25 2.97 3.93 3.08 5.08 3.85 2.59 6.7 3.66 ...
    ##  $ PctLargHouseOccup    : num  4.17 3.34 2.05 2.56 1.92 3.46 2.55 1.54 4.1 2.51 ...
    ##  $ PersPerOccupHous     : num  2.99 2.7 2.42 2.37 2.28 2.55 2.36 2.32 2.45 2.42 ...
    ##  $ PersPerOwnOccHous    : num  3 2.83 2.69 2.51 2.37 2.89 2.42 2.77 2.47 2.5 ...
    ##  $ PersPerRentOccHous   : num  2.84 1.96 2.06 2.2 2.16 2.09 2.27 1.91 2.44 2.31 ...
    ##  $ PctPersOwnOccup      : num  91.5 89 64.2 58.2 57.8 ...
    ##  $ PctPersDenseHous     : num  0.39 1.01 2.03 1.21 2.11 1.47 1.9 1.67 6.14 3.41 ...
    ##  $ PctHousLess3BR       : num  11.1 23.6 47.5 45.7 53.2 ...
    ##  $ MedNumBR             : num  3 3 3 3 2 3 2 2 2 2 ...
    ##  $ HousVacant           : num  64 240 544 669 5119 ...
    ##  $ PctHousOccup         : num  98.4 97.2 95.7 91.2 91.8 ...
    ##  $ PctHousOwnOcc        : num  91 84.9 57.8 54.9 55.5 ...
    ##  $ PctVacantBoarded     : num  3.12 0 0.92 2.54 2.09 1.41 6.39 0.45 5.64 2.77 ...
    ##  $ PctVacMore6Mos       : num  37.5 18.33 7.54 57.85 26.22 ...
    ##  $ MedYrHousBuilt       : num  1959 1958 1976 1939 1966 ...
    ##  $ PctHousNoPhone       : num  0 0.31 1.55 7 6.13 ...
    ##  $ PctWOFullPlumb       : num  0.28 0.14 0.12 0.87 0.31 0.28 0.49 0.19 0.33 0.3 ...
    ##  $ OwnOccLowQuart       : num  215900 136300 74700 36400 37700 ...
    ##  $ OwnOccMedVal         : num  262600 164200 90400 49600 53900 ...
    ##  $ OwnOccHiQuart        : num  326900 199900 112000 66500 73100 ...
    ##  $ OwnOccQrange         : num  111000 63600 37300 30100 35400 60400 26100 39200 38800 41400 ...
    ##  $ RentLowQ             : num  685 467 370 195 215 463 186 241 192 234 ...
    ##  $ RentMedian           : num  1001 560 428 250 280 ...
    ##  $ RentHighQ            : num  1001 672 520 309 349 ...
    ##  $ RentQrange           : num  316 205 150 114 134 361 139 146 177 142 ...
    ##  $ MedRent              : num  1001 627 484 333 340 ...
    ##  $ MedRentPctHousInc    : num  23.8 27.6 24.1 28.7 26.4 24.4 26.3 25.2 29.6 23.8 ...
    ##  $ MedOwnCostPctInc     : num  21.1 20.7 21.7 20.6 17.3 20.8 15.1 20.7 19.4 17.1 ...
    ##  $ MedOwnCostPctIncNoMtg: num  14 12.5 11.6 14.5 11.7 12.5 12.2 12.8 13 12.9 ...
    ##  $ NumInShelters        : num  11 0 16 0 327 0 21 125 43 1 ...
    ##  $ NumStreet            : num  0 0 0 0 4 0 0 15 4 0 ...
    ##  $ PctForeignBorn       : num  10.66 8.3 5 2.04 1.49 ...
    ##  $ PctBornSameState     : num  53.7 77.2 44.8 88.7 64.3 ...
    ##  $ PctSameHouse85       : num  65.3 71.3 36.6 56.7 42.3 ...
    ##  $ PctSameCity85        : num  78.1 90.2 61.3 90.2 70.6 ...
    ##  $ PctSameState85       : num  89.1 96.1 82.8 96.2 85.7 ...
    ##  $ LemasSwornFT         : num  NA NA NA NA NA NA NA NA 198 NA ...
    ##   [list output truncated]
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   population = col_double(),
    ##   ..   householdsize = col_double(),
    ##   ..   racepctblack = col_double(),
    ##   ..   racePctWhite = col_double(),
    ##   ..   racePctAsian = col_double(),
    ##   ..   racePctHisp = col_double(),
    ##   ..   agePct12t21 = col_double(),
    ##   ..   agePct12t29 = col_double(),
    ##   ..   agePct16t24 = col_double(),
    ##   ..   agePct65up = col_double(),
    ##   ..   numbUrban = col_double(),
    ##   ..   pctUrban = col_double(),
    ##   ..   medIncome = col_double(),
    ##   ..   pctWWage = col_double(),
    ##   ..   pctWFarmSelf = col_double(),
    ##   ..   pctWInvInc = col_double(),
    ##   ..   pctWSocSec = col_double(),
    ##   ..   pctWPubAsst = col_double(),
    ##   ..   pctWRetire = col_double(),
    ##   ..   medFamInc = col_double(),
    ##   ..   perCapInc = col_double(),
    ##   ..   whitePerCap = col_double(),
    ##   ..   blackPerCap = col_double(),
    ##   ..   indianPerCap = col_double(),
    ##   ..   AsianPerCap = col_double(),
    ##   ..   OtherPerCap = col_double(),
    ##   ..   HispPerCap = col_double(),
    ##   ..   NumUnderPov = col_double(),
    ##   ..   PctPopUnderPov = col_double(),
    ##   ..   PctLess9thGrade = col_double(),
    ##   ..   PctNotHSGrad = col_double(),
    ##   ..   PctBSorMore = col_double(),
    ##   ..   PctUnemployed = col_double(),
    ##   ..   PctEmploy = col_double(),
    ##   ..   PctEmplManu = col_double(),
    ##   ..   PctEmplProfServ = col_double(),
    ##   ..   PctOccupManu = col_double(),
    ##   ..   PctOccupMgmtProf = col_double(),
    ##   ..   MalePctDivorce = col_double(),
    ##   ..   MalePctNevMarr = col_double(),
    ##   ..   FemalePctDiv = col_double(),
    ##   ..   TotalPctDiv = col_double(),
    ##   ..   PersPerFam = col_double(),
    ##   ..   PctFam2Par = col_double(),
    ##   ..   PctKids2Par = col_double(),
    ##   ..   PctYoungKids2Par = col_double(),
    ##   ..   PctTeen2Par = col_double(),
    ##   ..   PctWorkMomYoungKids = col_double(),
    ##   ..   PctWorkMom = col_double(),
    ##   ..   NumKidsBornNeverMar = col_double(),
    ##   ..   PctKidsBornNeverMar = col_double(),
    ##   ..   NumImmig = col_double(),
    ##   ..   PctImmigRecent = col_double(),
    ##   ..   PctImmigRec5 = col_double(),
    ##   ..   PctImmigRec8 = col_double(),
    ##   ..   PctImmigRec10 = col_double(),
    ##   ..   PctRecentImmig = col_double(),
    ##   ..   PctRecImmig5 = col_double(),
    ##   ..   PctRecImmig8 = col_double(),
    ##   ..   PctRecImmig10 = col_double(),
    ##   ..   PctSpeakEnglOnly = col_double(),
    ##   ..   PctNotSpeakEnglWell = col_double(),
    ##   ..   PctLargHouseFam = col_double(),
    ##   ..   PctLargHouseOccup = col_double(),
    ##   ..   PersPerOccupHous = col_double(),
    ##   ..   PersPerOwnOccHous = col_double(),
    ##   ..   PersPerRentOccHous = col_double(),
    ##   ..   PctPersOwnOccup = col_double(),
    ##   ..   PctPersDenseHous = col_double(),
    ##   ..   PctHousLess3BR = col_double(),
    ##   ..   MedNumBR = col_double(),
    ##   ..   HousVacant = col_double(),
    ##   ..   PctHousOccup = col_double(),
    ##   ..   PctHousOwnOcc = col_double(),
    ##   ..   PctVacantBoarded = col_double(),
    ##   ..   PctVacMore6Mos = col_double(),
    ##   ..   MedYrHousBuilt = col_double(),
    ##   ..   PctHousNoPhone = col_double(),
    ##   ..   PctWOFullPlumb = col_double(),
    ##   ..   OwnOccLowQuart = col_double(),
    ##   ..   OwnOccMedVal = col_double(),
    ##   ..   OwnOccHiQuart = col_double(),
    ##   ..   OwnOccQrange = col_double(),
    ##   ..   RentLowQ = col_double(),
    ##   ..   RentMedian = col_double(),
    ##   ..   RentHighQ = col_double(),
    ##   ..   RentQrange = col_double(),
    ##   ..   MedRent = col_double(),
    ##   ..   MedRentPctHousInc = col_double(),
    ##   ..   MedOwnCostPctInc = col_double(),
    ##   ..   MedOwnCostPctIncNoMtg = col_double(),
    ##   ..   NumInShelters = col_double(),
    ##   ..   NumStreet = col_double(),
    ##   ..   PctForeignBorn = col_double(),
    ##   ..   PctBornSameState = col_double(),
    ##   ..   PctSameHouse85 = col_double(),
    ##   ..   PctSameCity85 = col_double(),
    ##   ..   PctSameState85 = col_double(),
    ##   ..   LemasSwornFT = col_double(),
    ##   ..   LemasSwFTPerPop = col_double(),
    ##   ..   LemasSwFTFieldOps = col_double(),
    ##   ..   LemasSwFTFieldPerPop = col_double(),
    ##   ..   LemasTotalReq = col_double(),
    ##   ..   LemasTotReqPerPop = col_double(),
    ##   ..   PolicReqPerOffic = col_double(),
    ##   ..   PolicPerPop = col_double(),
    ##   ..   RacialMatchCommPol = col_double(),
    ##   ..   PctPolicWhite = col_double(),
    ##   ..   PctPolicBlack = col_double(),
    ##   ..   PctPolicHisp = col_double(),
    ##   ..   PctPolicAsian = col_double(),
    ##   ..   PctPolicMinor = col_double(),
    ##   ..   OfficAssgnDrugUnits = col_double(),
    ##   ..   NumKindsDrugsSeiz = col_double(),
    ##   ..   PolicAveOTWorked = col_double(),
    ##   ..   LandArea = col_double(),
    ##   ..   PopDens = col_double(),
    ##   ..   PctUsePubTrans = col_double(),
    ##   ..   PolicCars = col_double(),
    ##   ..   PolicOperBudg = col_double(),
    ##   ..   LemasPctPolicOnPatr = col_double(),
    ##   ..   LemasGangUnitDeploy = col_double(),
    ##   ..   LemasPctOfficDrugUn = col_double(),
    ##   ..   PolicBudgPerPop = col_double(),
    ##   ..   ViolentCrimesPerPop = col_double()
    ##   .. )

``` r
# from the structure we can see that all of the variables are numberical,
# but "MedNumBR" can be a categorical variable since it only has 4 values:
# 1, 2, 3, and 4.
```

  - What are the general summary statistics of the data? How can these
    be
    visualized?

<!-- end list -->

``` r
summary(CC)
```

    ##    population      householdsize    racepctblack    racePctWhite  
    ##  Min.   :  10005   Min.   :1.600   Min.   : 0.00   Min.   : 2.68  
    ##  1st Qu.:  14359   1st Qu.:2.490   1st Qu.: 0.94   1st Qu.:75.88  
    ##  Median :  22681   Median :2.650   Median : 3.15   Median :89.61  
    ##  Mean   :  52251   Mean   :2.707   Mean   : 9.51   Mean   :83.49  
    ##  3rd Qu.:  43154   3rd Qu.:2.850   3rd Qu.:11.96   3rd Qu.:95.99  
    ##  Max.   :7322564   Max.   :5.280   Max.   :96.67   Max.   :99.63  
    ##                                                                   
    ##   racePctAsian      racePctHisp      agePct12t21     agePct12t29   
    ##  Min.   : 0.0300   Min.   : 0.120   Min.   : 4.58   Min.   : 9.38  
    ##  1st Qu.: 0.6125   1st Qu.: 0.920   1st Qu.:12.23   1st Qu.:24.38  
    ##  Median : 1.2400   Median : 2.340   Median :13.62   Median :26.77  
    ##  Mean   : 2.7508   Mean   : 8.482   Mean   :14.43   Mean   :27.62  
    ##  3rd Qu.: 2.7375   3rd Qu.: 8.610   3rd Qu.:15.39   3rd Qu.:29.18  
    ##  Max.   :57.4600   Max.   :95.290   Max.   :54.40   Max.   :70.51  
    ##                                                                    
    ##   agePct16t24      agePct65up       numbUrban          pctUrban     
    ##  Min.   : 4.64   Min.   : 1.660   Min.   :      0   Min.   :  0.00  
    ##  1st Qu.:11.34   1st Qu.: 8.922   1st Qu.:      0   1st Qu.:  0.00  
    ##  Median :12.54   Median :11.855   Median :  17348   Median :100.00  
    ##  Mean   :13.99   Mean   :12.005   Mean   :  46672   Mean   : 69.62  
    ##  3rd Qu.:14.36   3rd Qu.:14.547   3rd Qu.:  41932   3rd Qu.:100.00  
    ##  Max.   :63.62   Max.   :52.770   Max.   :7322564   Max.   :100.00  
    ##                                                                     
    ##    medIncome         pctWWage      pctWFarmSelf      pctWInvInc   
    ##  Min.   : 11576   Min.   :31.68   Min.   :0.0000   Min.   : 7.91  
    ##  1st Qu.: 23597   1st Qu.:73.22   1st Qu.:0.4700   1st Qu.:34.19  
    ##  Median : 30896   Median :78.38   Median :0.7000   Median :42.38  
    ##  Mean   : 33699   Mean   :78.08   Mean   :0.8933   Mean   :43.36  
    ##  3rd Qu.: 41215   3rd Qu.:83.70   3rd Qu.:1.1100   3rd Qu.:52.07  
    ##  Max.   :123625   Max.   :96.62   Max.   :6.5300   Max.   :89.04  
    ##                                                                   
    ##    pctWSocSec     pctWPubAsst       pctWRetire      medFamInc     
    ##  Min.   : 4.81   Min.   : 0.500   Min.   : 3.46   Min.   : 13785  
    ##  1st Qu.:20.98   1st Qu.: 3.362   1st Qu.:12.99   1st Qu.: 29307  
    ##  Median :26.79   Median : 5.720   Median :15.66   Median : 36010  
    ##  Mean   :26.66   Mean   : 6.806   Mean   :16.06   Mean   : 39553  
    ##  3rd Qu.:31.84   3rd Qu.: 9.150   3rd Qu.:18.78   3rd Qu.: 46683  
    ##  Max.   :76.39   Max.   :26.920   Max.   :45.51   Max.   :131315  
    ##                                                                   
    ##    perCapInc      whitePerCap     blackPerCap      indianPerCap   
    ##  Min.   : 5237   Min.   : 5472   Min.   :     0   Min.   :     0  
    ##  1st Qu.:11548   1st Qu.:12596   1st Qu.:  6706   1st Qu.:  6336  
    ##  Median :13977   Median :15028   Median :  9664   Median :  9834  
    ##  Mean   :15522   Mean   :16535   Mean   : 11472   Mean   : 12257  
    ##  3rd Qu.:17774   3rd Qu.:18610   3rd Qu.: 14464   3rd Qu.: 14690  
    ##  Max.   :63302   Max.   :68850   Max.   :212120   Max.   :480000  
    ##                                                                   
    ##   AsianPerCap      OtherPerCap       HispPerCap     NumUnderPov       
    ##  Min.   :     0   Min.   :     0   Min.   :    0   Min.   :     78.0  
    ##  1st Qu.:  8441   1st Qu.:  5500   1st Qu.: 7253   1st Qu.:    936.2  
    ##  Median : 12331   Median :  8144   Median : 9676   Median :   2217.5  
    ##  Mean   : 14284   Mean   :  9375   Mean   :10989   Mean   :   7398.4  
    ##  3rd Qu.: 17346   3rd Qu.: 11378   3rd Qu.:13360   3rd Qu.:   5097.5  
    ##  Max.   :106165   Max.   :137000   Max.   :54648   Max.   :1384994.0  
    ##                   NA's   :1                                           
    ##  PctPopUnderPov   PctLess9thGrade   PctNotHSGrad    PctBSorMore   
    ##  Min.   : 0.640   Min.   : 0.200   Min.   : 2.09   Min.   : 1.63  
    ##  1st Qu.: 4.692   1st Qu.: 4.770   1st Qu.:14.20   1st Qu.:14.09  
    ##  Median : 9.650   Median : 7.920   Median :21.66   Median :19.62  
    ##  Mean   :11.796   Mean   : 9.444   Mean   :22.70   Mean   :22.99  
    ##  3rd Qu.:17.078   3rd Qu.:12.245   3rd Qu.:29.66   3rd Qu.:28.93  
    ##  Max.   :48.820   Max.   :49.890   Max.   :73.66   Max.   :73.63  
    ##                                                                   
    ##  PctUnemployed      PctEmploy      PctEmplManu    PctEmplProfServ
    ##  Min.   : 1.320   Min.   :24.82   Min.   : 2.05   Min.   : 8.69  
    ##  1st Qu.: 4.090   1st Qu.:56.35   1st Qu.:11.94   1st Qu.:20.11  
    ##  Median : 5.485   Median :62.27   Median :16.66   Median :23.41  
    ##  Mean   : 6.024   Mean   :61.78   Mean   :17.79   Mean   :24.58  
    ##  3rd Qu.: 7.430   3rd Qu.:67.50   3rd Qu.:22.75   3rd Qu.:27.63  
    ##  Max.   :23.830   Max.   :84.67   Max.   :50.03   Max.   :62.67  
    ##                                                                  
    ##   PctOccupManu    PctOccupMgmtProf MalePctDivorce   MalePctNevMarr 
    ##  Min.   : 1.370   Min.   : 6.48    Min.   : 2.130   Min.   :12.06  
    ##  1st Qu.: 9.072   1st Qu.:21.92    1st Qu.: 7.162   1st Qu.:25.41  
    ##  Median :13.040   Median :26.30    Median : 9.240   Median :29.00  
    ##  Mean   :13.747   Mean   :28.25    Mean   : 9.180   Mean   :30.67  
    ##  3rd Qu.:17.465   3rd Qu.:32.89    3rd Qu.:11.110   3rd Qu.:33.47  
    ##  Max.   :44.270   Max.   :64.97    Max.   :19.090   Max.   :76.32  
    ##                                                                    
    ##   FemalePctDiv    TotalPctDiv      PersPerFam      PctFam2Par   
    ##  Min.   : 3.35   Min.   : 2.83   Min.   :2.290   Min.   :32.24  
    ##  1st Qu.: 9.94   1st Qu.: 8.64   1st Qu.:2.990   1st Qu.:67.67  
    ##  Median :12.63   Median :11.04   Median :3.095   Median :74.77  
    ##  Mean   :12.40   Mean   :10.88   Mean   :3.129   Mean   :73.90  
    ##  3rd Qu.:14.80   3rd Qu.:13.06   3rd Qu.:3.220   3rd Qu.:81.64  
    ##  Max.   :23.46   Max.   :19.11   Max.   :4.640   Max.   :93.60  
    ##                                                                 
    ##   PctKids2Par    PctYoungKids2Par  PctTeen2Par    PctWorkMomYoungKids
    ##  Min.   :26.11   Min.   : 27.43   Min.   :30.64   Min.   :24.42      
    ##  1st Qu.:63.62   1st Qu.: 74.42   1st Qu.:69.92   1st Qu.:55.45      
    ##  Median :72.06   Median : 83.77   Median :76.67   Median :60.70      
    ##  Mean   :70.91   Mean   : 81.75   Mean   :75.34   Mean   :60.43      
    ##  3rd Qu.:79.82   3rd Qu.: 91.44   3rd Qu.:82.52   3rd Qu.:65.80      
    ##  Max.   :92.58   Max.   :100.00   Max.   :97.34   Max.   :87.97      
    ##                                                                      
    ##    PctWorkMom    NumKidsBornNeverMar PctKidsBornNeverMar    NumImmig      
    ##  Min.   :41.95   Min.   :     0.0    Min.   : 0.000      Min.   :     20  
    ##  1st Qu.:64.96   1st Qu.:   146.2    1st Qu.: 1.083      1st Qu.:    407  
    ##  Median :69.25   Median :   361.0    Median : 2.080      Median :   1040  
    ##  Mean   :68.80   Mean   :  2041.5    Mean   : 3.140      Mean   :   6314  
    ##  3rd Qu.:73.34   3rd Qu.:  1070.2    3rd Qu.: 3.980      3rd Qu.:   3389  
    ##  Max.   :89.37   Max.   :527557.0    Max.   :24.190      Max.   :2082931  
    ##                                                                           
    ##  PctImmigRecent    PctImmigRec5    PctImmigRec8   PctImmigRec10  
    ##  Min.   : 0.000   Min.   : 0.00   Min.   : 0.00   Min.   : 0.00  
    ##  1st Qu.: 6.942   1st Qu.:11.70   1st Qu.:17.91   1st Qu.:23.54  
    ##  Median :12.440   Median :19.64   Median :27.46   Median :35.58  
    ##  Mean   :13.734   Mean   :20.83   Mean   :28.12   Mean   :35.48  
    ##  3rd Qu.:18.090   3rd Qu.:27.69   3rd Qu.:37.07   3rd Qu.:46.81  
    ##  Max.   :64.290   Max.   :76.16   Max.   :80.81   Max.   :88.00  
    ##                                                                  
    ##  PctRecentImmig    PctRecImmig5     PctRecImmig8    PctRecImmig10   
    ##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
    ##  1st Qu.: 0.180   1st Qu.: 0.290   1st Qu.: 0.410   1st Qu.: 0.540  
    ##  Median : 0.530   Median : 0.780   Median : 1.080   Median : 1.380  
    ##  Mean   : 1.149   Mean   : 1.781   Mean   : 2.424   Mean   : 3.094  
    ##  3rd Qu.: 1.370   3rd Qu.: 2.180   3rd Qu.: 2.870   3rd Qu.: 3.680  
    ##  Max.   :13.710   Max.   :19.930   Max.   :25.340   Max.   :32.630  
    ##                                                                     
    ##  PctSpeakEnglOnly PctNotSpeakEnglWell PctLargHouseFam  PctLargHouseOccup
    ##  Min.   : 6.15    Min.   : 0.000      Min.   : 0.960   Min.   : 0.440   
    ##  1st Qu.:83.70    1st Qu.: 0.510      1st Qu.: 3.390   1st Qu.: 2.360   
    ##  Median :91.78    Median : 0.955      Median : 4.290   Median : 3.050   
    ##  Mean   :86.55    Mean   : 2.538      Mean   : 5.465   Mean   : 3.975   
    ##  3rd Qu.:95.41    3rd Qu.: 2.467      3rd Qu.: 5.957   3rd Qu.: 4.280   
    ##  Max.   :98.98    Max.   :38.330      Max.   :34.870   Max.   :30.870   
    ##                                                                         
    ##  PersPerOccupHous PersPerOwnOccHous PersPerRentOccHous PctPersOwnOccup
    ##  Min.   :1.580    Min.   :1.610     Min.   :1.580      Min.   :13.93  
    ##  1st Qu.:2.400    1st Qu.:2.540     1st Qu.:2.120      1st Qu.:56.56  
    ##  Median :2.560    Median :2.700     Median :2.290      Median :64.99  
    ##  Mean   :2.614    Mean   :2.734     Mean   :2.382      Mean   :65.50  
    ##  3rd Qu.:2.770    3rd Qu.:2.890     3rd Qu.:2.540      3rd Qu.:75.30  
    ##  Max.   :4.520    Max.   :4.480     Max.   :4.730      Max.   :96.59  
    ##                                                                       
    ##  PctPersDenseHous PctHousLess3BR     MedNumBR       HousVacant      
    ##  Min.   : 0.050   Min.   : 3.06   Min.   :1.000   Min.   :    36.0  
    ##  1st Qu.: 1.300   1st Qu.:37.93   1st Qu.:2.000   1st Qu.:   310.0  
    ##  Median : 2.470   Median :46.78   Median :3.000   Median :   582.5  
    ##  Mean   : 4.325   Mean   :45.84   Mean   :2.626   Mean   :  1733.0  
    ##  3rd Qu.: 4.920   3rd Qu.:54.09   3rd Qu.:3.000   3rd Qu.:  1280.5  
    ##  Max.   :59.490   Max.   :95.34   Max.   :4.000   Max.   :172768.0  
    ##                                                                     
    ##   PctHousOccup   PctHousOwnOcc   PctVacantBoarded PctVacMore6Mos 
    ##  Min.   :37.47   Min.   :16.86   Min.   : 0.000   Min.   : 3.12  
    ##  1st Qu.:90.98   1st Qu.:54.09   1st Qu.: 0.780   1st Qu.:24.74  
    ##  Median :93.98   Median :62.08   Median : 1.740   Median :34.52  
    ##  Mean   :92.71   Mean   :62.63   Mean   : 2.791   Mean   :35.15  
    ##  3rd Qu.:95.91   3rd Qu.:71.59   3rd Qu.: 3.520   3rd Qu.:44.26  
    ##  Max.   :99.00   Max.   :96.36   Max.   :39.890   Max.   :82.13  
    ##                                                                  
    ##  MedYrHousBuilt PctHousNoPhone   PctWOFullPlumb   OwnOccLowQuart  
    ##  Min.   :1939   Min.   : 0.000   Min.   :0.0000   Min.   : 15700  
    ##  1st Qu.:1956   1st Qu.: 0.980   1st Qu.:0.1800   1st Qu.: 41800  
    ##  Median :1964   Median : 3.090   Median :0.3300   Median : 65900  
    ##  Mean   :1963   Mean   : 4.446   Mean   :0.4377   Mean   : 91116  
    ##  3rd Qu.:1971   3rd Qu.: 7.080   3rd Qu.:0.5700   3rd Qu.:126800  
    ##  Max.   :1987   Max.   :23.630   Max.   :5.3300   Max.   :500001  
    ##                                                                   
    ##   OwnOccMedVal    OwnOccHiQuart     OwnOccQrange       RentLowQ     
    ##  Min.   : 26600   Min.   : 36700   Min.   :     0   Min.   :  99.0  
    ##  1st Qu.: 56700   1st Qu.: 74800   1st Qu.: 32925   1st Qu.: 210.0  
    ##  Median : 84600   Median :109500   Median : 44250   Median : 305.0  
    ##  Mean   :116102   Mean   :149007   Mean   : 57891   Mean   : 328.1  
    ##  3rd Qu.:156250   3rd Qu.:192850   3rd Qu.: 67475   3rd Qu.: 420.0  
    ##  Max.   :500001   Max.   :500001   Max.   :331000   Max.   :1001.0  
    ##                                                                     
    ##    RentMedian       RentHighQ        RentQrange       MedRent      
    ##  Min.   : 120.0   Min.   : 182.0   Min.   :  0.0   Min.   : 192.0  
    ##  1st Qu.: 286.0   1st Qu.: 361.2   1st Qu.:139.0   1st Qu.: 363.0  
    ##  Median : 394.0   Median : 484.0   Median :173.0   Median : 467.0  
    ##  Mean   : 428.4   Mean   : 528.4   Mean   :200.3   Mean   : 502.7  
    ##  3rd Qu.: 547.8   3rd Qu.: 667.8   3rd Qu.:241.0   3rd Qu.: 621.0  
    ##  Max.   :1001.0   Max.   :1001.0   Max.   :803.0   Max.   :1001.0  
    ##                                                                    
    ##  MedRentPctHousInc MedOwnCostPctInc MedOwnCostPctIncNoMtg
    ##  Min.   :14.90     Min.   :14.10    Min.   :10.10        
    ##  1st Qu.:24.30     1st Qu.:19.10    1st Qu.:11.90        
    ##  Median :26.20     Median :21.20    Median :12.80        
    ##  Mean   :26.33     Mean   :21.21    Mean   :13.03        
    ##  3rd Qu.:28.10     3rd Qu.:23.30    3rd Qu.:13.80        
    ##  Max.   :35.10     Max.   :32.70    Max.   :23.40        
    ##                                                          
    ##  NumInShelters        NumStreet        PctForeignBorn   PctBornSameState
    ##  Min.   :    0.00   Min.   :    0.00   Min.   : 0.180   Min.   : 6.75   
    ##  1st Qu.:    0.00   1st Qu.:    0.00   1st Qu.: 2.080   1st Qu.:48.87   
    ##  Median :    0.00   Median :    0.00   Median : 4.490   Median :62.52   
    ##  Mean   :   67.72   Mean   :   18.71   Mean   : 7.606   Mean   :60.50   
    ##  3rd Qu.:   24.00   3rd Qu.:    1.00   3rd Qu.: 9.585   3rd Qu.:74.38   
    ##  Max.   :23383.00   Max.   :10447.00   Max.   :60.400   Max.   :93.14   
    ##                                                                         
    ##  PctSameHouse85  PctSameCity85   PctSameState85   LemasSwornFT    
    ##  Min.   :11.83   Min.   :27.95   Min.   :32.83   Min.   :   65.0  
    ##  1st Qu.:44.68   1st Qu.:71.92   1st Qu.:84.73   1st Qu.:  131.0  
    ##  Median :51.87   Median :79.31   Median :89.64   Median :  173.0  
    ##  Mean   :51.32   Mean   :77.11   Mean   :87.73   Mean   :  458.7  
    ##  3rd Qu.:58.51   3rd Qu.:84.70   3rd Qu.:92.73   3rd Qu.:  314.0  
    ##  Max.   :78.56   Max.   :96.59   Max.   :99.90   Max.   :25655.0  
    ##                                                  NA's   :1675     
    ##  LemasSwFTPerPop  LemasSwFTFieldOps LemasSwFTFieldPerPop LemasTotalReq    
    ##  Min.   :  29.4   Min.   :   14.0   Min.   :  19.21      Min.   :   8100  
    ##  1st Qu.: 149.1   1st Qu.:  113.5   1st Qu.: 130.43      1st Qu.:  49864  
    ##  Median : 196.0   Median :  152.0   Median : 170.16      Median :  89205  
    ##  Mean   : 248.1   Mean   :  395.9   Mean   : 211.32      Mean   : 240510  
    ##  3rd Qu.: 260.8   3rd Qu.:  283.0   3rd Qu.: 226.81      3rd Qu.: 174171  
    ##  Max.   :3437.2   Max.   :22496.0   Max.   :3290.62      Max.   :8328470  
    ##  NA's   :1675     NA's   :1675      NA's   :1675         NA's   :1675     
    ##  LemasTotReqPerPop PolicReqPerOffic  PolicPerPop     RacialMatchCommPol
    ##  Min.   :   2705   Min.   :  41.4   Min.   :  29.4   Min.   : 42.15    
    ##  1st Qu.:  65486   1st Qu.: 342.9   1st Qu.: 149.2   1st Qu.: 79.44    
    ##  Median :  91035   Median : 444.8   Median : 196.0   Median : 87.95    
    ##  Mean   : 122280   Mean   : 526.8   Mean   : 248.1   Mean   : 85.49    
    ##  3rd Qu.: 131894   3rd Qu.: 646.0   3rd Qu.: 260.8   3rd Qu.: 93.62    
    ##  Max.   :1926282   Max.   :2162.5   Max.   :3437.2   Max.   :100.00    
    ##  NA's   :1675      NA's   :1675     NA's   :1675     NA's   :1675      
    ##  PctPolicWhite    PctPolicBlack     PctPolicHisp    PctPolicAsian    
    ##  Min.   :  1.60   Min.   : 0.000   Min.   : 0.000   Min.   : 0.0000  
    ##  1st Qu.: 76.36   1st Qu.: 2.055   1st Qu.: 0.450   1st Qu.: 0.0000  
    ##  Median : 86.18   Median : 4.840   Median : 2.110   Median : 0.0000  
    ##  Mean   : 82.53   Mean   : 8.983   Mean   : 5.683   Mean   : 0.7088  
    ##  3rd Qu.: 93.09   3rd Qu.:13.355   3rd Qu.: 6.490   3rd Qu.: 0.6650  
    ##  Max.   :100.00   Max.   :67.310   Max.   :98.400   Max.   :18.5700  
    ##  NA's   :1675     NA's   :1675     NA's   :1675     NA's   :1675     
    ##  PctPolicMinor   OfficAssgnDrugUnits NumKindsDrugsSeiz PolicAveOTWorked
    ##  Min.   : 0.00   Min.   :   0.00     Min.   : 1.000    Min.   :  0.0   
    ##  1st Qu.: 5.05   1st Qu.:   6.00     1st Qu.: 7.000    1st Qu.: 55.1   
    ##  Median :11.39   Median :  12.00     Median : 9.000    Median : 99.0   
    ##  Mean   :15.20   Mean   :  25.87     Mean   : 8.784    Mean   :119.8   
    ##  3rd Qu.:19.68   3rd Qu.:  23.00     3rd Qu.:10.500    3rd Qu.:153.6   
    ##  Max.   :98.40   Max.   :1773.00     Max.   :15.000    Max.   :634.7   
    ##  NA's   :1675    NA's   :1675        NA's   :1675      NA's   :1675    
    ##     LandArea          PopDens      PctUsePubTrans     PolicCars     
    ##  Min.   :   0.90   Min.   :   10   Min.   : 0.000   Min.   :  20.0  
    ##  1st Qu.:   7.40   1st Qu.: 1171   1st Qu.: 0.350   1st Qu.:  54.0  
    ##  Median :  13.70   Median : 1996   Median : 1.220   Median :  86.0  
    ##  Mean   :  27.96   Mean   : 2790   Mean   : 3.063   Mean   : 177.3  
    ##  3rd Qu.:  25.77   3rd Qu.: 3270   3rd Qu.: 3.377   3rd Qu.: 191.0  
    ##  Max.   :3569.80   Max.   :44230   Max.   :54.330   Max.   :3187.0  
    ##                                                     NA's   :1675    
    ##  PolicOperBudg       LemasPctPolicOnPatr LemasGangUnitDeploy
    ##  Min.   :2.380e+06   Min.   :10.85       Min.   : 0.000     
    ##  1st Qu.:7.247e+06   1st Qu.:83.87       1st Qu.: 0.000     
    ##  Median :1.075e+07   Median :89.44       Median : 5.000     
    ##  Mean   :2.896e+07   Mean   :86.77       Mean   : 4.404     
    ##  3rd Qu.:2.047e+07   3rd Qu.:93.06       3rd Qu.:10.000     
    ##  Max.   :1.617e+09   Max.   :99.94       Max.   :10.000     
    ##  NA's   :1675        NA's   :1675        NA's   :1675       
    ##  LemasPctOfficDrugUn PolicBudgPerPop   ViolentCrimesPerPop
    ##  Min.   : 0.00       Min.   :  15260   Min.   :   0.0     
    ##  1st Qu.: 0.00       1st Qu.:  86869   1st Qu.: 161.7     
    ##  Median : 0.00       Median : 114582   Median : 374.1     
    ##  Mean   : 1.01       Mean   : 154590   Mean   : 589.1     
    ##  3rd Qu.: 0.00       3rd Qu.: 156961   3rd Qu.: 794.4     
    ##  Max.   :48.44       Max.   :2422367   Max.   :4877.1     
    ##                      NA's   :1675

  - Is the data normalized? Should it be normalized?

<!-- end list -->

``` r
# use Shapiro test to check normality:
sha = lapply(CC, shapiro.test)
sha1 = as.data.frame(t(sapply(sha, "[", c("statistic", "p.value"))))
sha2 = sha1 %>% select(p.value)
sh = setDT(sha2, keep.rownames = TRUE)[]
# check if any variables has p-value < 0.05, which means not normalized.
length(sh$p.value[sh$p.value < 0.05])
```

    ## [1] 125

``` r
# all the variables' p-value are less than 0.05, 
# so the data is not normalized.

# I think the data that have severe skewness should be normalize 
# since variables in great distince scales may affect the performance of fitted models.


# check for skewness:
skew = lapply(CC, skewness)
skew = sapply(skew, '[')
skewn = as.data.frame(skew)
skew1 = skewn %>% select(skew)
skew2 = setDT(skew1, keep.rownames = TRUE)[]
skew2
```

    ##                       rn      skew
    ##   1:          population 26.462913
    ##   2:       householdsize  1.692693
    ##   3:        racepctblack  2.263018
    ##   4:        racePctWhite -1.497464
    ##   5:        racePctAsian  4.746267
    ##  ---                              
    ## 121: LemasPctPolicOnPatr        NA
    ## 122: LemasGangUnitDeploy        NA
    ## 123: LemasPctOfficDrugUn  5.015527
    ## 124:     PolicBudgPerPop        NA
    ## 125: ViolentCrimesPerPop  2.063762

``` r
high.skew = c(colnames(CC[,1]), colnames(CC[,11]), colnames(CC[,24]),colnames(CC[,28]), colnames(CC[,50]), colnames(CC[,52]), colnames(CC[,72]), colnames(CC[,92]), colnames(CC[,93]), colnames(CC[,116]))

# from the table we can see that some of the variables have great skewness:
high.skew
```

    ##  [1] "population"          "numbUrban"           "indianPerCap"       
    ##  [4] "NumUnderPov"         "NumKidsBornNeverMar" "NumImmig"           
    ##  [7] "HousVacant"          "NumInShelters"       "NumStreet"          
    ## [10] "LandArea"

``` r
# these variables may need to normalize. some of the varibales are NA
# after calculated skewness.


# to normalize:
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}
newCC = CC
newCC[,1] = normalize(newCC[,1])
newCC[,11] = normalize(newCC[,11])
newCC[,24] = normalize(newCC[,24])
newCC[,28] = normalize(newCC[,28])
newCC[,50] = normalize(newCC[,50])
newCC[,52] = normalize(newCC[,52])
newCC[,72] = normalize(newCC[,72])
newCC[,92] = normalize(newCC[,92])
newCC[,93] = normalize(newCC[,93])
newCC[,116] = normalize(newCC[,116])
```

  - Are there missing values in the data? How should these missing
    values be
    handled?

<!-- end list -->

``` r
colSums(is.na(newCC))
```

    ##            population         householdsize          racepctblack 
    ##                     0                     0                     0 
    ##          racePctWhite          racePctAsian           racePctHisp 
    ##                     0                     0                     0 
    ##           agePct12t21           agePct12t29           agePct16t24 
    ##                     0                     0                     0 
    ##            agePct65up             numbUrban              pctUrban 
    ##                     0                     0                     0 
    ##             medIncome              pctWWage          pctWFarmSelf 
    ##                     0                     0                     0 
    ##            pctWInvInc            pctWSocSec           pctWPubAsst 
    ##                     0                     0                     0 
    ##            pctWRetire             medFamInc             perCapInc 
    ##                     0                     0                     0 
    ##           whitePerCap           blackPerCap          indianPerCap 
    ##                     0                     0                     0 
    ##           AsianPerCap           OtherPerCap            HispPerCap 
    ##                     0                     1                     0 
    ##           NumUnderPov        PctPopUnderPov       PctLess9thGrade 
    ##                     0                     0                     0 
    ##          PctNotHSGrad           PctBSorMore         PctUnemployed 
    ##                     0                     0                     0 
    ##             PctEmploy           PctEmplManu       PctEmplProfServ 
    ##                     0                     0                     0 
    ##          PctOccupManu      PctOccupMgmtProf        MalePctDivorce 
    ##                     0                     0                     0 
    ##        MalePctNevMarr          FemalePctDiv           TotalPctDiv 
    ##                     0                     0                     0 
    ##            PersPerFam            PctFam2Par           PctKids2Par 
    ##                     0                     0                     0 
    ##      PctYoungKids2Par           PctTeen2Par   PctWorkMomYoungKids 
    ##                     0                     0                     0 
    ##            PctWorkMom   NumKidsBornNeverMar   PctKidsBornNeverMar 
    ##                     0                     0                     0 
    ##              NumImmig        PctImmigRecent          PctImmigRec5 
    ##                     0                     0                     0 
    ##          PctImmigRec8         PctImmigRec10        PctRecentImmig 
    ##                     0                     0                     0 
    ##          PctRecImmig5          PctRecImmig8         PctRecImmig10 
    ##                     0                     0                     0 
    ##      PctSpeakEnglOnly   PctNotSpeakEnglWell       PctLargHouseFam 
    ##                     0                     0                     0 
    ##     PctLargHouseOccup      PersPerOccupHous     PersPerOwnOccHous 
    ##                     0                     0                     0 
    ##    PersPerRentOccHous       PctPersOwnOccup      PctPersDenseHous 
    ##                     0                     0                     0 
    ##        PctHousLess3BR              MedNumBR            HousVacant 
    ##                     0                     0                     0 
    ##          PctHousOccup         PctHousOwnOcc      PctVacantBoarded 
    ##                     0                     0                     0 
    ##        PctVacMore6Mos        MedYrHousBuilt        PctHousNoPhone 
    ##                     0                     0                     0 
    ##        PctWOFullPlumb        OwnOccLowQuart          OwnOccMedVal 
    ##                     0                     0                     0 
    ##         OwnOccHiQuart          OwnOccQrange              RentLowQ 
    ##                     0                     0                     0 
    ##            RentMedian             RentHighQ            RentQrange 
    ##                     0                     0                     0 
    ##               MedRent     MedRentPctHousInc      MedOwnCostPctInc 
    ##                     0                     0                     0 
    ## MedOwnCostPctIncNoMtg         NumInShelters             NumStreet 
    ##                     0                     0                     0 
    ##        PctForeignBorn      PctBornSameState        PctSameHouse85 
    ##                     0                     0                     0 
    ##         PctSameCity85        PctSameState85          LemasSwornFT 
    ##                     0                     0                  1675 
    ##       LemasSwFTPerPop     LemasSwFTFieldOps  LemasSwFTFieldPerPop 
    ##                  1675                  1675                  1675 
    ##         LemasTotalReq     LemasTotReqPerPop      PolicReqPerOffic 
    ##                  1675                  1675                  1675 
    ##           PolicPerPop    RacialMatchCommPol         PctPolicWhite 
    ##                  1675                  1675                  1675 
    ##         PctPolicBlack          PctPolicHisp         PctPolicAsian 
    ##                  1675                  1675                  1675 
    ##         PctPolicMinor   OfficAssgnDrugUnits     NumKindsDrugsSeiz 
    ##                  1675                  1675                  1675 
    ##      PolicAveOTWorked              LandArea               PopDens 
    ##                  1675                     0                     0 
    ##        PctUsePubTrans             PolicCars         PolicOperBudg 
    ##                     0                  1675                  1675 
    ##   LemasPctPolicOnPatr   LemasGangUnitDeploy   LemasPctOfficDrugUn 
    ##                  1675                  1675                     0 
    ##       PolicBudgPerPop   ViolentCrimesPerPop 
    ##                  1675                     0

``` r
# Yes, there are some missing values in the data. 
# These missing values are 1675 empty observations, which are hugh amount
# by comparing with total observations of each row(1994 obs).
# so I will exlude the missing values.

newCC = na.omit(newCC)
#after omitting the missing rows, we only have 319 observations of each columns.
```

  - Can the data be well-represented in fewer
dimensions?

<!-- end list -->

``` r
# fit a full simple model, and then drop the variables that have high Pr(>|t|).
# The asterisks following the Pr(>|t|) provide a visually accessible 
# way of assessing whether the statistic met various alpha criterion.

a = lm(ViolentCrimesPerPop~., data = newCC)
summary(a)
```

    ## 
    ## Call:
    ## lm(formula = ViolentCrimesPerPop ~ ., data = newCC)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1014.39  -242.57   -37.42   177.76  1801.76 
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                         Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)           -3.478e+03  1.731e+04  -0.201   0.8410   
    ## population            -7.570e+05  3.528e+05  -2.145   0.0331 * 
    ## householdsize          1.835e+03  1.056e+03   1.737   0.0839 . 
    ## racepctblack          -3.188e+00  1.671e+01  -0.191   0.8488   
    ## racePctWhite          -1.676e+00  1.588e+01  -0.105   0.9161   
    ## racePctAsian           4.850e+00  2.369e+01   0.205   0.8380   
    ## racePctHisp           -2.075e+00  1.328e+01  -0.156   0.8760   
    ## agePct12t21           -7.431e+01  9.315e+01  -0.798   0.4260   
    ## agePct12t29           -8.565e+01  7.597e+01  -1.127   0.2609   
    ## agePct16t24            8.868e+01  1.246e+02   0.712   0.4774   
    ## agePct65up            -7.704e+01  7.823e+01  -0.985   0.3260   
    ## numbUrban              7.563e+05  3.527e+05   2.144   0.0332 * 
    ## pctUrban              -2.109e+01  9.566e+00  -2.205   0.0286 * 
    ## medIncome             -1.065e-01  4.755e-02  -2.240   0.0262 * 
    ## pctWWage              -8.825e+01  4.056e+01  -2.176   0.0308 * 
    ## pctWFarmSelf          -4.227e+02  1.554e+02  -2.721   0.0071 **
    ## pctWInvInc            -3.825e+01  1.555e+01  -2.459   0.0148 * 
    ## pctWSocSec            -2.342e+01  3.860e+01  -0.607   0.5448   
    ## pctWPubAsst            1.147e+01  3.194e+01   0.359   0.7199   
    ## pctWRetire             1.844e+01  2.264e+01   0.814   0.4164   
    ## medFamInc              3.591e-02  3.984e-02   0.901   0.3685   
    ## perCapInc              1.858e-02  6.358e-02   0.292   0.7704   
    ## whitePerCap            1.122e-02  4.164e-02   0.269   0.7879   
    ## blackPerCap           -1.473e-02  1.904e-02  -0.774   0.4401   
    ## indianPerCap           1.225e+03  1.702e+03   0.720   0.4724   
    ## AsianPerCap            1.912e-03  8.704e-03   0.220   0.8264   
    ## OtherPerCap           -5.591e-03  7.822e-03  -0.715   0.4756   
    ## HispPerCap             5.233e-04  1.276e-02   0.041   0.9673   
    ## NumUnderPov           -1.905e+04  7.339e+03  -2.596   0.0102 * 
    ## PctPopUnderPov        -2.740e+01  3.010e+01  -0.910   0.3639   
    ## PctLess9thGrade       -2.918e+01  3.597e+01  -0.811   0.4183   
    ## PctNotHSGrad          -2.084e+01  2.640e+01  -0.790   0.4307   
    ## PctBSorMore            3.175e+01  2.076e+01   1.529   0.1278   
    ## PctUnemployed          7.279e+01  4.917e+01   1.480   0.1404   
    ## PctEmploy              4.973e+01  3.102e+01   1.603   0.1106   
    ## PctEmplManu           -6.138e+00  1.086e+01  -0.565   0.5727   
    ## PctEmplProfServ        9.302e+00  1.675e+01   0.555   0.5793   
    ## PctOccupManu           3.733e+01  2.399e+01   1.556   0.1213   
    ## PctOccupMgmtProf       1.022e+01  2.789e+01   0.366   0.7145   
    ## MalePctDivorce         5.359e+01  4.921e+02   0.109   0.9134   
    ## MalePctNevMarr        -6.601e+00  2.452e+01  -0.269   0.7880   
    ## FemalePctDiv           1.252e+02  5.667e+02   0.221   0.8254   
    ## TotalPctDiv           -2.777e+02  1.067e+03  -0.260   0.7949   
    ## PersPerFam            -6.415e+02  2.005e+03  -0.320   0.7493   
    ## PctFam2Par             2.289e+01  4.461e+01   0.513   0.6083   
    ## PctKids2Par           -8.068e+01  3.376e+01  -2.390   0.0178 * 
    ## PctYoungKids2Par      -5.962e+00  1.396e+01  -0.427   0.6698   
    ## PctTeen2Par            4.939e+00  1.354e+01   0.365   0.7157   
    ## PctWorkMomYoungKids    1.649e+00  2.108e+01   0.078   0.9377   
    ## PctWorkMom             7.816e+00  2.709e+01   0.289   0.7732   
    ## NumKidsBornNeverMar    1.427e+04  5.679e+03   2.513   0.0128 * 
    ## PctKidsBornNeverMar   -5.115e+01  4.685e+01  -1.092   0.2763   
    ## NumImmig               4.228e+03  2.798e+03   1.511   0.1324   
    ## PctImmigRecent         4.964e+01  2.350e+01   2.112   0.0359 * 
    ## PctImmigRec5          -7.854e+01  3.447e+01  -2.278   0.0238 * 
    ## PctImmigRec8           1.045e+01  2.822e+01   0.370   0.7115   
    ## PctImmigRec10          3.460e+01  1.687e+01   2.050   0.0417 * 
    ## PctRecentImmig        -4.629e+02  2.913e+02  -1.589   0.1137   
    ## PctRecImmig5           7.552e+02  4.118e+02   1.834   0.0682 . 
    ## PctRecImmig8          -4.535e+02  3.251e+02  -1.395   0.1646   
    ## PctRecImmig10         -4.740e+01  1.809e+02  -0.262   0.7935   
    ## PctSpeakEnglOnly      -1.226e+01  1.696e+01  -0.723   0.4706   
    ## PctNotSpeakEnglWell    2.564e+00  4.750e+01   0.054   0.9570   
    ## PctLargHouseFam        1.427e+02  1.617e+02   0.883   0.3785   
    ## PctLargHouseOccup     -2.944e+02  2.222e+02  -1.325   0.1866   
    ## PersPerOccupHous       1.786e+02  2.168e+03   0.082   0.9344   
    ## PersPerOwnOccHous      9.973e+02  1.279e+03   0.780   0.4366   
    ## PersPerRentOccHous    -1.539e+03  9.760e+02  -1.576   0.1165   
    ## PctPersOwnOccup       -1.095e+02  9.539e+01  -1.148   0.2524   
    ## PctPersDenseHous       9.553e+01  4.284e+01   2.230   0.0269 * 
    ## PctHousLess3BR         6.873e+00  1.195e+01   0.575   0.5659   
    ## MedNumBR              -6.402e+01  1.065e+02  -0.601   0.5485   
    ## HousVacant             1.487e+03  2.511e+03   0.592   0.5543   
    ## PctHousOccup           9.970e+00  2.015e+01   0.495   0.6214   
    ## PctHousOwnOcc          1.199e+02  9.651e+01   1.242   0.2156   
    ## PctVacantBoarded       1.159e+01  1.233e+01   0.939   0.3487   
    ## PctVacMore6Mos        -3.132e+00  5.846e+00  -0.536   0.5927   
    ## MedYrHousBuilt         7.883e+00  7.540e+00   1.045   0.2971   
    ## PctHousNoPhone         7.069e+00  2.683e+01   0.264   0.7924   
    ## PctWOFullPlumb        -2.614e+02  1.970e+02  -1.327   0.1859   
    ## OwnOccLowQuart        -3.883e-03  5.917e-03  -0.656   0.5125   
    ## OwnOccMedVal           4.696e-03  5.959e-03   0.788   0.4317   
    ## OwnOccHiQuart         -2.206e-03  2.320e-03  -0.951   0.3429   
    ## OwnOccQrange                  NA         NA      NA       NA   
    ## RentLowQ              -2.583e+00  1.782e+00  -1.450   0.1488   
    ## RentMedian             3.129e+00  3.894e+00   0.804   0.4226   
    ## RentHighQ             -2.660e+00  1.727e+00  -1.541   0.1250   
    ## RentQrange                    NA         NA      NA       NA   
    ## MedRent                4.607e+00  3.067e+00   1.502   0.1347   
    ## MedRentPctHousInc     -2.779e+01  3.178e+01  -0.874   0.3830   
    ## MedOwnCostPctInc      -3.765e+01  3.034e+01  -1.241   0.2161   
    ## MedOwnCostPctIncNoMtg  2.419e-01  3.856e+01   0.006   0.9950   
    ## NumInShelters          1.908e+03  3.539e+03   0.539   0.5905   
    ## NumStreet             -3.786e+01  3.571e+03  -0.011   0.9916   
    ## PctForeignBorn         4.570e+01  2.949e+01   1.549   0.1229   
    ## PctBornSameState      -4.248e+00  8.441e+00  -0.503   0.6154   
    ## PctSameHouse85         1.805e+01  1.526e+01   1.183   0.2383   
    ## PctSameCity85          4.381e-01  1.060e+01   0.041   0.9671   
    ## PctSameState85         1.213e+01  1.619e+01   0.749   0.4546   
    ## LemasSwornFT          -4.406e-01  7.907e-01  -0.557   0.5780   
    ## LemasSwFTPerPop        1.121e+02  1.037e+03   0.108   0.9140   
    ## LemasSwFTFieldOps      2.383e-01  6.969e-01   0.342   0.7328   
    ## LemasSwFTFieldPerPop  -3.210e-01  1.280e+00  -0.251   0.8022   
    ## LemasTotalReq         -2.344e-04  2.410e-04  -0.973   0.3319   
    ## LemasTotReqPerPop     -2.477e-04  7.845e-04  -0.316   0.7526   
    ## PolicReqPerOffic       3.207e-01  2.085e-01   1.538   0.1257   
    ## PolicPerPop           -1.116e+02  1.037e+03  -0.108   0.9144   
    ## RacialMatchCommPol    -3.202e+00  7.287e+00  -0.439   0.6609   
    ## PctPolicWhite         -5.943e-01  6.280e+00  -0.095   0.9247   
    ## PctPolicBlack         -3.806e+00  5.662e+01  -0.067   0.9465   
    ## PctPolicHisp           6.943e+00  5.524e+01   0.126   0.9001   
    ## PctPolicAsian         -1.079e+01  6.419e+01  -0.168   0.8667   
    ## PctPolicMinor         -7.422e+00  5.430e+01  -0.137   0.8914   
    ## OfficAssgnDrugUnits   -4.965e+00  2.968e+00  -1.673   0.0960 . 
    ## NumKindsDrugsSeiz      2.292e+00  1.203e+01   0.190   0.8491   
    ## PolicAveOTWorked      -6.784e-01  3.750e-01  -1.809   0.0719 . 
    ## LandArea               4.479e+03  1.753e+03   2.554   0.0114 * 
    ## PopDens               -2.231e-02  1.771e-02  -1.260   0.2092   
    ## PctUsePubTrans         8.639e+00  1.224e+01   0.706   0.4812   
    ## PolicCars              4.964e-01  3.663e-01   1.355   0.1769   
    ## PolicOperBudg          8.659e-06  4.977e-06   1.740   0.0834 . 
    ## LemasPctPolicOnPatr    3.596e+00  5.535e+00   0.650   0.5167   
    ## LemasGangUnitDeploy    5.547e+00  8.564e+00   0.648   0.5180   
    ## LemasPctOfficDrugUn    4.212e+00  1.000e+01   0.421   0.6741   
    ## PolicBudgPerPop       -4.331e-04  7.013e-04  -0.618   0.5376   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 451.8 on 196 degrees of freedom
    ## Multiple R-squared:  0.7959, Adjusted R-squared:  0.6689 
    ## F-statistic: 6.265 on 122 and 196 DF,  p-value: < 2.2e-16

``` r
# I will drop variables with Pr > 0.7 to avoid overfitting.

todrop = c("racepctblack", "racePctWhite", "racePctAsian", "racePctHisp", "pctWPubAsst", "perCapInc", "whitePerCap", "AsianPerCap", "HispPerCap", "PctOccupMgmtProf", "MalePctDivorce", "MalePctNevMarr", "FemalePctDiv", "TotalPctDiv", "PctTeen2Par", "PctWorkMomYoungKids", "PctWorkMom", "PctImmigRec8", "PctRecImmig10", "PctNotSpeakEnglWell", "PersPerOccupHous","PctHousNoPhone", "OwnOccQrange", "RentQrange", "MedOwnCostPctIncNoMtg", "NumStreet", "PctSameCity85", "LemasSwFTPerPop", "LemasSwFTFieldOps", "LemasSwFTFieldPerPop", "LemasTotReqPerPop", "PolicPerPop", "PctPolicWhite", "PctPolicBlack","PctPolicHisp", "PctPolicAsian", "PctPolicMinor","NumKindsDrugsSeiz")

length(todrop)
```

    ## [1] 38

``` r
newdat = newCC[,!names(newCC) %in% todrop]

# do the process again to drop more variables.
b=lm(ViolentCrimesPerPop~., data = newdat)
summary(b)
```

    ## 
    ## Call:
    ## lm(formula = ViolentCrimesPerPop ~ ., data = newdat)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -960.9 -235.3  -25.1  181.9 1988.8 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)         -2.462e+02  1.274e+04  -0.019  0.98459   
    ## population          -6.231e+05  3.096e+05  -2.012  0.04534 * 
    ## householdsize        1.868e+03  7.849e+02   2.380  0.01813 * 
    ## agePct12t21         -2.676e+01  6.912e+01  -0.387  0.69894   
    ## agePct12t29         -2.314e+01  6.138e+01  -0.377  0.70645   
    ## agePct16t24          1.710e+01  9.706e+01   0.176  0.86031   
    ## agePct65up           1.935e+01  5.299e+01   0.365  0.71539   
    ## numbUrban            6.234e+05  3.099e+05   2.012  0.04542 * 
    ## pctUrban            -1.737e+01  7.814e+00  -2.224  0.02714 * 
    ## medIncome           -9.301e-02  3.908e-02  -2.380  0.01812 * 
    ## pctWWage            -8.836e+01  3.015e+01  -2.931  0.00372 **
    ## pctWFarmSelf        -3.739e+02  1.273e+02  -2.937  0.00365 **
    ## pctWInvInc          -2.585e+01  1.090e+01  -2.372  0.01851 * 
    ## pctWSocSec          -4.926e+01  2.763e+01  -1.783  0.07592 . 
    ## pctWRetire           9.008e+00  1.759e+01   0.512  0.60909   
    ## medFamInc            4.279e-02  2.760e-02   1.550  0.12239   
    ## blackPerCap         -1.864e-02  1.539e-02  -1.211  0.22711   
    ## indianPerCap         1.253e+03  1.443e+03   0.868  0.38632   
    ## OtherPerCap         -6.203e-03  5.941e-03  -1.044  0.29757   
    ## NumUnderPov         -1.973e+04  6.096e+03  -3.237  0.00139 **
    ## PctPopUnderPov      -2.657e+01  2.324e+01  -1.143  0.25411   
    ## PctLess9thGrade     -3.663e+00  2.584e+01  -0.142  0.88738   
    ## PctNotHSGrad        -3.325e+01  2.155e+01  -1.543  0.12423   
    ## PctBSorMore          2.911e+01  1.252e+01   2.325  0.02095 * 
    ## PctUnemployed        6.388e+01  3.821e+01   1.672  0.09591 . 
    ## PctEmploy            4.710e+01  2.350e+01   2.004  0.04624 * 
    ## PctEmplManu         -6.074e+00  8.417e+00  -0.722  0.47119   
    ## PctEmplProfServ      9.360e+00  1.261e+01   0.742  0.45884   
    ## PctOccupManu         2.834e+01  1.723e+01   1.644  0.10146   
    ## PersPerFam          -3.283e+02  1.389e+03  -0.236  0.81334   
    ## PctFam2Par           4.359e+01  3.347e+01   1.302  0.19416   
    ## PctKids2Par         -6.822e+01  2.682e+01  -2.544  0.01162 * 
    ## PctYoungKids2Par    -9.924e+00  1.104e+01  -0.899  0.36967   
    ## NumKidsBornNeverMar  1.456e+04  4.771e+03   3.051  0.00255 **
    ## PctKidsBornNeverMar -2.000e+01  3.223e+01  -0.621  0.53547   
    ## NumImmig             4.298e+03  2.179e+03   1.973  0.04972 * 
    ## PctImmigRecent       4.626e+01  1.895e+01   2.441  0.01538 * 
    ## PctImmigRec5        -6.137e+01  2.189e+01  -2.803  0.00549 **
    ## PctImmigRec10        3.149e+01  9.600e+00   3.280  0.00120 **
    ## PctRecentImmig      -4.913e+02  2.329e+02  -2.109  0.03599 * 
    ## PctRecImmig5         6.639e+02  2.939e+02   2.259  0.02479 * 
    ## PctRecImmig8        -3.678e+02  1.437e+02  -2.559  0.01112 * 
    ## PctSpeakEnglOnly    -1.669e+01  7.444e+00  -2.242  0.02592 * 
    ## PctLargHouseFam      8.158e+01  1.119e+02   0.729  0.46671   
    ## PctLargHouseOccup   -2.270e+02  1.487e+02  -1.526  0.12829   
    ## PersPerOwnOccHous    1.120e+03  7.693e+02   1.455  0.14691   
    ## PersPerRentOccHous  -1.369e+03  7.997e+02  -1.712  0.08815 . 
    ## PctPersOwnOccup     -1.103e+02  7.734e+01  -1.427  0.15500   
    ## PctPersDenseHous     6.994e+01  3.182e+01   2.198  0.02892 * 
    ## PctHousLess3BR       7.580e+00  9.620e+00   0.788  0.43153   
    ## MedNumBR            -1.865e+01  9.233e+01  -0.202  0.84008   
    ## HousVacant           1.599e+03  2.048e+03   0.781  0.43584   
    ## PctHousOccup         2.471e+00  1.595e+01   0.155  0.87699   
    ## PctHousOwnOcc        1.208e+02  7.683e+01   1.573  0.11715   
    ## PctVacantBoarded     6.943e+00  1.041e+01   0.667  0.50538   
    ## PctVacMore6Mos      -2.870e+00  4.602e+00  -0.624  0.53354   
    ## MedYrHousBuilt       4.235e+00  5.916e+00   0.716  0.47475   
    ## PctWOFullPlumb      -2.999e+02  1.557e+02  -1.926  0.05537 . 
    ## OwnOccLowQuart      -2.461e-03  4.912e-03  -0.501  0.61679   
    ## OwnOccMedVal         3.902e-03  5.064e-03   0.771  0.44178   
    ## OwnOccHiQuart       -2.142e-03  1.831e-03  -1.170  0.24327   
    ## RentLowQ            -3.278e+00  1.397e+00  -2.346  0.01981 * 
    ## RentMedian           3.591e+00  3.252e+00   1.104  0.27065   
    ## RentHighQ           -2.255e+00  1.389e+00  -1.624  0.10581   
    ## MedRent              3.684e+00  2.661e+00   1.384  0.16757   
    ## MedRentPctHousInc   -1.629e+01  2.526e+01  -0.645  0.51977   
    ## MedOwnCostPctInc    -4.491e+01  2.294e+01  -1.958  0.05148 . 
    ## NumInShelters        1.934e+03  2.508e+03   0.771  0.44150   
    ## PctForeignBorn       2.433e+01  1.715e+01   1.419  0.15724   
    ## PctBornSameState    -1.783e+00  6.276e+00  -0.284  0.77665   
    ## PctSameHouse85       1.635e+01  1.107e+01   1.477  0.14109   
    ## PctSameState85       9.894e+00  1.282e+01   0.772  0.44114   
    ## LemasSwornFT        -2.282e-01  2.707e-01  -0.843  0.40011   
    ## LemasTotalReq       -1.976e-04  1.705e-04  -1.159  0.24769   
    ## PolicReqPerOffic     2.430e-01  1.146e-01   2.120  0.03506 * 
    ## RacialMatchCommPol  -7.964e+00  3.914e+00  -2.035  0.04301 * 
    ## OfficAssgnDrugUnits -4.951e+00  2.584e+00  -1.916  0.05656 . 
    ## PolicAveOTWorked    -7.175e-01  3.144e-01  -2.282  0.02340 * 
    ## LandArea             3.284e+03  1.417e+03   2.319  0.02129 * 
    ## PopDens             -2.535e-02  1.294e-02  -1.958  0.05140 . 
    ## PctUsePubTrans       1.146e+01  9.700e+00   1.181  0.23873   
    ## PolicCars            5.224e-01  3.081e-01   1.695  0.09133 . 
    ## PolicOperBudg        7.856e-06  3.054e-06   2.572  0.01073 * 
    ## LemasPctPolicOnPatr  4.594e+00  2.902e+00   1.583  0.11482   
    ## LemasGangUnitDeploy  4.492e+00  7.634e+00   0.588  0.55679   
    ## LemasPctOfficDrugUn  3.068e+00  8.603e+00   0.357  0.72174   
    ## PolicBudgPerPop     -1.692e-04  1.944e-04  -0.870  0.38497   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 426.8 on 232 degrees of freedom
    ## Multiple R-squared:  0.7845, Adjusted R-squared:  0.7046 
    ## F-statistic:  9.82 on 86 and 232 DF,  p-value: < 2.2e-16

``` r
todrop1 = c("agePct12t21", "agePct12t29", "agePct16t24", "agePct65up", "PctLess9thGrade","PersPerFam", "MedNumBR", "PctHousOccup", "PctBornSameState", "LemasPctOfficDrugUn", "pctWRetire", "OwnOccLowQuart")
newdat = newdat[,!names(newdat) %in% todrop1]
dim(newdat)
```

    ## [1] 319  75

``` r
summary(lm(ViolentCrimesPerPop~., data = newdat))
```

    ## 
    ## Call:
    ## lm(formula = ViolentCrimesPerPop ~ ., data = newdat)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -960.63 -234.04  -38.26  170.93 1991.94 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          4.669e+02  1.101e+04   0.042 0.966220    
    ## population          -5.725e+05  2.879e+05  -1.989 0.047873 *  
    ## householdsize        1.180e+03  4.661e+02   2.531 0.012013 *  
    ## numbUrban            5.732e+05  2.883e+05   1.988 0.047904 *  
    ## pctUrban            -1.614e+01  7.342e+00  -2.198 0.028908 *  
    ## medIncome           -7.953e-02  3.241e-02  -2.454 0.014841 *  
    ## pctWWage            -1.010e+02  2.525e+01  -4.002 8.35e-05 ***
    ## pctWFarmSelf        -3.623e+02  1.151e+02  -3.148 0.001848 ** 
    ## pctWInvInc          -2.396e+01  9.776e+00  -2.451 0.014945 *  
    ## pctWSocSec          -4.063e+01  2.045e+01  -1.987 0.048018 *  
    ## medFamInc            3.611e-02  2.295e-02   1.574 0.116895    
    ## blackPerCap         -2.180e-02  1.356e-02  -1.608 0.109143    
    ## indianPerCap         1.302e+03  1.373e+03   0.948 0.344079    
    ## OtherPerCap         -6.060e-03  5.663e-03  -1.070 0.285580    
    ## NumUnderPov         -1.856e+04  5.787e+03  -3.206 0.001524 ** 
    ## PctPopUnderPov      -4.063e+01  1.914e+01  -2.122 0.034818 *  
    ## PctNotHSGrad        -3.648e+01  1.139e+01  -3.203 0.001539 ** 
    ## PctBSorMore          2.768e+01  1.130e+01   2.449 0.015022 *  
    ## PctUnemployed        6.367e+01  3.314e+01   1.921 0.055896 .  
    ## PctEmploy            4.238e+01  1.875e+01   2.260 0.024714 *  
    ## PctEmplManu         -7.806e+00  7.848e+00  -0.995 0.320896    
    ## PctEmplProfServ      9.228e+00  1.185e+01   0.779 0.436968    
    ## PctOccupManu         2.979e+01  1.595e+01   1.868 0.062997 .  
    ## PctFam2Par           4.949e+01  2.970e+01   1.667 0.096884 .  
    ## PctKids2Par         -7.522e+01  2.365e+01  -3.181 0.001660 ** 
    ## PctYoungKids2Par    -9.588e+00  1.042e+01  -0.920 0.358545    
    ## NumKidsBornNeverMar  1.354e+04  4.550e+03   2.975 0.003219 ** 
    ## PctKidsBornNeverMar -1.504e+01  2.945e+01  -0.511 0.610046    
    ## NumImmig             3.668e+03  1.934e+03   1.896 0.059083 .  
    ## PctImmigRecent       4.815e+01  1.815e+01   2.652 0.008525 ** 
    ## PctImmigRec5        -6.550e+01  2.051e+01  -3.194 0.001588 ** 
    ## PctImmigRec10        3.259e+01  8.845e+00   3.684 0.000283 ***
    ## PctRecentImmig      -5.128e+02  2.194e+02  -2.337 0.020230 *  
    ## PctRecImmig5         7.078e+02  2.772e+02   2.554 0.011271 *  
    ## PctRecImmig8        -3.856e+02  1.358e+02  -2.841 0.004881 ** 
    ## PctSpeakEnglOnly    -1.510e+01  6.571e+00  -2.299 0.022372 *  
    ## PctLargHouseFam      4.914e+01  8.378e+01   0.587 0.558044    
    ## PctLargHouseOccup   -1.871e+02  1.299e+02  -1.440 0.151097    
    ## PersPerOwnOccHous    1.145e+03  6.640e+02   1.724 0.085933 .  
    ## PersPerRentOccHous  -1.302e+03  7.374e+02  -1.765 0.078747 .  
    ## PctPersOwnOccup     -1.118e+02  7.401e+01  -1.511 0.132028    
    ## PctPersDenseHous     7.786e+01  2.953e+01   2.637 0.008903 ** 
    ## PctHousLess3BR       6.529e+00  7.036e+00   0.928 0.354335    
    ## HousVacant           1.019e+03  1.591e+03   0.640 0.522483    
    ## PctHousOwnOcc        1.253e+02  7.304e+01   1.715 0.087579 .  
    ## PctVacantBoarded     8.592e+00  9.934e+00   0.865 0.387944    
    ## PctVacMore6Mos      -2.859e+00  4.159e+00  -0.687 0.492480    
    ## MedYrHousBuilt       4.631e+00  5.319e+00   0.871 0.384848    
    ## PctWOFullPlumb      -2.540e+02  1.444e+02  -1.759 0.079759 .  
    ## OwnOccMedVal         1.517e-03  1.970e-03   0.770 0.442008    
    ## OwnOccHiQuart       -1.538e-03  1.478e-03  -1.040 0.299201    
    ## RentLowQ            -3.247e+00  1.327e+00  -2.446 0.015133 *  
    ## RentMedian           4.142e+00  3.068e+00   1.350 0.178269    
    ## RentHighQ           -2.371e+00  1.288e+00  -1.841 0.066848 .  
    ## MedRent              2.989e+00  2.410e+00   1.240 0.216128    
    ## MedRentPctHousInc   -1.901e+01  2.368e+01  -0.803 0.423023    
    ## MedOwnCostPctInc    -4.118e+01  2.132e+01  -1.932 0.054531 .  
    ## NumInShelters        1.903e+03  2.327e+03   0.818 0.414271    
    ## PctForeignBorn       2.616e+01  1.612e+01   1.623 0.105917    
    ## PctSameHouse85       1.696e+01  9.766e+00   1.737 0.083734 .  
    ## PctSameState85       5.643e+00  7.775e+00   0.726 0.468682    
    ## LemasSwornFT        -2.381e-01  2.476e-01  -0.962 0.337182    
    ## LemasTotalReq       -1.736e-04  1.626e-04  -1.067 0.286820    
    ## PolicReqPerOffic     2.464e-01  1.104e-01   2.232 0.026512 *  
    ## RacialMatchCommPol  -8.382e+00  3.794e+00  -2.209 0.028109 *  
    ## OfficAssgnDrugUnits -4.651e+00  1.874e+00  -2.482 0.013733 *  
    ## PolicAveOTWorked    -7.468e-01  2.981e-01  -2.505 0.012897 *  
    ## LandArea             3.064e+03  1.328e+03   2.308 0.021846 *  
    ## PopDens             -2.649e-02  1.207e-02  -2.194 0.029144 *  
    ## PctUsePubTrans       1.070e+01  8.941e+00   1.197 0.232512    
    ## PolicCars            5.680e-01  2.901e-01   1.958 0.051414 .  
    ## PolicOperBudg        7.901e-06  2.899e-06   2.726 0.006884 ** 
    ## LemasPctPolicOnPatr  4.797e+00  2.756e+00   1.741 0.083014 .  
    ## LemasGangUnitDeploy  4.222e+00  7.233e+00   0.584 0.559986    
    ## PolicBudgPerPop     -1.845e-04  1.825e-04  -1.011 0.313252    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 418.4 on 244 degrees of freedom
    ## Multiple R-squared:  0.7821, Adjusted R-squared:  0.716 
    ## F-statistic: 11.83 on 74 and 244 DF,  p-value: < 2.2e-16

## Perform Regression Analysis

To develop a model to predict ViolentCrimesPerPop using the 124 features
(or some subset of them) stored in \(\mathbf{X}\), I’m going to try
several different methods, and use model selection methods to determine
which model is best. Also, keeping a held-out test set to evaluate the
performance of the model.

#### prepare train and test datasets

``` r
set.seed(123)
i = sample(nrow(newdat), 0.8*nrow(newdat), replace = F)
hold = setdiff(1:nrow(newdat), i)
train = newdat[i,]
test = newdat[hold,]
ytrain <- train$ViolentCrimesPerPop
xtrain  <- as.matrix(subset(train, select = -c(ViolentCrimesPerPop)))
ytest <- test$ViolentCrimesPerPop
xtest <- as.matrix(subset(test, select = -c(ViolentCrimesPerPop)))
```

#### Use simple linear regression

``` r
simple.fit = lm(ViolentCrimesPerPop~., data = train)
plot(simple.fit)
```

![](crime_and_communities_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->![](crime_and_communities_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->![](crime_and_communities_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

    ## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
    
    ## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced

![](crime_and_communities_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

``` r
simple.fit.sum = summary(simple.fit)
simple.fit.sum$r.squared
```

    ## [1] 0.7982077

``` r
simple.fit.sum$adj.r.squared
```

    ## [1] 0.7152487

``` r
# use predict to get the test mse.
simple.test.mse = mean((test$ViolentCrimesPerPop - predict.lm(simple.fit, test))^2)
```

#### Use lasso regression

``` r
lasso.fit = glmnet(xtrain, ytrain, alpha = 1)
plot(lasso.fit)
```

![](crime_and_communities_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
# use cross validation and compute test mse
set.seed(123)
cv.lasso = cv.glmnet(xtrain, ytrain, alpha = 1)
plot(cv.lasso)
```

![](crime_and_communities_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
bestlam = cv.lasso$lambda.min

lasso.pred = predict(lasso.fit, s= bestlam, newx=xtest)
lasso.test.mse = mean((lasso.pred - ytest)^2)
```

#### Use ridge regression

``` r
ridge.fit = glmnet(xtrain, ytrain, alpha = 0)
plot(ridge.fit)
```

![](crime_and_communities_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# use cross validation and compute test mse
set.seed(123)
cv.ridge = cv.glmnet(xtrain, ytrain, alpha = 0)
plot(cv.ridge)
```

![](crime_and_communities_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
bestlam = cv.ridge$lambda.min

ridge.pred = predict(ridge.fit, s= bestlam, newx=xtest)
ridge.test.mse = mean((ridge.pred - ytest)^2)
```

#### Use stepwise: forward method

``` r
step.fit = regsubsets(ViolentCrimesPerPop~., data = train, method = "forward", nvmax = 74)
step.fit.sum = summary(step.fit)
plot(step.fit.sum$bic, xlab = "number of variables", ylab = "BIC")
```

![](crime_and_communities_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
plot(step.fit.sum$rss, xlab = "number of variables", ylab = "RSS")
```

![](crime_and_communities_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
plot(step.fit.sum$adjr2, xlab = "number of variables", ylab = "adj_R2")
```

![](crime_and_communities_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

``` r
plot(step.fit.sum$cp, xlab = "number of variables", ylab = "CP")
```

![](crime_and_communities_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->

``` r
# display best 5 models
bic = step.fit.sum$bic
bic = sort(bic, decreasing = FALSE, index.return = TRUE)
best5 = head(bic$ix, 5)

# best 5 variable model for each is different
coef(step.fit, best5)
```

    ## [[1]]
    ##        (Intercept)        NumUnderPov        PctEmplManu 
    ##       4.245803e+03      -1.141443e+04      -1.508251e+01 
    ##        PctKids2Par           NumImmig   PctPersDenseHous 
    ##      -3.664142e+01       3.723766e+03       2.111013e+01 
    ##   PctVacantBoarded RacialMatchCommPol          PolicCars 
    ##       3.214424e+01      -1.042304e+01       9.928409e-01 
    ##      PolicOperBudg    PolicBudgPerPop 
    ##       3.081831e-06      -4.439996e-04 
    ## 
    ## [[2]]
    ##        (Intercept)        NumUnderPov        PctEmplManu 
    ##       4.612067e+03      -7.772751e+03      -1.350310e+01 
    ##        PctKids2Par   PctPersDenseHous RacialMatchCommPol 
    ##      -4.118286e+01       2.245908e+01      -1.056034e+01 
    ##          PolicCars      PolicOperBudg    PolicBudgPerPop 
    ##       1.093498e+00       2.977069e-06      -4.372299e-04 
    ## 
    ## [[3]]
    ##        (Intercept)        NumUnderPov        PctEmplManu 
    ##       4426.3394368      -2439.6288172        -13.7283960 
    ##        PctKids2Par   PctPersDenseHous RacialMatchCommPol 
    ##        -39.5615109         22.6127135        -10.2478362 
    ##          PolicCars 
    ##          0.8579014 
    ## 
    ## [[4]]
    ##        (Intercept)        NumUnderPov        PctEmplManu 
    ##       4.534608e+03      -3.008461e+03      -1.383386e+01 
    ##        PctKids2Par   PctPersDenseHous RacialMatchCommPol 
    ##      -4.013954e+01       2.203544e+01      -1.058259e+01 
    ##          PolicCars    PolicBudgPerPop 
    ##       1.006199e+00      -3.166304e-04 
    ## 
    ## [[5]]
    ##        (Intercept)        NumUnderPov        PctEmplManu 
    ##       4.206750e+03      -8.188984e+03      -1.338021e+01 
    ##        PctKids2Par   PctPersDenseHous   PctVacantBoarded 
    ##      -3.630143e+01       2.433632e+01       2.152199e+01 
    ## RacialMatchCommPol          PolicCars      PolicOperBudg 
    ##      -1.063799e+01       1.026239e+00       3.352617e-06 
    ##    PolicBudgPerPop 
    ##      -4.226090e-04

``` r
#find the best fit model from stepwise method
validation_errors <- vector("double", length = 74)
test_m = model.matrix(ViolentCrimesPerPop~., data = test)
for(i in 1:74) {
  coef_x <- coef(step.fit, id = i)                    
  pred_x <- test_m[ , names(coef_x)] %*% coef_x           
  validation_errors[i] <- mean((ytest - pred_x)^2)  
}

# plot validation errors
plot(validation_errors, type = "b")
```

![](crime_and_communities_files/figure-gfm/unnamed-chunk-11-5.png)<!-- -->

``` r
# get the best model from test mse
which.min(validation_errors)
```

    ## [1] 5

``` r
# so the 5th model is the best model using stepwise method.
```

#### Comparing the test mse for each methods

``` r
c(simple.test.mse, lasso.test.mse, ridge.test.mse, min(validation_errors))
```

    ## [1] 263390.7 217404.1 210479.7 217481.6

In conclusion, the smallest test mse among these methods is from ridge
regression. so the model from ridge regression is the best fit for the
data by comparing with above methods I use. However, since the test mse
is still large of the ridge regression model, there could be a better
model instead of the ridge. I would keep trying to find the better fit
model in the future.
