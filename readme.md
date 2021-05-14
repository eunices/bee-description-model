# Global trajectories of bee discovery

Eunice Jia Yu Soh, Luis Roman Carrasco, John Stoskopf Ascher*

*corresponding author: dbsajs@nus.edu.sg


![Manuscript figure](docs/img/ms.png)


---

## Introduction

Code for the paper **Global trajectories of bee discovery** which you may use for your own dataset. Model code was adapted from [Edie et al. (2017)](https://github.com/sedie/bayside) and diagnostic code was adapted from [@betanalpha](https://github.com/betanalpha/knitr_case_studies/blob/master/rstan_workflow/stan_utility.R).

## Environment setup

- Clone (using git)/download this repository to your workspace
- Install [RTools](https://cran.r-project.org/bin/windows/Rtools/history.html) which is required for RStan
- Install all libraries specified in `code/libraries.r`, including RStan. You may create a new library folder (if you don't wish to mess up your existing R libraries):

```r
.libPath(c('<your new library path>', .libPaths())) # add to front
install.packages(c(<packages to install>))
```

To revert to original library:

```r
libs <- .libPaths()
.libPaths(libs[2:length(libs)]) # removing the 1st/new library
```


## How to: general overview

- Add input data

- Run numbered steps sequentially `01-model.r` must be run first before `02-forecast.r` or `03-evaluate.r` but
- Files are persisted so dependent scripts (e.g. `02-forecast.r`) may be run in the future without having re-run `01-model.r`
- Some results are output as artifacts (`.csv`, `.pdf` files) others are printed to console.
- If the model parameters or datasets are changed, contents in `model/` should be deleted and `01-model.r` must be re-run.

Example pipeline for script:
```r
source('01-model.r')
source('02-forecast.r')
source('03-evaluate.r')

```

## Input data format

- For `data/data.csv`

```csv
valid_species_id,year,group
1,1878,NA
2,2004,NT
3,2004,NA
4,1869,NT
5,1861,NT
6,1966,NA
```

Description: species data. Each row is one species.

- `valid_species_id`: unique integer identifier for species
- `year`: year in which species was described/ "discovered"
- `group`: group in which the species belong to. Group may be a region (e.g. biogeographic realm) or a taxonomic rank (e.g. family)

- For `data/offset.csv` (**OPTIONAL**)

This file is optional. If not provided, offset matrix will contain zeroes.

```csv
year,group,N
1758,NA,0
1759,NA,0
1760,NA,0
1761,NA,0
1762,NA,0
1763,NA,0
1764,NA,0
```

Description: aggregated offset data (by publication or by describer, or any other metric of taxonomice effort) summarised by year

- `year`: year for taxonomic effort metric
- `group`: group for taxonomic effort metric
- `N`: number of units of units of taxonomic effort 

## How to: run the model

- Edit `model_params` in `params.r`
- Open R and run the following:

```r
source('01-model.r')
```
- Model fit and posterior simulation will be output into `model/`

note:  the duration increased with number of iterations and/or `adapt_delta` hyperparameters.

Benchmark timings, using the sample data (`data-example.csv` and `offset-example.csv`) with Intel Xeon W-2125 Processor with 32 GB RAM (tree depth 12, 4 chains):

| Iterations | Adapt delta | Duration   |
|------------|-------------|------------|
| 20000      | 0.95        | ~1.2 hours |


## How to: forecast using the model

- Edit `forecast_params` in `params.r`
- Open R and run the following:

```r
source('02-forecast.r')
```
- Forecast simulation will be output in `model/forecast/`

note: different forecasting windows will be output into the same folder (`model/forecast`) but with different filenames indicating the duration of forecast (in years).

## How to: evaluate the model

- Open R and run the following:

```r
source('03-evaluate.r')
```
- Model diagnostics and model fit (LOOAIC) will be printed out in the R console.

note: it is not uncommon for the model diagnostics to be violated. Sometimes increasing adapt_delta and the number of iterations helps, but this increases modelling duration.

## How to: choose an appropriate window for forecasting

- Open R and run the following:

```r
source('04-validate.r')
```

- Validation window chart using MAPE will be output to `model/validate/` folder.

For more information on the rationale of the validation method, refer to the paper. 

## Folder structure and important files

- `data/`: where data should be added. Two important files required: `data.csv` and `offset.csv`. Sample data provided are `data-example.csv` and `offset-example.csv` which may be renamed to `data.csv` and `offset.csv` respectively to test out the model.
- `params.r`: parameters that need to be set to run the entire modelling pipeline
- `01-model.r`: modelling code which calls code from `code/model/` to preprocess data from `data/` and run the stan model
- `02-forecast.r`: modelling code which calls code from `code/forecast/` for forecasting (into the future)
- `03-evaluate.r`: model evaluation code which calls code from `code/evaluate/` for diagnostics and model fit using LOOAIC
- `04-validate.r`: modelling code which calls code from `code/validate/` to determine the  appropriate window for forecasting
- `code/`: 
  - `code/model/`
  - `code/forecast/`
  - `code/evaluate/`
  - `code/validate/`
- `model/`: where model artifacts are persisted (and should be empty at first instance), including:
  - `model/count_info_ref.data.R`: data for in validation
  - `model/count_info.data.R`: data for modelling
  - `model/fit.data`: model fit
  - `model/post.data`: posterior samples (count data)
  - `model/warnings.log`: warnings logged from `01-model.r`. May include stan warnings
  - `model/model.log`: model log, including model parameters and duration taken to run the stan model
  - `model/results/*`: visualisations and summarised results (`csv` files) from the model
  - `model/forecast/forecast-<X>-yrs.data`: posterior forecast (count data) for `X` number of years into the future from the last date. Recommended to use 10 years.
  - `model/validate/*`: dataset and visualisation used in determining an appropriate window fore forecasting


## Session info

RTools version 3.5

```r
> sessionInfo()
R version 3.6.2 (2019-12-12)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19041)

Matrix products: default

locale:
[1] LC_COLLATE=English_Singapore.1252  LC_CTYPE=English_Singapore.1252   
[3] LC_MONETARY=English_Singapore.1252 LC_NUMERIC=C                      
[5] LC_TIME=English_Singapore.1252    

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods  
[8] base     

other attached packages:
[1] dplyr_0.8.3        plyr_1.8.5         data.table_1.12.8  loo_2.2.0         
[5] gamlss.dist_5.1-5  MASS_7.3-51.4      rstan_2.19.2       ggplot2_3.2.1     
[9] StanHeaders_2.19.0

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.3         pillar_1.4.3       compiler_3.6.2     prettyunits_1.1.0 
 [5] tools_3.6.2        pkgbuild_1.0.6     lifecycle_1.0.0    tibble_2.1.3      
 [9] gtable_0.3.0       checkmate_1.9.4    pkgconfig_2.0.3    rlang_0.4.10      
[13] cli_2.0.1          gridExtra_2.3      withr_2.1.2        stats4_3.6.2      
[17] grid_3.6.2         tidyselect_0.2.5   glue_1.4.2         inline_0.3.15     
[21] R6_2.4.1           processx_3.4.1     fansi_0.4.0        callr_3.4.0       
[25] purrr_0.3.3        magrittr_1.5       backports_1.1.5    scales_1.1.0      
[29] ps_1.3.0           matrixStats_0.55.0 assertthat_0.2.1   colorspace_1.4-1  
[33] lazyeval_0.2.2     munsell_0.5.0      crayon_1.3.4     
```
