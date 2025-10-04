# Meta Analysis - Pharmacogenomic Effect Sizes



## Files
* `pgx-meta-analysis.html` - rmd version of manuscript including all code for running of meta-analyses and associated methods. A version of this in .rmd format is also available in the repository. The live version of this file is located [here](https://locksk.github.io/pgx-effects/)
* `calculate_SMDs.html` - contains methods and code for calculating the standardised mean difference (SMD) for all studies included in the meta-analysis. A version of this in .rmd format is also available in the repository. 
* `all_results.xlsx` - containes reported effects (i.e., mean, median, observations, OR, beta, *t*) for studies included in the meta-analysis. These were used to calculate SMDs as described in `calculate_SMDs.html`.
* file 3 - 
* `/app` - directory containing files for the companion shiny app. The live version of the Shiny app is hosted [here](https://locksk.shinyapps.io/pgx-effect-sizes/). 
    * `R/app.R` - shiny app file (server + ui together)
    * `R/functions.R` - helper functions for shiny app (also used in pgx-meta-analysis.html)
    * `R/data/all_smds.RData` - effect sizes generated from `calculate_SMDs.html` and used for analysis in  `pgx-meta-analysis.html`.
    * `R/www` - contains images used in shiny app.
* `/scripts` - directory containing computationally intensive scripts (not run within the markdown). 
    * `run_R_parallel_final.sh` - runs `R/model_*.R` scripts in parallel (profile plots).
    * `run_vcalc.sh` - runs `R/vcalc.R` script (sensitivity analysis)

## Data and Packages

* Effect sizes from 184 studies were included in the analysis. Extracted data is found in `all_results.xlsx`. Effect size calculation was guided by the [Cochrane handbook](https://www.cochrane.org/authors/handbooks-and-manuals/handbook/current), particularly chapters 6 and 10. The effect size conversion / [esc](https://cran.r-project.org/web/packages/esc/readme/README.html) R package.
* [PyPGx](https://github.com/sbslee/pypgx/tree/master) and [PharmGKB](https://www.clinpgx.org/page/pgxGeneRef) databases informed pharamcogenomic allele and metabolism phenotype calling. 
* Meta-Analyses were fit using [metafor](https://www.metafor-project.org/doku.php/metafor) and [clubSandwich](https://jepusto.github.io/clubSandwich/), for robust variance estimation. 
* Bayseian analysis to determine effect size shrinkage was based on [van Zwet & Gelman, 2022](https://doi.org/10.1080/00031305.2021.1938225)
* The companion app was built using [RShiny](https://shiny.posit.co/) and hosted using [Docker](https://www.docker.com/)
