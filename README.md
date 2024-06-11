# Generous acts have contrasting meanings in equal versus hierarchical social relationships

Alicia Chen, Rebecca Saxe

Preprint: https://osf.io/preprints/psyarxiv/6kfwr

## Preregistrations

- Study 1
    - Study 1a: https://osf.io/fxwbh
    - Study 1b: https://osf.io/jvnca
- Study 2
    - Study 2a: https://osf.io/hmkg2
    - Study 2b: https://osf.io/4bhv6
    - Study 2c: https://osf.io/dfv24
- Study 3
    - Study 3a/c/d: https://osf.io/jcwpy
    - Study 3b: https://osf.io/c9t8g
- Study 4: https://osf.io/6qr39

*Note that we have changed the number of the studies from in the preregistrations.*

## This repository

- `experiments` contains the code for each experiment. Stimuli for each experiment are in `json/stimuli.json` under each experiment folder. You can access each experiment by visiting the `index.html` files (i.e on your server of choice). Between-participant condition assignment (for randomization) is handled using `php/participant_id_allocator.php`.
- `data` contains the anonymized data and demographics files for each experiment, accompanied by a codebook.
- `analysis` contains scripts for reproducing the analyses and figures reported in the paper.
    - To reproduce all analyses and figures at once, run `analysis/run_analyses.sh`. The outputs of the R scripts will be saved in `analysis/outputs`, and figures will be saved in `figures/outputs`.
    - In each script, the numerical analyses are reported in the same order as they appear in the manuscript.
- `figures` contains pdf files of our figures (both in one single big file and also in individual files)


## R session details

```{r}
sessionInfo()
R version 4.4.0 (2024-04-24)
Platform: aarch64-apple-darwin20
Running under: macOS Sonoma 14.4.1

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: America/New_York
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
 [1] afex_1.3-1        emmeans_1.10.1    glue_1.7.0        wesanderson_0.3.7 lmerTest_3.1-3    lme4_1.1-35.3     Matrix_1.7-0
 [8] ggthemes_5.1.0    tidyboot_0.1.1    lubridate_1.9.3   forcats_1.0.0     stringr_1.5.1     dplyr_1.1.4       purrr_1.0.2
[15] readr_2.1.5       tidyr_1.3.1       tibble_3.2.1      ggplot2_3.5.1     tidyverse_2.0.0   here_1.0.1

loaded via a namespace (and not attached):
 [1] gtable_0.3.5        lattice_0.22-6      tzdb_0.4.0          numDeriv_2016.8-1.1 vctrs_0.6.5         tools_4.4.0
 [7] generics_0.1.3      pbkrtest_0.5.2      parallel_4.4.0      fansi_1.0.6         pkgconfig_2.0.3     lifecycle_1.0.4
[13] farver_2.1.1        compiler_4.4.0      munsell_0.5.1       carData_3.0-5       crayon_1.5.2        pillar_1.9.0
[19] car_3.1-2           nloptr_2.0.3        MASS_7.3-60.2       boot_1.3-30         abind_1.4-5         nlme_3.1-164
[25] tidyselect_1.2.1    mvtnorm_1.2-4       stringi_1.8.3       reshape2_1.4.4      labeling_0.4.3      splines_4.4.0
[31] rprojroot_2.0.4     grid_4.4.0          colorspace_2.1-0    cli_3.6.2           magrittr_2.0.3      utf8_1.2.4
[37] broom_1.0.5         withr_3.0.0         scales_1.3.0        backports_1.4.1     timechange_0.3.0    estimability_1.5.1
[43] modelr_0.1.11       hms_1.1.3           coda_0.19-4.1       rlang_1.1.3         Rcpp_1.0.12         xtable_1.8-4
[49] pkgload_1.3.4       rstudioapi_0.16.0   minqa_1.2.6         R6_2.5.1            plyr_1.8.9
```


## Contact

aliciach@mit.edu

