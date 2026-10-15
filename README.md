# Code for "Sociodemographic Disparities and Childhood Correlates of Loneliness Across 22 Countries"

This folder contains the code to reproduce the analyses, tables and figures in the paper and its
Supplementary Information, using
Wave 1 data from the Global Flourishing Study (GFS).

## Data

The GFS Wave 1 data are available through the Center for Open Science
(https://doi.org/10.17605/OSF.IO/3JTZ8; access instructions at https://www.cos.io/gfs-access-data).
The data cannot be redistributed here. Place the following files in `data/`:

| File                                     | Description                                                  |
| ---------------------------------------- | ------------------------------------------------------------ |
| `gfs_all_countries_wave1.dta`            | GFS Wave 1 data (all countries)                              |
| `GFSsample.df.imputed.long.wave1.csv`    | Imputed data for the sociodemographic analyses (created by `01`) |
| `GFSsample.df.imputed.CP.long.wave1.csv` | Imputed data for the childhood analyses (created by `02`)    |

The imputed datasets (5 imputations per country, chained equations with `mice`/`futuremice`,
seed 123) are created by `01` and `02` when `RUN_IMPUTATION <- TRUE` in `00_config.R`.
Imputation takes several hours; by default the scripts read previously created imputed datasets.

## Scripts

Run the scripts in order from this folder (the working directory). Outputs are written to `output/`.

| Script                                  | Produces                                                     |
| --------------------------------------- | ------------------------------------------------------------ |
| `00_config.R`                           | Shared settings (paths, imputation switch, labels); sourced by the R scripts |
| `01_sociodemographic_variation.R`       | Table 3; Supplementary Tables S1b–S22b; Table 1 and S1a–S22a (requires `gtsummary`); meta-analysis input |
| `02_childhood_correlates.R`             | Supplementary Tables S1c–S22c (regressions) and S1d–S22d (E-values); Table 2 and S1a–S22a (requires `gtsummary`); meta-analysis input |
| `03_meta_analysis.R`                    | Table 3 note (pooled mean); Tables 4–6; Supplementary Tables S23–S25 |
| `04_distribution_convergent_validity.R` | Supplementary Table S27; Supplementary Figure S65; pooled correlations reported in the Results |
| `05_main_figures.R`                     | Figures 1 and 2                                              |
| `06_SI_figures.py`                      | Supplementary Figures S1–S64 (`python3 06_SI_figures.py output output`) |

Frequency of loneliness is analysed on a 0 (never) to 10 (always) scale; the GFS item `LONELY`
is reverse-coded (10 − `LONELY`) in all scripts.

The final layout of the tables in the paper (column order, fonts, merged cells) was edited
in Microsoft Word; the numbers in the paper are those written by the scripts above.

## Software

Analyses were conducted in R (version 4.2.1) with survey (4.1.1), convey (0.2.5), mice (3.16.0),
future (1.32.0), EValue (4.1.3), metafor (3.8.1), harmonicmeanp, tidyverse (2.0.0),
haven (2.5.4), fastDummies (1.7.3), broom (1.0.11), gtsummary (2.5.0), flextable (0.9.4) and
officer (0.6.3). Supplementary Figures S1–S64 are drawn with Python 3 (matplotlib, pandas,
python-docx). The meta-analytic procedures in `03_meta_analysis.R` follow the GFS Wave 1
analysis code (https://doi.org/10.17605/OSF.IO/VBYPE; R package Rglobalflourishing).
