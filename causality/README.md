# Causality in Epidemiology demonstration data

This folder contains the synthetic smoking cohort used in the R practicals for *Causality in Epidemiology* by Jamalludin Ab Rahman.

## Files

- `smoking_cohort_demo.csv`: 3,000 fictional current smokers followed from baseline.
- `generate_demo_data.R`: reproducible R script used to generate the dataset.

The dataset contains no real participants and must not be used for clinical or policy decisions.

## Import directly into R

```r
data_url <- paste0(
  "https://raw.githubusercontent.com/",
  "profjamal/biostatistics/main/causality/smoking_cohort_demo.csv"
)
demo <- read.csv(data_url)
```

An internet connection is required for direct import. Readers using the Quarto project may use the bundled local copy when offline.
