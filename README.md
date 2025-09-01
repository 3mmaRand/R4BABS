# Data Analysis in R for Becoming a Bioscientist (R4BABS)

R4BABS is a Quarto-powered website that gathers teaching materials for the University of York Biology curriculum. The site renders into the `_site` directory and offers navigation for BABS 1, BABS 2, BABS 4, and the postgraduate course PGT 52M.

## Teaching Materials

- **BABS 1** – Stage 1 module taken by every student. Covers core concepts of scientific computing, types of variables, the role of variables in analysis, and how to use RStudio to organise work and import, summarise, and plot data.
- **BABS 2** – Stage 1 module building on BABS 1. Introduces classical statistics, including the logic of hypothesis testing, confidence intervals, statistical models, two-sample tests, and one- and two-way ANOVA.
- **BABS 4** – Stage 2 module. Starts with a review of stage 1 skills (file types, file systems, working directories, paths, and RStudio Projects) then covers project organisation, working with datasets that have many variables and observations, getting an overview with summaries and distribution plots, and filtering rows and columns. The final three workshops, aimed at Biomedical Science students, teach flow cytometry data analysis.
- **PGT 52M** – Postgraduate module revisiting core concepts before progressing through hypothesis testing, regression, ANOVA, and assessment preparation.

## Getting Started

1. **Restore the R environment**
   ```r
   renv::restore()
   ```
   Targets R 4.5.0 with Bioconductor 3.21 repositories.

2. **Render the site**
   ```bash
   quarto render       # build the entire site
   quarto preview      # serve locally with live reload
   ```

## Continuous Deployment

A GitHub Actions workflow builds and deploys the rendered site to the `gh-pages` branch whenever changes land on `main`.

## Repository Layout

- `_quarto.yml` – project configuration
- `r4babs1/`, `r4babs2/`, `r4babs4/`, `pgt52m/` – module content organised by week
- `renv.lock`, `renv/` – reproducible R package environment
- `CITATION.cff` – citation metadata
- `.github/workflows/` – deployment workflow

## Citation

> Rand, E. *Data Analysis in R for Becoming a Bioscientist* (Version 0.1).
> https://3mmarand.github.io/R4BABS/

