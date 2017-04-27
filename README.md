[![Build Status](https://travis-ci.org/PalEON-Project/stepps-baconizing.svg?branch=master)](https://travis-ci.org/PalEON-Project/stepps-baconizing)

# Baconizing Midwestern US Age Models

This repository represents an effort by the PalEON Project to re-build age models from the (Neotoma Paleoecological Database)[http://neotomadb.org] using the Bayesian chronology method known as Bacon (Blaauw & Christen, 2001).

This work is used within the STEPPS pollen-prediction model, but also represents an intellectual contribution in its own right, detailled in the RMarkdown file `Baconizing_paper.Rmd`.

**NOTE**: In most cases the `travis` build for this repository will fail since a large number of required files are not linked to the repository.  We are attempting to solve this problem using `rds` save-files within R but this is an ongoing process.

## Development

  * Andria Dawson
    * University of Arizona
    * University of Wisconsin - Madison
  * Simon Goring - [webpage](http://goring.org)
    * Department of Geography, University of Wisconsin - Madison
  
## To use this Repository

### To Generate Age Models
To run the code to generate the age models:

1.  From the command line (within the current working directory):

```bash
Rscript ./setup_bacon.r
Rscript ./run_bacon.r
```

### To Generate the Paper

The paper can either be generated using RStudio's built in tools for rendering RMarkdown files, or from the commandline:

```bash
Rscript -e 'rmarkdown::render("Baconizing_paper.Rmd")'
```
