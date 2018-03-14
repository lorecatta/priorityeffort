# priorityeffort

> Spatial priotization of conservation management effort

Optimization framework for spatial allocation of conservation management effort.

This package contains the data and R code underpinning the analysis presented in the following publication:

[Cattarino, Hermoso, Carwardine, Adams, Kennard and Linke. In press. Information uncertainty influences conservation outcomes when prioritizing multi-action management efforts. Journal of Applied Ecology. doi:10.1111/1365-2664.13147](http://onlinelibrary.wiley.com/doi/10.1111/1365-2664.13147/full)

Please contact l.cattarino@imperial.ac.uk or pull a request on this github project if you have any queries.

## Installation
```r
install.packages("devtools")
devtools::install_github("LorenzoCattarino/priorityeffort")
```

## Description

The `analysis` directory contains the scripts for running the two prioritization experiments described in the article:
* Species targets are linearly scaled to each species' area of occupancy (Experiment 1)
* Species targets are fixed for all species and the actual target value is varied (Experiment 2) 

The `analysis` directory contains also the scripts for post-processing the experiment results and generating the figures shown in the article and in the supporting information. Post-processing scripts assume that you had previosuly run the simulation experiments yourself, as experiments results have not been made available in this repository, due to file size limits. 
