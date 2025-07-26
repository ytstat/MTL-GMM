Reproducibility Guidance
================

This GitHub repository contains the code and the guidance to reproduce the numerical results in the paper "Neyman-pearson multi-class classification via cost-sensitive learning" by Ye Tian and Yang Feng.

## Description of the code
The code for simulations and real data studies are stored in two separate folders. There is another folder storing the code for producing the empirical plots in the paper, where we mention the related figure number for each produced plot in the code files. Each R file can be run by the `.sh` file with job ID input 1-500 as random seeds.

## Datasets
We include some small datasets in the folder "data". The original LendingClub dataset is very large, hence we include a pre-processed version used in our paper, and attach a Kaggle link to the original dataset with the code to pre-process the original data.

## Packages
The package `npcs` (version 0.2.0) can be installed from GitHub repository: [https://github.com/ytstat/npcs](https://github.com/ytstat/npcs). In R, we can use the `install_github` function from `devtools` package to install `npcs`.
```
library(devtools)
install_github("https://github.com/ytstat/npcs")
```

## Code running
We ran all experiments on the HPC cluster and include all the `.sh` files in the "code" folder. Each replication was done by setting up a unique random seed (1-500 for 500 replications, 1-200 for 200 replications), which will produce an individual `.RData` output. Since the number of outputs are numerous due to the large number of replications we have done for each simulation and real data study, we did not include the original output files in this repository.

## Approximate running time
We list the approximate running time for each script below.
### Simulation
- logistic.sh: 25 minutes
- lda.sh: 25 minutes
- knn.sh: 30 minutes
- nnb.sh: 25 minutes
- logistic_model1_check.sh: 1.5 hours
- knn_model1_check.sh: 2.5 hours

### Real data
- beans.sh: 100 seconds
- sat.sh: 60 seconds
- dementia.sh: 10 seconds
- loanclub.sh: 1 hour
- loanclub_logistic.sh: 7 hours
- loanclub_rf.sh: 10 hours

## References
Tian, Y., & Feng, Y. (2021). "[Neyman-pearson multi-class classification via cost-sensitive learning](https://arxiv.org/abs/2111.04597)." arXiv preprint arXiv:2111.04597.
