Reproducibility Guidance
================

This GitHub repository contains the code and the guidance to reproduce the numerical results in the paper "Robust Unsupervised Multi-task and Transfer Learning on Gaussian Mixture Models" by Ye Tian, Haolei Weng, Lucy Xia, and Yang Feng.

## Description of the code
The code for simulations and real data studies are stored in two separate folders. There is another folder storing the code for producing the empirical plots in the paper, where we mention the related figure number for each produced plot in the code files. Each R file can be run by the `.sh` file with job ID input 1-200 as random seeds.

## Datasets
We include two datasets in the folder "data". 

## Packages
The package `mtlgmm` (version 0.2.0) can be installed from CRAN.
```
install.package("mtlgmm")
```
It can also be installed by the .tar.gz file attached in folder "package" via the following command in R:
```
install.package("mtlgmm_0.2.0.tar.gz", source = TRUE, repos = NULL)
```

## Code running
We ran all experiments on the HPC cluster and include all the `.sh` files in the "code" folder. Each replication was done by setting up a unique random seed (1-200 for 200 replications), which will produce an individual `.RData` output. The original output files can be found in folder "output".

## Approximate running time
We list the approximate running time for each script below.
### Simulation
- mtl_1.sh: 5 minutes
- mtl_2.sh: 2 hours
- mtl_3.sh: 10 minutes
- mtl_1_epsilon.sh: 25 minutes
- mtl_epsilon.sh: 40 minutes
- mtl_diff_R.sh: 40 minutes
- mtl_repre.sh: 10 minutes
- misspecified_em.sh: 4 hours
- mtl_1_tuning_C.sh: 10 minutes
- mtl_1_tuning_kappa.sh: 45 minutes
- tl_1.sh: 2 hours

### Real data
- har.sh: 5 minutes
- har_multi.sh: 7 hours
- pen.sh: 50 minutes
- pen_multi.sh: 4 hours

## References
Tian, Y., Weng, H., Xia, L., & Feng, Y. (2022). "[Robust unsupervised multi-task and transfer learning on gaussian mixture models](https://arxiv.org/abs/2209.15224)" arXiv preprint arXiv:2209.15224.
