## About the scripts
Most of the R files come with a `.sh` file of the same name. The script is used to execute the R program on the HPC cluster.

## Code for simulations
Each code file corresponds to one simulation example in the paper:
- mtl_1.sh: Simulation 1 of MTL in Section 3.1 and S.5.1.1
- mtl_2.sh: Simulation 2 of MTL, the multi-component case in Section S.5.1.2
- mtl_3.sh: Simulation 3 of MTL in Section 5.1.3
- mtl_1_epsilon.sh: Simulation 4 of MTL in Section S.5.1.4, the case of varying contamination proportion
- mtl_epsilon.sh: Simulation 5 of MTL in Section S.5.1.4, another example of varying contamination proportion
- mtl_diff_R.sh: Simulation 6 of MTL in Section S.5.1.5, the case of different numbers of clusters across tasks
- mtl_repre.sh: Simulation 7 of MTL in Section S.5.1.5, the case of different numbers of clusters across tasks, but based on a shared low-rank structure
- misspecified_em.sh: Simulation 1 with misspecified cluster numbers, Figure S.5
- mtl_1_tuning_C.sh: Simulation 1 with different $C_{\lambda}$ tuning parameter values in Section S.5.1.7
- mtl_1_tuning_kappa.sh: Simulation 1 with different $\kappa$ tuning parameter values in Section S.5.1.8
- tl_1.sh: A transfer learning example in Section S.5.1.6


## Code for real-data studies
Each code file corresponds to one real-data example in the paper:
- har.sh: HAR dataset, binary clustering problem in Section 3.2
- har_multi.sh: HAR dataset, multi-cluster clustering problem in Section S.5.2.1
- pen.sh: PRHD dataset, binary clustering problem in Section S.5.2.2
- pen_multi.sh: PRHD dataset, multi-cluster clustering problem in Section S.5.2.2

## Code for producing the figures and tables
The code `result_summary.R` for producing the figures in the paper can be found in a separate folder "drawing_figure_table".  The code `results_summary_func.R` is imported for loading the output results in `result_summary.R`.
