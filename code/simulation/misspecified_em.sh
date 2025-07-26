#!/bin/sh
#
# Simple "Hello World" submit script for Slurm.
#
#SBATCH --account=stats         # Replace ACCOUNT with your group account name
#SBATCH --job-name=misspecified_em     # The job name.
#SBATCH -c 10                      # The number of cpu cores to use
#SBATCH -t 00-11:59                 # Runtime in D-HH:MM
#SBATCH --mem-per-cpu=1gb         # The memory the job will use per cpu core
#SBATCH --output=/burg/home/yt2661/trash/slurm-%A_%a.out

export OMP_NUM_THREADS=1

module load R
 
#Command to execute Python program
R CMD BATCH --no-save --vanilla misspecified_em.R /burg/home/yt2661/projects/MTL-GMM/experiments/misspecified_em/out/${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}.txt
 
#End of script