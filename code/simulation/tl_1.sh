#!/bin/sh
#
# Simple "Hello World" submit script for Slurm.
#
#SBATCH --account=stats         # Replace ACCOUNT with your group account name
#SBATCH --job-name=tl_1     # The job name.
#SBATCH -c 10                      # The number of cpu cores to use
#SBATCH -t 00-50:00                 # Runtime in D-HH:MM
#SBATCH --mem-per-cpu=1gb         # The memory the job will use per cpu core
#SBATCH --output=/burg/home/yt2661/trash/slurm-%A_%a.out

export OMP_NUM_THREADS=1

module load R
 
#Command to execute Python program
R CMD BATCH --no-save --vanilla tl_1.R /burg/home/yt2661/projects/MTL-GMM/experiments/tl_1/out/${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}.txt
 