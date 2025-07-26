#!/bin/sh
#
# run "lda.R" using the script
#
#SBATCH --account=stats         # Replace ACCOUNT with your group account name
#SBATCH --job-name=lda     # The job name.
#SBATCH -c 1                     # The number of cpu cores to use
#SBATCH -t 00-00:55                 # Runtime in D-HH:MM
#SBATCH --mem-per-cpu=1gb         # The memory the job will use per cpu core
#SBATCH --output=/out/slurm-%A_%a.out  # save the .out file

export OMP_NUM_THREADS=1 # limit the number of threads to 1

module load R/4.1.0 # load R
 
#Command to execute R program
R CMD BATCH --no-save --vanilla lda.R /out/${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}.txt  # run "lda.R" and save the output information
 