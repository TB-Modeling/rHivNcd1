#!/bin/bash -l

#each node has 48 cores*.

#We should also limit the wall time to what we might actually use, as I believe we get
#billed as if we used the entire amount.  2 hours and 30 minutes to be on the safe side.

#SBATCH --partition=defq
#SBATCH --job-name=hivncd-slurm
#SBATCH --time=05:00:0
#SBATCH --cpus-per-task=2
#SBATCH --nodes=1
#SBATCH --mem-per-cpu=4GB
#SBATCH --array=1-10
#SBATCH --output=outputs/outSlurm_%a.out
#SBATCH --error=outputs/outSlurm_%a.err
#SBATCH --mail-type=end
#SBATCH --mail-user=pkasaie@jhu.edu


module load r
Rscript driver.R $SLURM_ARRAY_TASK_ID
