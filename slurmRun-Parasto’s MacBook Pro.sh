#!/bin/bash -l

#each node has 48 cores*.

#We should also limit the wall time to what we might actually use, as I believe we get
#billed as if we used the entire amount.  2 hours and 30 minutes to be on the safe side.

#SBATCH --partition=defq
#SBATCH --job-name=hivncd-slurm
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=1
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1 
#SBATCH --output=outSlurm.out
#SBATCH --error=outSlurm.err
#SBATCH --mail-type=end
#SBATCH --mail-user=pkasaie@jh.edu
#SBATCH –array=1-10

rm /home/pkasaie/source/hivncd/rHivNcd/outputs/*
module load r
Rscript driver.R
