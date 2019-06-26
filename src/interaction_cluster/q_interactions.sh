#!/bin/env bash
#SBATCH --job-name=q_interactions
#SBATCH --time=10:00:00
#SBATCH --mem=40G
#SBATCH --ntasks=1
#SBATCH --partition=short
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=thiyanga.talagala@monash.edu
#SBATCH --output=q_interactions.txt
module load R/3.5.1
R --vanilla < q_interactions.R > q_interactions.txt