#!/bin/env bash
#SBATCH --job-name=y_interactions
#SBATCH --time=10:00:00
#SBATCH --mem=50G
#SBATCH --ntasks=1
#SBATCH --partition=short
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=thiyanga.talagala@monash.edu
#SBATCH --output=y_interactions.txt
module load R/3.5.1
R --vanilla < y_interactions.R > y_interactions.txt