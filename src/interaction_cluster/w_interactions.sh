#!/bin/env bash
#SBATCH --job-name=w_interactions
#SBATCH --time=10:00:00
#SBATCH --mem=40G
#SBATCH --ntasks=1
#SBATCH --partition=short
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=thiyanga.talagala@monash.edu
#SBATCH --output=w_interactions.txt
module load R/3.5.1
R --vanilla < w_interactions.R > w_interactions.txt