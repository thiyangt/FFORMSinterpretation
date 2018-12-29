#!/bin/env bash
#SBATCH --job-name=d_interactions
#SBATCH --time=10:00:00
#SBATCH --mem=40G
#SBATCH --ntasks=1
#SBATCH --partition=short
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=thiyanga.talagala@monash.edu
#SBATCH --output=d_interactions.txt
module load R/3.5.1
R --vanilla < d_interactions.R > d_interactions.txt