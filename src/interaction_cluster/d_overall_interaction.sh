#!/bin/env bash
#SBATCH --job-name=d_overall_interaction
#SBATCH --time=10:00:00
#SBATCH --mem=50G
#SBATCH --ntasks=1
#SBATCH --partition=short,gpu,comp
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=thiyanga.talagala@monash.edu
#SBATCH --output=d_overall_interaction.txt
module load R/3.5.1
R --vanilla < d_overall_interaction.R > d_overall_interaction.txt