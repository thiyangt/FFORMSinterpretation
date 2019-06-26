#!/bin/env bash
#SBATCH --job-name=y_overall_interaction
#SBATCH --time=90:00:00
#SBATCH --mem=50G
#SBATCH --ntasks=1
#SBATCH --partition=gpu,comp
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=thiyanga.talagala@monash.edu
#SBATCH --output=y_overall_interaction.txt
module load R/3.5.1
R --vanilla < y_overall_interaction.R > y_overall_interaction.txt