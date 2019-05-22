#!/bin/env bash
#SBATCH --job-name=yFriedExplainer
#SBATCH --time=10:00:00
#SBATCH --mem=50G
#SBATCH --ntasks=1
#SBATCH --partition=short,comp,gpu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=thiyanga.talagala@monash.edu
#SBATCH --output=y_interactions_explainer.txt
module load R/3.5.1
R --vanilla < y_interactions_explainer.R > y_interactions_explainer.txt