#!/bin/env bash
#SBATCH --job-name=y_friedman_trend
#SBATCH --time=9:00:00
#SBATCH --mem=50G
#SBATCH --ntasks=1
#SBATCH --partition=short,gpu,comp
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=thiyanga.talagala@monash.edu
#SBATCH --output=y_friedman_trend.txt
module load R/3.5.1
R --vanilla < y_friedman_trend.R > y_friedman_trend.txt