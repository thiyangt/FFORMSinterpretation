#!/bin/env bash
#SBATCH --job-name=limemonthly
#SBATCH --time=10:00:00
#SBATCH --mem=40G
#SBATCH --ntasks=1
#SBATCH --partition=comp
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=thiyanga.talagala@monash.edu
#SBATCH --output=limemonthly.txt
module load R/3.5.1
R --vanilla < limemonthly.R > limemonthly.txt