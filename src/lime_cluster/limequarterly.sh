#!/bin/env bash
#SBATCH --job-name=limequarterly
#SBATCH --time=10:00:00
#SBATCH --mem=40G
#SBATCH --ntasks=1
#SBATCH --partition=comp
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=thiyanga.talagala@monash.edu
#SBATCH --output=limequarterly.txt
module load R/3.5.1
R --vanilla < limequarterly.R > limequarterly.txt