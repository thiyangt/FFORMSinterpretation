#!/bin/env bash
#SBATCH --job-name=limeyearly
#SBATCH --time=10:00:00
#SBATCH --mem=40G
#SBATCH --ntasks=1
#SBATCH --partition=short
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=thiyanga.talagala@monash.edu
#SBATCH --output=limeyearly.txt
module load R/3.5.1
R --vanilla < limeyearly.R > limeyearly.txt