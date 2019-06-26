#!/bin/env bash
#SBATCH --job-name=m_overall_interaction
#SBATCH --time=10:00:00
#SBATCH --mem=50G
#SBATCH --ntasks=1
#SBATCH --partition=short,gpu,comp
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=thiyanga.talagala@monash.edu
#SBATCH --output=m_overall_interaction.txt
module load R/3.5.1
R --vanilla < m_overall_interaction.R > m_overall_interaction.txt