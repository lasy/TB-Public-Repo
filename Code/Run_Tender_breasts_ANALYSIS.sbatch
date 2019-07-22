#!/bin/bash
#
#SBATCH --job-name=Tender_breasts_ANALYSIS
#SBATCH --output=sbatch_output_Tender_breasts_ANALYSIS.out
#SBATCH --error=sbatch_output_Tender_breasts_ANALYSIS.err
#SBATCH --partition=hns,stat,normal
#SBATCH --qos=normal
#SBATCH --time=12:00:00
#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=8G
#SBATCH --mail-type=ALL

module load R/3.5.1

R -e "rmarkdown::render('Tender_breasts_ANALYSIS.Rmd')"
