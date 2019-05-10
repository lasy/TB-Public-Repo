#!/bin/bash
#
#SBATCH --job-name=Tender_breasts_ANALYSIS
#SBATCH --output=sbatch_output_Tender_breasts_ANALYSIS.out
#SBATCH --error=sbatch_output_Tender_breasts_ANALYSIS.err
#SBATCH --partition=hns,stat,normal
#SBATCH --qos=normal
#SBATCH --time=3:00:00
#SBATCH --nodes=1
#SBATCH --mem=32G



module load R/3.5.1

R -e "rmarkdown::render('Tender_breasts_ANALYSIS.Rmd')"
