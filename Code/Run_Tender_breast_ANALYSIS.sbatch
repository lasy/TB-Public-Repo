#!/bin/bash
#
#SBATCH --job-name=ExampleSbatch
#SBATCH --output=ExampleSbatch.out
#SBATCH --error=ExampleSbatch.err
#SBATCH --partition=normal
#SBATCH --qos=normal
#SBATCH --time=10:00:00
#SBATCH --nodes=1
#SBATCH --mem=20G

module load R/3.5.1

R -e "rmarkdown::render('Tender_breasts_ANALYSIS.Rmd')"
