#!/bin/bash
#SBATCH --job-name=cog_noncog_pipeline
#SBATCH --time=08:00:00
#SBATCH --mem=16G
#SBATCH --cpus-per-task=1
#SBATCH --partition=regular
 
module load RPlus

# Set up a personal R library in your home directory
export R_LIBS_USER="$HOME/R/library"
mkdir -p "$R_LIBS_USER"
 
set -e  # Stop immediately if any Rscript call fails
 
echo "=== Installing missing R packages: $(date) ==="
Rscript -e '
  .libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))
  pkgs <- c("dplyr", "data.table", "lubridate", "magrittr", "ggplot2", "ggridges", "tidyr", "writexl", "fixest", "openxlsx", "broom", "car", "pwrss", "lmtest", "sandwich", "lme4")
  missing <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
  if (length(missing) > 0) install.packages(missing, lib=Sys.getenv("R_LIBS_USER"), repos="https://cloud.r-project.org")
'

echo "--- 1_1_Prepare_PGI_data ---"
bash 1_1_Prepare_PGI_data.sh

echo "--- 1_2_Prepare_PGIs ---"
Rscript 1_2_Prepare_PGIs.r

echo "--- 2_1_Prepare_CBCL ---"
Rscript 2_1_Prepare_CBCL.r

echo "--- 2_2_Prepare_YSR ---"
Rscript 2_2_Prepare_YSR.r

echo "--- 2_3_Combine_samples ---"
Rscript 2_3_Combine_samples.r

echo "--- 3_1_Behavioral_variables ---"
Rscript 3_1_Behavioral_variables.r

echo "--- 3_2_Descriptives ---"
Rscript 3_2_Descriptives.r

echo "--- 4_1_OLS_regressions ---"
Rscript 4_1_OLS_regressions.r

echo "--- 4_2_Results_tables ---"
Rscript 4_2_Results_tables.R

# echo "--- 5_Power_calculation ---"
# Rscript 5_Power_calculation.r

echo "=== Pipeline complete: $(date) ==="