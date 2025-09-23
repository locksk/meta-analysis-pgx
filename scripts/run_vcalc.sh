#!/bin/bash
#SBATCH --job-name=vcalc_plots_
#SBATCH --account=scw2063
#SBATCH -n 2
#SBATCH -c 1
#SBATCH -p c_compute_neuro1
#SBATCH --time=72:00:00
#SBATCH --mem=100G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=locksk@cardiff.ac.uk


start=`date +%s`

set -eu

echo "Running R Script"
echo

module load R/4.4.0

Rscript final/vcalc.R

end=`date +%s`

runtime=$((end-start))
echo
echo "Script runtime (minutes) ="
echo "($runtime/60)" | bc -l


