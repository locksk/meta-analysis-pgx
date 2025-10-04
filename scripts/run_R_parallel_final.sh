#!/bin/bash
#SBATCH --job-name=final_model_plots
#SBATCH --account=scw2063
#SBATCH -n 2
#SBATCH -c 1
#SBATCH -p c_compute_neuro1
#SBATCH --time=72:00:00
#SBATCH --mem=100G
#SBATCH --array=1-6
#SBATCH --mail-type=ALL
#SBATCH --mail-user=locksk@cardiff.ac.uk


start=`date +%s`

set -eu

echo "Running R Script"
echo

module load R/4.4.0

Rscript final/model_${SLURM_ARRAY_TASK_ID}.R

echo "Script complete: This is task $SLURM_ARRAY_TASK_ID of $SLURM_ARRAY_TASK_COUNT"

#OUTDIR=primary-plots
#mkdir -p $OUTDIR
#mv plot.pdf $OUTDIR/plot_${SLURM_ARRAY_TASK_ID}.pdf 

end=`date +%s`

runtime=$((end-start))

echo
echo "Script runtime (minutes) ="
echo "($runtime/60)" | bc -l

