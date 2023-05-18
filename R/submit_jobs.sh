#!/bin/bash  
#SBATCH --nodes=1 
#SBATCH --exclusive
#SBATCH --ntasks=125
#SBATCH --partition=thin
#SBATCH --time=35:00:00  
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=n.sekulovski@uva.nl

module purge    
module load 2022   
module load R/4.2.1-foss-2022a
export R_LIBS=$HOME/R:$R_LIBS

cp -r "$HOME"/Sim_clustering_1 "$TMPDIR" 

mkdir "$TMPDIR"/Sim_clustering_1/ 

cd "$TMPDIR"/Sim_clustering_1  

 
Rscript script.R  


cp -r "$TMPDIR"/Sim_clustering_1/estimates_bern.RDS "$HOME"/Sim_clustering_1 
cp -r "$TMPDIR"/Sim_clustering_1/estimates_bb.RDS "$HOME"/Sim_clustering_1
cp -r "$TMPDIR"/Sim_clustering_1/estimates_sbm.RDS "$HOME"/Sim_clustering_1