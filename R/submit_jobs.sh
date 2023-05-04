#!/bin/bash  
#SBATCH --nodes=1 
#SBATCH --exclusive
#SBATCH --ntasks=125
#SBATCH --partition=thin
#SBATCH --time=26:00:00  # ? (estimate this)
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=n.sekulovski@uva.nl

module purge    
module load 2022   
module load R/4.2.1-foss-2022a
export R_LIBS=$HOME/R:$R_LIBS

cp -r "$HOME"/Sim12 "$TMPDIR" 

mkdir "$TMPDIR"/Sim12/ 

cd "$TMPDIR"/Sim12  

 
Rscript script.R  


cp -r "$TMPDIR"/Sim12/estimates_bern_bern.RDS "$HOME"/Sim12 
cp -r "$TMPDIR"/Sim12/estimates_bern_bb.RDS "$HOME"/Sim12
cp -r "$TMPDIR"/Sim12/estimates_bern_sbm.RDS "$HOME"/Sim12
cp -r "$TMPDIR"/Sim12/estimates_bb_bern.RDS "$HOME"/Sim12 
cp -r "$TMPDIR"/Sim12/estimates_bb_bb.RDS "$HOME"/Sim12
cp -r "$TMPDIR"/Sim12/estimates_bb_sbm.RDS "$HOME"/Sim12
cp -r "$TMPDIR"/Sim12/estimates_sbm_bern.RDS "$HOME"/Sim12 
cp -r "$TMPDIR"/Sim12/estimates_sbm_bb.RDS "$HOME"/Sim12
cp -r "$TMPDIR"/Sim12/estimates_sbm_sbm.RDS "$HOME"/Sim12