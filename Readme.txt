This repository contains the data and R scripts to replicate 
all graphs and analyses presented in the paper entitled 
"Novel cases of sluicing with mismatched antecedents: 
theoretical consequences" by Till Poppels and Andrew Kehler.

It contains the datasets in .RDS format, corresponding to the 
two experiments reported in the paper plus a norming experiment, 
as explained in the paper:
- expt-01-clean-data.rds
- expt-02-clean-data.rds
- norming-expt-clean-data.rds

The following scripts generate Figures 1-3 and 5 in the paper 
(Figure 4 is a screenshot):
- fig1.R
- fig2.R
- fig3.R
- fig5.R

All results described in the Section "Experiment 1: Tough mismatches" 
can be reproduced by running expt-01-analysis.R.

All results described in the Section "Experiment 2" (including those from 
the norming experiment) can be reproduced by running expt-02-analysis.R.

Finally, setup.R lists all required R packages for running the other scripts.