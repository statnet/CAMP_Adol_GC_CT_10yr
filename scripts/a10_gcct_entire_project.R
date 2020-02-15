
########################################################################
## Code for manuscript "XXXXX"

setwd("C:/git/CAMP_10yr_proj/scripts/")  # Change depending on machine
rm(list=ls())

#install.packages("EasyABC")
library(EasyABC)
library(MASS)
set.seed(0)

########################################################################
### Inputs for GC and CT

source("a10_import.R")                          # Get all inputs
source("a10_process_inputs.R")                  # Process inputs (i.e. conduct regressions, etc.)
source("a10_make_behav_inputs_all_2007.R")      # override 2009-2017 numbers with 2007 for both calibration and no-behavior-change models
source("a10_process_dx_for_calib.R")            # create total (pseudo-age-specific) diagnosis numbers for step 1 of the calibration 
source("a10_ABC_gc.R")
source("a10_ABC_ct.R")

########################################################################
### Calibrate GC

source("a10_gc_calibration_pt1.R")              # GC calib pt 1 (starting with non-age-specific dx)

source("a10_gc_calibration_ABC_check.R")        # Load calibration check function
boxplot(a10_calib_gc_pt1$param)                 # Check pt 1 calibration
calib_test_gc(a10_calib_gc_pt1, 
  "../output/a10_calib_test_gc_step1_f.pdf", 
  "../output/a10_calib_test_gc_step1_m.pdf")

source("a10_gc_calibration_pt1_sim.R")          # simulate from GC calib pt 1 to get starting age-specific dx

source("a10_gc_calibration_pt2.R")              # GC calib pt 2 (starting with non-age-specific dx)

boxplot(a10_calib_gc_pt2$param)                 # Check pt 2 calibration
calib_test_gc(a10_calib_gc_pt2, 
  "../output/a10_calib_test_gc_step2_f.pdf", 
  "../output/a10_calib_test_gc_step2_m.pdf")

source("a10_gc_calibration_pt2_sim.R")          # GC calib pt 2 (starting with age-specific dx)


########################################################################
### Run GC scenarios and credible intervals

source("a10_gc_no_behav_change_script.R")       # No behavior change
source("a10_gc_obs_behav_change.R")             # Observed behavior change
source("a10_gc_credible_intervals.R")             # Observed behavior change


########################################################################
### Calibrate CT

source("a10_ct_calibration_pt1.R")              # CT calib pt 1 (starting with non-age-specific dx)

source("a10_ct_calibration_ABC_check.R")        # Load calibration check function
boxplot(a10_calib_ct_pt1$param)                 # Check pt 1 calibration
calib_test_ct(a10_calib_ct_pt1, 
              "../output/a10_calib_test_ct_step1_f.pdf", 
              "../output/a10_calib_test_ct_step1_m.pdf")

source("a10_ct_calibration_pt1_sim.R")          # simulate from CT calib pt 1 to get starting age-specific dx

source("a10_ct_calibration_pt2.R")              # CT calib pt 2 (starting with age-specific dx)

boxplot(a10_calib_ct_pt2$param)                 # Check pt 2 calibration
calib_test_ct(a10_calib_ct_pt2, 
              "../output/a10_calib_test_ct_step2_f.pdf", 
              "../output/a10_calib_test_ct_step2_m.pdf")

source("a10_ct_calibration_pt2_sim.R")          # CT calib pt 3 (starting with non-age-specific dx)

source("a10_ct_calibration_pt3.R")              # CT calib pt 3 (starting with age-specific dx)

boxplot(a10_calib_ct_pt3$param)                 # Check pt 3 calibration
calib_test_ct(a10_calib_ct_pt3, 
              "../output/a10_calib_test_ct_step3_f.pdf", 
              "../output/a10_calib_test_ct_step3_m.pdf")

source("a10_ct_calibration_pt3_sim.R")          # CT calib pt 3 (starting with non-age-specific dx)



########################################################################
### Run CT scenarios and credible intervals

source("a10_ct_no_behav_change_script.R")       # No behavior change
source("a10_ct_obs_behav_change.R")             # Observed behavior change
source("a10_ct_credible_intervals.R")             # Observed behavior change

########################################################
## Generate results for paper 

source("a10_results.R")

