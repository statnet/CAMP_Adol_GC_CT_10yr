
setwd("C:/git/CAMP_10yr_proj/scripts/")  # Change depending on machine
rm(list=ls())
source("a10_import.R")                   # Get all inputs

calibstage <- 'first'                    # Do initial calibration of GC from the overall diagnoses 
source("a10_no_behav_change_script.R")
source("a10_gc_calibration_ABC.R")    ###### HANDRUN

minrun <- which(nbc_gc_ABC$stats==min(nbc_gc_ABC$stats))

part_prev_ratio_f <- as.vector(nbc_gc_ABC$param[minrun,1:3])
part_prev_ratio_m <- as.vector(nbc_gc_ABC$param[minrun,4:6])

calibstage <- 'second'                   # Do secondary calibration of GC from the age-sepcific diagnoses output from the first calibration
source("a10_no_behav_change_script.R")
source("a10_gc_calibration_ABC.R")    ###### HANDRUN
source("a10_gc_calibration_ABC_check.R")

