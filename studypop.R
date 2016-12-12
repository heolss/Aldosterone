# Header
# Filename: studypop
# Created: 20161212
# Updated: 20160527 (updated variables) 
# Author: Henrik Olsson
# Purpose: Generate variables and study data
# Data used: swedeheart.raw (rhia_epc_final.sav), RHIA_LKML_JERN.sav
# Output: studypop


rm(list = ls())

library("haven")
library("dplyr")
# library("lubridate")

## help functions for retaining attributes in haven data structures (fix of bug in haven as_factor)
is_labelled <- function(x) inherits(x, "labelled")
as_factor <- function(x, ...) UseMethod("as_factor")
as_factor.labelled <- function(x, levels = c("labels", "values"),
                               ordered = FALSE, ...) {
    levels <- match.arg(levels)
    
    if (is.character(x)) {
        levs <- unname(attr(x, "labels"))
        labs <- switch(
            levels,
            labels = names(attr(x, "labels")),
            values = levs
        )
        f <- factor(x, levs, labels = labs, ordered = ordered)[drop = TRUE]
        attr(f, "label") <- attr(x, "label")
        f
    } else {
        labs <- attr(x, "labels")
        f <- factor(x, levels = unname(labs), labels = names(labs))[drop = TRUE]
        attr(f, "label") <- attr(x, "label")
        f
    }
}

setwd("C:/Users/henols/Desktop/Swedeheart/Aldosterone")

## Swedeheart raw data
swedeheart_raw <- readRDS(file.path("Data", "Original data", "swedeheart_raw.rds"))

## Drug treatment data
RHIA_LKML_JERN <- read_spss(file.path("Data", "Original data", "RHIA_LKML_JERN.sav"))
RHIA_LKML_JERN <- subset(RHIA_LKML_JERN, select = c("idnr", "id_rhia", "C03DA01_6mb", "C03DA04_6mb", "C03DA01_2w", "C03DA04_2w"))

## add aldosterone treatment - treatment on admission
head(swedeheart_raw); head(RHIA_LKML_JERN)
swedeheart_raw <- left_join(swedeheart_raw, RHIA_LKML_JERN, by = c("id_rhia", "id_rhia"))
names(swedeheart_raw) <- tolower(names(swedeheart_raw))

## study cohort (N = 161989)
# 2005 - 2014
# Acute MI
# First visit per patient
studypop <- swedeheart_raw %>%
    filter(d_year_num >= 2005 & d_ami == 1) %>%
    rename(idnr = idnr.x) %>%
    group_by(idnr) %>%
    arrange(admission_date) %>%
    filter(row_number() == 1)

# FLAG previous heart disease (a little weird but based on their code & documentation)
studypop <- within(studypop, {
    prev_HF <- NA_integer_
    prev_HF[history_of_chf %in% c(1, 2, 3, 4, 5) | kom_hsvikt == 1] <- 1L
    prev_HF[pulmonary_rales %in% c(1, 2, 3) | cardiac_shock == 1] <- 1L
    prev_HF[d_diuretics_treat == 1 | d_inotropes == 1 | d_left_ventricular_function %in% c(2, 3, 4) | 
                d_cpap == 1 | d_cardiogenic_shock == 1] <- 1L
})

table(studypop$prev_HF, useNA = "i")

## study cohort (N = 74740)
# Alive 14 days after discharge (befdoddtm contains all information about deaths) (-n = 9274)
# With previous heart disease (nr: -n = 79402; actual removed by criteria: -n = 77975)
studypop <- studypop %>%
    mutate(surv14d = difftime(befdoddtm, admission_date, units = c("days"))) %>%
    filter(surv14d > 14 | is.na(surv14d)) %>%
    filter(prev_HF == 1)

## Recode other NA-values (9) as NA
is_labelled <- function(x) inherits(x, "labelled")
studypop[] <- lapply(studypop, function(i){
    replace(i, is_labelled(i) & i == 9, NA)
})

## Define variables
# some variables are coded missing when actually 0/No, these variables have been checked against Patient Registry in the Swedeheart data
# and there verified to be 0/No, for these variables missing apparently means 0/No. The kom_ variables are from patient registry
studypop_var <- within(studypop, {
    
    diabetes_def <- NA_integer_
    diabetes_def[d_diabetes == 0 & kom_diabetes == 0 & diab_med_insulin == 0 & diab_med_oral == 0 & diab_med_insulin == 0 & 
                     diab_med_oral_dc == 0] <- 0L
    diabetes_def[d_diabetes == 1 | kom_diabetes == 1 | diab_med_insulin == 1 | diab_med_oral == 1 | diab_med_insulin == 1 | 
                     diab_med_oral_dc == 1] <- 1L
    diabetes_all <- ifelse(is.na(diabetes_def) | diabetes_def == 0, 0L, 1L)
    
    hypertoni_all <- NA_integer_
    hypertoni_all[hypertension == 0 | kom_hypertoni == 0] <- 0L
    hypertoni_all[hypertension == 1 | kom_hypertoni == 1] <- 1L
    
    current_smoking <- NA_integer_
    current_smoking[smoking_status %in% c(0, 1)] <- 0L
    current_smoking[smoking_status == 2] <- 1L
    
    prior_mi <- NA_integer_
    prior_mi[is.na(previous_mi) | is.na(kom_hinfarkt)] <- 0L    # example missing apparently means 0/No
    prior_mi[previous_mi == 0 & kom_hinfarkt == 0] <- 0L
    prior_mi[previous_mi == 1 | kom_hinfarkt == 1] <- 1L
       
    prior_pci <- NA_integer_
    prior_pci[is.na(previous_pci) | is.na(kom_pci)] <- 0L
    prior_pci[previous_pci == 0 & kom_pci == 0] <- 0L
    prior_pci[previous_pci == 1 | kom_pci == 1] <- 1L
    
    prior_cabg <- NA_integer_
    prior_cabg[is.na(prior_cardiac_surgery) | is.na(kom_cabg)] <- 0L
    prior_cabg[prior_cardiac_surgery == 0 & kom_cabg == 0] <- 0L
    prior_cabg[prior_cardiac_surgery == 1 | kom_cabg == 1] <- 1L

    prior_hf <- NA_integer_
    prior_hf[is.na(history_of_chf) | is.na(kom_hsvikt)] <- 0L
    prior_hf[history_of_chf == 0 & kom_hsvikt == 0] <- 0L
    prior_hf[history_of_chf == 1 | kom_hsvikt == 1] <- 1L
    
    # prior_ischemic_stroke <- NA_integer_          OBS!!! variables missing in data
    # prior_ischemic_stroke[is.na(kom_isch_stroke) | is.na(kom_ospec_stroke) | is.na(kom_tia)] <- 0L
    # prior_ischemic_stroke[kom_isch_stroke == 0 & kom_ospec_stroke == 0 & kom_tia == 0] <- 0L
    # prior_ischemic_stroke[kom_isch_stroke == 1 | kom_ospec_stroke == 1 | kom_tia == 1] <- 1L

    afib_adm <- NA_integer_
    afib_adm[ecg_rhythm == 1] <- 0L
    afib_adm[ecg_rhythm == 2] <- 1L
    
    afib_dis <- NA_integer_
    afib_dis[discharge_ecg_rhythm == 1] <- 0L
    afib_dis[discharge_ecg_rhythm == 2] <- 1L
    
    killip_above_1 <- NA_integer_
    killip_above_1[pulmonary_rales == 0 & cardiac_shock == 0] <- 1L
    killip_above_1[pulmonary_rales %in% c(1, 2, 3) | cardiac_shock == 1] <- 1L

    dualapt_adm <- NA_integer_      # replace mono-dual-APT with one variable with all info
    dualapt_adm[aspirin_reg == 0 & other_antiplatelet_reg == 0] <- 0L    
    dualapt_adm[aspirin_reg == 1 | other_antiplatelet_reg %in% c(1, 2, 3, 4, 8)] <- 1L
    dualapt_adm[aspirin_reg == 1 & other_antiplatelet_reg %in% c(1, 2, 3, 4, 8)] <- 2L
    
    dualapt_dis <- NA_integer_      # replace mono-dual-APT with one variable with all info
    dualapt_dis[aspirin_discharge == 0 & other_antiplatelet_discharge == 0] <- 0L    
    dualapt_dis[aspirin_discharge == 1 | other_antiplatelet_discharge %in% c(1, 2, 3, 4, 8)] <- 1L
    dualapt_dis[aspirin_discharge == 1 & other_antiplatelet_discharge %in% c(1, 2, 3, 4, 8)] <- 2L
        
    warfarin_adm <- NA_integer_
    warfarin_adm <- ifelse(oral_anticoagulants_reg == 1, 1L, 0L)
    
    warfarin_dis <- NA_integer_
    warfarin_dis <- ifelse(oral_anticoagulants_discharge == 1, 1L, 0L)
    
    ras_adm <- NA_integer_
    ras_adm[ace_inhibitors_reg == 0 & angiotensin_ii_block_reg == 0] <- 0L
    ras_adm[ace_inhibitors_reg == 1 | angiotensin_ii_block_reg == 1] <- 1L
    
    ras_dis <- NA_integer_
    ras_dis[ace_inhibitors_discharge == 0 & angiotensin_ii_block_discharge == 0] <- 0L
    ras_dis[ace_inhibitors_discharge == 1 | angiotensin_ii_block_discharge == 1] <- 1L

    spironolactone_adm <- NA_integer_
    spironolactone_adm <- ifelse(c03da01_6mb == 1, 1L, 0L)
    
    spironolactone_dis <- NA_integer_
    spironolactone_dis <- ifelse(c03da01_2w == 1, 1L, 0L)

    eplerenon_adm <- NA_integer_
    eplerenon_adm <- ifelse(c03da04_6mb == 1, 1L, 0L)
    
    eplerenon_dis <- NA_integer_
    eplerenon_dis <- ifelse(c03da04_2w == 1, 1L, 0L)
    
    aldosterone_adm <- NA_integer_
    aldosterone_adm[c03da01_6mb == 0 & c03da04_6mb == 0] <- 0L
    aldosterone_adm[c03da01_6mb == 1 | c03da04_6mb == 1] <- 1L
    
    aldosterone_dis <- NA_integer_
    aldosterone_dis[c03da01_2w == 0 & c03da04_2w == 0] <- 0L
    aldosterone_dis[c03da01_2w == 1 | c03da04_2w == 1] <- 1L

})


## Add variables and format factor variables (attributes and information from format = labelled)
# uses help function at the top as_factor
studypop_var[] <- lapply(studypop_var, function(i) if (is_labelled(i)) as_factor(i) else i)

test <- select(studypop_var, c(idnr, d_year_num, d_age_hia, d_gender, diabetes_all, hypertoni_all, current_smoking, prior_mi, prior_pci, 
                               prior_cabg, afib_adm, afib_dis, killip_above_1, dualapt_adm, killip_above_1, dualapt_adm, dualapt_dis,
                               warfarin_adm, warfarin_dis, ras_adm, ras_dis, spironolactone_adm, spironolactone_dis,
                               eplerenon_adm, eplerenon_dis, aldosterone_adm, aldosterone_dis,kom_perifer, kom_kol, kom_cancer,
                               d_stemi, beta_blockers_reg, beta_blockers_discharge, calcium_antagonist_reg, 
                               calcium_antagonist_discharge, digitalis_reg, digitalis_discharge, 
                               diuretics_reg, diuretics_discharge, statins_reg, statins_discharge, d_pci, d_cabg, 
                               d_left_ventricular_function, d_inotropes, d_diuretics_treat))
glimpse(test)

# factor variables and labels of binary integer/numeric variables 
binlab <- c("No", "Yes")
test[] <- lapply(test, function(i){
    if((is.integer(i) | is.numeric(i)) & length(unique(i)) %in% c(2, 3)) i <- factor(i, levels = c(0, 1), labels = binlab) else i <- i
}) 

glimpse(test)

# final fix of label attributes
test[] <- lapply(test, function(x)
  if(is.factor(x)) plyr::revalue(x, c("Nej" = "No", "Ja" = "Yes", "Man" = "Male", "Kvinna" = "Female"))
  else x
)

glimpse(test)

test <- within(test,{
    dualapt_adm <- factor(dualapt_adm, labels = c("No", "Yes/Or", "Yes/Both"))
    dualapt_dis <- factor(dualapt_dis, labels = c("No", "Yes/Or", "Yes/Both"))
    d_left_ventricular_function <- factor(d_left_ventricular_function, 
                                          labels = c("Normal (>=50%)", 
                                                     "Slightly reduced (40-49%)", 
                                                     "Moderately reduced (30-39%)", 
                                                     "Heavily reduced (<30%)"))
})

glimpse(test)

## OBS!!! Define Crea in separate file and add in here !!!



# drop variables
studypop_var <- select(studypop_var, -c(diabetes_def))


table(studypop_var$aspirin_reg, useNA = "i"); table(studypop_var$other_antiplatelet_reg, useNA = "i")
table(studypop_var$aspirin_reg, studypop_var$other_antiplatelet_reg, useNA = "i")
table(studypop_var$mono_apt_adm, useNA = "i")

table(studypop_var$current_smoking, useNA = "i")
table(studypop_var$diabetes_def, useNA = "i"); table(studypop_var$diabetes_all, useNA = "i")
table(studypop_var$mi_def, useNA = "i"); table(studypop_var$prior_mi, useNA = "i")

################################## end of program #################################




# check dates
# dates <- subset(studypop, select = c("admission_date", "befdoddtm", "d_event_date", "fo1_DEATH_DATE", "fo1_FOLLOWUP_DATE", 
#                                      "fo2_DEATH_DATE", "fo2_FOLLOWUP_DATE"))
# head(dates)
# 
# identical(studypop$befdoddtm, studypop$fo1_DEATH_DATE); identical(studypop$befdoddtm, studypop$fo2_DEATH_DATE)
# 
# death_dates <- subset(dates, !is.na(fo1_DEATH_DATE) | !is.na(fo2_DEATH_DATE))
# head(death_dates)
# 
# nrow(subset(dates, is.na(befdoddtm) & !is.na(fo1_DEATH_DATE))); nrow(subset(dates, is.na(befdoddtm) & !is.na(fo2_DEATH_DATE)))
# death_check <- subset(dates, is.na(befdoddtm) & !is.na(fo2_DEATH_DATE))
# head(death_check)
# 
# a <- data.frame(studypop$admission_date, studypop$befdoddtm, difftime(studypop$befdoddtm, studypop$admission_date, units = c("days")))
# View(a)

# OBS!!! dessa borde ers?ttas med dualapt_adm, b?rande all information
# mono_apt_adm <- NA_integer_       
# mono_apt_adm[aspirin_reg == 0 & other_antiplatelet_reg == 0] <- 0L
# mono_apt_adm[aspirin_reg == 1 & other_antiplatelet_reg != 0] <- 0L
# mono_apt_adm[(aspirin_reg == 1 & other_antiplatelet_reg == 0) | (aspirin_reg == 0 & other_antiplatelet_reg != 0)] <- 1L
# 
# dual_apt_adm <- NA_integer_
# dual_apt_adm[aspirin_reg == 0 | other_antiplatelet_reg == 0] <- 0L
# dual_apt_adm[aspirin_reg == 1 & other_antiplatelet_reg %in% c(1, 2, 3, 4, 8)] <- 1L



## Obs g?r dessa separat - de b?r redan p? all info as_labelled
# f?rst if as_labelled -> as_factor (egen hj?lp funktion)
# d?refter g?r om kvarvarande numeriska variabler till faktorer f?r hand
# kom_perifer <- factor(kom_perifer, labels = c("No", "Yes"))
# copd <- factor(kom_kol, labels = c("No", "Yes"))
# cancer <- factor(kom_cancer, labels = c("No", "Yes"))
# d_stemi <- factor(d_stemi, labels = c("No", "Yes"))
# beta_blockers_reg <- factor(beta_blockers_reg, labels = c("No", "Yes"))
# calcium_antagonist_reg <- factor(calcium_antagonist_reg, labels = c("No", "Yes"))
# digitalis_reg <- factor(digitalis_reg, labels = c("No", "Yes"))
# diuretics_reg <- factor(diuretics_reg, labels = c("No", "Yes"))
# statins_reg <- factor(statins_reg, labels = c("No", "Yes"))
# d_pci <- factor(d_pci, labels=c("No", "Yes"))
# d_cabg <- factor(d_cabg, labels=c("No", "Yes"))


