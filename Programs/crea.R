# Header
# Filename: crea
# Created: 20161212
# Updated: 
# Author: Henrik Olsson
# Purpose: Define Creatinine variable to be used in analysis (CKD-EPI) & kidney failure
# Data used: swedeheart.raw (rhia_epc_final.sav)
# Output: crea

rm(list=ls())

library("haven")
library("dplyr")


setwd("C:/Users/henols/Desktop/Swedeheart/Aldosterone")

## Swedeheart raw data
swedeheart_raw <- readRDS(file.path("Data", "Original data", "swedeheart_raw.rds"))

## variables needed for definition
vars <- names(swedeheart_raw) %in% c("idnr", "id_rhia", "d_s_creatinin", "d_centreid_ic", "admission_date", "d_gender", "d_age_hia")
swh <- swedeheart_raw[vars]
str(swh)

## logical vector of creatinine levels that should be corrected
corr <- with(swh, {
    
    ( d_centreid_ic %in% c("?rebro HIA", "?rebro PCI") & as.Date(admission_date) < as.Date("1996-01-01") ) |
    ( d_centreid_ic %in% c("?rnsk?ldsvik HIA") & as.Date(admission_date) < as.Date("2004-02-16") ) |
    ( d_centreid_ic %in% c("?stersund HIA", "?stersund PCI") & as.Date(admission_date) < as.Date("2004-06-07") ) |
    ( d_centreid_ic %in% c("?land HIA") & as.Date(admission_date) < as.Date("2008-05-01") ) |
    ( d_centreid_ic %in% c("?ngelholm HIA") & as.Date(admission_date) < as.Date("2000-01-01") ) |
    ( d_centreid_ic %in% c("Alings?s HIA") & as.Date(admission_date) < as.Date("2005-11-01") ) |
    ( d_centreid_ic %in% c("Arvika HIA") & as.Date(admission_date) < as.Date("2002-04-09") ) |  
    ( d_centreid_ic %in% c("Avesta HIA") & as.Date(admission_date) < as.Date("2006-02-13") ) |
    ( d_centreid_ic %in% c("Bolln?s HIA") & as.Date(admission_date) < as.Date("2002-01-01") ) |
    ( d_centreid_ic %in% c("Bor?s HIA", "Bor?s PCI") & as.Date(admission_date) < as.Date("2004-10-25") ) |
    ( d_centreid_ic %in% c("Eksj? HIA") & as.Date(admission_date) < as.Date("2004-02-16") ) |
    ( d_centreid_ic %in% c("Enk?ping HIA") & as.Date(admission_date) < as.Date("2005-10-03") ) |
    ( d_centreid_ic %in% c("Eskilstuna HIA") & as.Date(admission_date) < as.Date("2006-05-29") ) |
    ( d_centreid_ic %in% c("Fagersta HIA") & as.Date(admission_date) < as.Date("2005-03-21") ) |
    ( d_centreid_ic %in% c("Falun HIA", "Falun PCI") & as.Date(admission_date) < as.Date("2006-01-16") ) |
    ( d_centreid_ic %in% c("Finsp?ngs lasarett HIA") & as.Date(admission_date) < as.Date("2007-09-12") ) |
    ( d_centreid_ic %in% c("G?teborg SU ?stra HIA", "G?teborg SU ?stra MAVA", "G?teborg SU Sahlgr HIA") & as.Date(admission_date) < as.Date("2004-06-01") ) |
    ( d_centreid_ic %in% c("G?teborg SU Sahlgr HKIR", "G?teborg SU Sahlgr Hkir Transpl", "G?teborg SU Sahlgr MAVA") & as.Date(admission_date) < as.Date("2004-06-01") ) |
    ( d_centreid_ic %in% c("G?teborg SU Sahlgr RTG", "G?teborg SU ?stra") & as.Date(admission_date) < as.Date("2004-06-01") ) |
    ( d_centreid_ic %in% c("G?teborg SU M?lndal HIA", "G?teborg SU M?lndal") & as.Date(admission_date) < as.Date("2004-06-09") ) |
    ( d_centreid_ic %in% c("G?llivare HIA") & as.Date(admission_date) < as.Date("2004-06-07") ) | 
    ( d_centreid_ic %in% c("G?vle HIA", "G?vle PCI", "G?vle Uppf?ljning") & as.Date(admission_date) < as.Date("2003-09-01") ) |
    ( d_centreid_ic %in% c("H?rn?sand HIA") & as.Date(admission_date) < as.Date("2004-01-21") ) |    
    ( d_centreid_ic %in% c("H?ssleholm HIA") & as.Date(admission_date) < as.Date("2002-01-28") ) |  
    ( d_centreid_ic %in% c("Halmstad avd 41", "Halmstad HIA") & as.Date(admission_date) < as.Date("2001-04-05") ) |
    ( d_centreid_ic %in% c("Helsingborg HIA") & as.Date(admission_date) < as.Date("1998-07-02") ) | 
    ( d_centreid_ic %in% c("Hudiksvall HIA") & as.Date(admission_date) < as.Date("2003-01-01") ) |          
    ( d_centreid_ic %in% c("J?nk?ping HIA", "J?nk?ping PCI") & as.Date(admission_date) < as.Date("2004-02-16") ) |
    ( d_centreid_ic %in% c("K?ping HIA") & as.Date(admission_date) < as.Date("2005-03-31") ) | 
    ( d_centreid_ic %in% c("Kalix HIA") & as.Date(admission_date) < as.Date("2004-06-07") ) | 
    ( d_centreid_ic %in% c("Kalmar HIA") & as.Date(admission_date) < as.Date("2003-06-12") ) | 
    ( d_centreid_ic %in% c("Karlshamn HIA") & as.Date(admission_date) < as.Date("2005-09-25") ) |
    ( d_centreid_ic %in% c("Karlskoga HIA") & as.Date(admission_date) < as.Date("2003-03-10") ) |
    ( d_centreid_ic %in% c("Karlskrona AVD 55", "Karlskrona HIA") & as.Date(admission_date) < as.Date("2006-12-11") ) |
    ( d_centreid_ic %in% c("Karlstad HIA", "Karlstad PCI") & as.Date(admission_date) < as.Date("2002-04-09") ) |
    ( d_centreid_ic %in% c("Katrineholm HIA") & as.Date(admission_date) < as.Date("2006-04-10") ) |
    ( d_centreid_ic %in% c("Kiruna HIA") & as.Date(admission_date) < as.Date("2004-06-07") ) |
    ( d_centreid_ic %in% c("Kristianstad HIA", "Kristianstad PCI") & as.Date(admission_date) < as.Date("2001-03-01") ) |
    ( d_centreid_ic %in% c("Kristinehamns sjukhus HIA") & as.Date(admission_date) < as.Date("2002-04-09") ) |
    ( d_centreid_ic %in% c("Kung?lv HIA") & as.Date(admission_date) < as.Date("2004-09-29") ) |
    ( d_centreid_ic %in% c("Lidk?ping HIA") & as.Date(admission_date) < as.Date("2006-02-13") ) |
    ( d_centreid_ic %in% c("Lindesberg HIA") & as.Date(admission_date) < as.Date("2003-01-01") ) |
    ( d_centreid_ic %in% c("Link?ping HIA", "Link?ping MAVA") & as.Date(admission_date) < as.Date("2007-09-12") ) |
    ( d_centreid_ic %in% c("Ljungby HIA") & as.Date(admission_date) < as.Date("2005-05-16") ) |
    ( d_centreid_ic %in% c("Ludvika HIA") & as.Date(admission_date) < as.Date("2006-02-13") ) |
    ( d_centreid_ic %in% c("Lund HIA", "Lund Hkir") & as.Date(admission_date) < as.Date("1990-01-01") ) |
    ( d_centreid_ic %in% c("Lycksele HIA") & as.Date(admission_date) < as.Date("1996-01-01") ) |
    ( d_centreid_ic %in% c("Malm? HIA", "Malm? Uppf?ljning") & as.Date(admission_date) < as.Date("2004-06-07") ) |
    ( d_centreid_ic %in% c("Mora HIA") & as.Date(admission_date) < as.Date("2006-01-16") ) |
    ( d_centreid_ic %in% c("Motala HIA") & as.Date(admission_date) < as.Date("2007-09-12") ) |
    ( d_centreid_ic %in% c("Nacka HIA") & as.Date(admission_date) < as.Date("2004-02-16") ) |
    ( d_centreid_ic %in% c("Norrk?ping Vrinnevi HIA") & as.Date(admission_date) < as.Date("2007-09-12") ) |  
    ( d_centreid_ic %in% c("Norrt?lje HIA") & as.Date(admission_date) < as.Date("2005-03-21") ) | 
    ( d_centreid_ic %in% c("Nyk?ping HIA") & as.Date(admission_date) < as.Date("2006-05-29") ) | 
    ( d_centreid_ic %in% c("Oskarshamn HIA") & as.Date(admission_date) < as.Date("2003-06-12") ) |
    ( d_centreid_ic %in% c("Pite? HIA") & as.Date(admission_date) < as.Date("2004-06-07") ) |
    ( d_centreid_ic %in% c("S?dert?lje HIA") & as.Date(admission_date) < as.Date("2009-12-01") ) | 
    ( d_centreid_ic %in% c("S?ffle HIA") & as.Date(admission_date) < as.Date("2002-04-09") ) |   
    ( d_centreid_ic %in% c("Sala HIA") & as.Date(admission_date) < as.Date("2005-03-21") ) | 
    ( d_centreid_ic %in% c("Sandviken HIA") & as.Date(admission_date) < as.Date("2006-01-16") ) | 
    ( d_centreid_ic %in% c("Simrishamn HIA") & as.Date(admission_date) < as.Date("2004-05-12") ) | 
    ( d_centreid_ic %in% c("Sk?vde HIA", "Sk?vde PCI") & as.Date(admission_date) < as.Date("2007-01-15") ) |
    ( d_centreid_ic %in% c("Skellefte? HIA") & as.Date(admission_date) < as.Date("1996-01-01") ) | 
    ( d_centreid_ic %in% c("Skene HIA") & as.Date(admission_date) < as.Date("2005-11-01") ) | 
    ( d_centreid_ic %in% c("Sollefte? HIA") & as.Date(admission_date) < as.Date("2004-06-07") ) | 
    ( d_centreid_ic %in% c("Stockholm KS Solna AVA", "Stockholm KS Solna HIA", "Stockholm KS Solna Hkir") & as.Date(admission_date) < as.Date("2005-03-31") ) |
    ( d_centreid_ic %in% c("Stockholm Danderyd HIA", "Stockholm Danderyd PCI") & as.Date(admission_date) < as.Date("2005-03-31") ) |
    ( d_centreid_ic %in% c("Stockholm KS Huddinge CT", "Stockholm KS Huddinge HIA") & as.Date(admission_date) < as.Date("2004-02-16") ) |
    ( d_centreid_ic %in% c("Stockholm KS Huddinge MAVA", "Stockholm KS Huddinge PCI") & as.Date(admission_date) < as.Date("2004-02-16") ) |
    ( d_centreid_ic %in% c("Stockholm S?S HIA", "Stockholm S?S PCI") & as.Date(admission_date) < as.Date("2010-04-01") ) |
    ( d_centreid_ic %in% c("Stockholm St G?ran BSE", "Stockholm St G?ran HIA", "Stockholm St G?ran PCI") & as.Date(admission_date) < as.Date("2005-10-03") ) |
    ( d_centreid_ic %in% c("St G?ran CT") & as.Date(admission_date) < as.Date("2005-10-03") ) |
    ( d_centreid_ic %in% c("Sunderbyn HIA") & as.Date(admission_date) < as.Date("2004-06-07") ) |
    ( d_centreid_ic %in% c("Sundsvall HIA", "Sundsvall PCI") & as.Date(admission_date) < as.Date("2004-02-16") ) |
    ( d_centreid_ic %in% c("Torsby HIA") & as.Date(admission_date) < as.Date("2002-04-09") ) |
    ( d_centreid_ic %in% c("Trelleborg HIA") & as.Date(admission_date) < as.Date("2012-01-01") ) |
    ( d_centreid_ic %in% c("Trollh?ttan NU-sjukv?rden HIA", "Trollh?ttan NU-sjukv?rden PCI") & as.Date(admission_date) < as.Date("2004-11-01") ) |
    ( d_centreid_ic %in% c("Uddevalla HIA") & as.Date(admission_date) < as.Date("2004-11-01") ) |
    ( d_centreid_ic %in% c("Ume? HIA") & as.Date(admission_date) < as.Date("1996-03-01") ) | 
    ( d_centreid_ic %in% c("Uppsala HIA", "Uppsala PCI", "Uppsala TAVI") & as.Date(admission_date) < as.Date("2005-10-03") ) | 
    ( d_centreid_ic %in% c("V?rnamo HIA") & as.Date(admission_date) < as.Date("2004-02-16") ) | 
    ( d_centreid_ic %in% c("V?ster?s HIA", "V?ster?s PCI", "Uppsala TAVI") & as.Date(admission_date) < as.Date("2005-03-31") ) |
    ( d_centreid_ic %in% c("V?stervik HIA") & as.Date(admission_date) < as.Date("2003-06-12") ) | 
    ( d_centreid_ic %in% c("V?xj? HIA") & as.Date(admission_date) < as.Date("2004-11-01") ) | 
    ( d_centreid_ic %in% c("Varberg HIA", "Varberg") & as.Date(admission_date) < as.Date("2001-04-05") ) |
    ( d_centreid_ic %in% c("Visby HIA") & as.Date(admission_date) < as.Date("2010-05-04") ) |
    ( d_centreid_ic %in% c("Ystad HIA") & as.Date(admission_date) < as.Date("2007-01-15") )
    
})

table(corr)


## create corrected variables (based on Swedeheart SPSS code)
# define Creatinine
# define Estimated Glomerular Filtration Rate (eGFR)
swh_crea <- within(swh, {
    
    crea <- NA
    crea <- ifelse(corr == 1, d_s_creatinin*0.95, d_s_creatinin)
    flag <- ifelse(corr == 1, 1, 0)
    
    crea_egfr <- NA
    crea_egfr <- ifelse(d_gender == 1 & crea < 79.76 & !is.na(d_age_hia) & !is.na(crea)
                        , 141*(((crea/88.4)/0.9)^-0.411)*(0.993^d_age_hia)
                        , crea_egfr)
    
    crea_egfr <- ifelse(d_gender == 2 & crea < 61.88 & !is.na(d_age_hia) & !is.na(crea)
                        , 144*(((crea/88.4)/0.7)^-0.329)*(0.993^d_age_hia)
                        , crea_egfr)
    
    crea_egfr <- ifelse(d_gender == 1 & crea > 79.76 & !is.na(d_age_hia) & !is.na(crea)
                        , 141*(((crea/88.4)/0.9)^-1.209)*(0.993^d_age_hia)
                        , crea_egfr)
    
    crea_egfr <- ifelse(d_gender == 2 & crea > 61.88 & !is.na(d_age_hia) & !is.na(crea)
                        , 144*(((crea/88.4)/0.7)^-1.209)*(0.993^d_age_hia)
                        , crea_egfr)
    
    njursvikt_korr <- NA_integer_
    njursvikt_korr[crea_egfr >= 60] <- 0L
    njursvikt_korr[crea_egfr < 60] <- 1L
    
    njure_korr <- NA_integer_
    njure_korr[crea_egfr >= 60] <- 0L
    njure_korr[crea_egfr < 60 & crea_egfr >= 30] <- 1L
    njure_korr[crea_egfr < 30] <- 2L
    
    njure_korr2 <- NA_integer_
    njure_korr2[crea_egfr >= 60] <- 1L
    njure_korr2[crea_egfr < 60 & crea_egfr >= 50] <- 2L
    njure_korr2[crea_egfr < 50 & crea_egfr >= 40] <- 3L
    njure_korr2[crea_egfr < 40 & crea_egfr >= 30] <- 4L
    njure_korr2[crea_egfr < 30 & crea_egfr >= 20] <- 5L
    njure_korr2[crea_egfr < 20] <- 6L
    
})


crea <- select(swh_crea, idnr, id_rhia, crea, crea_egfr, njursvikt_korr, njure_korr, njure_korr2)
saveRDS(crea, file.path("Data", "Derived data", "crea.rds"), compress = TRUE)


################################## end of program #################################

