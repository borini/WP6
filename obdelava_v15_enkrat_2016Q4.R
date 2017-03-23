# OBDELAVA_V15_ENKRAT_2016Q4.R
# Code to estimate 1 time series 1 time
# Method: balanced method (enterprises presenting missing values are removed)
# Manca Golmajer
# September 2016 - January 2017

# Instructions:
# 1. Set the parameters at the beginning (point 0.).
# 2. Run the whole code!

#######################################################################################
# 0. CHANGE THE PARAMETERS HERE
#######################################################################################

# Clear the workspace: remove all the visible objects
rm(list=ls())

# Install packages (only if they or the appropriate versions are not yet installed)
# install.packages("missMDA")
# install.packages("Formula")
# install.packages("Hmisc")
# install.packages("foreign")
# install.packages("graphics") # for function ts.plot
# install.packages("ggplot2") # for function ggplot
# install.packages("gvlma") # for function glvma
# install.packages("car") # for function durbinWatsonTest
# install.packages("lmtest") # for function dwtest
# install.packages("haven") # for function read_sas (If you want to use sas7bdat files, SAS has to be installed on the computer!)
# install.packages("dplyr") # for functions group_by, summarise_each
# install.packages("reshape") # for function cast
# Package stats (for function shapiro.test) is not available.

# Version of code
verzija <- "v15"

# Time since the end of reference period (idea: only enterprise data that are available
# before the last day of reference period + t_plus are taken into account):
# - a positive number of days (in data are not final) or NA (in data are already final);
# - used only for all enterprise data in second form in v_pod_vir (not used for other predictors).
t_plus <- 46
#t_plus <- NA

# Suffix for the output files' name can be used instead of some parameter values ...
#izhod_pripona <- "" # no suffix, some parameter values are used instead (e.g. output file OBDELAVA_v12_ind_2008M01_2015M12_1-2-3-4-5_zadnja5-po7-po8-po10-po15-po20-70-75-80-85-90_backward_NO_no-stl_NO.csv)
izhod_pripona <- "hitra_ocena_2016Q4_vir1_u_46_klima3_80_stl" # suffix primer1 (e.g. output file OBDELAVA_v13_ind_2008M01_2015M12_primer1.csv)

# Paths
#pot1 <- "C:/Users/v3user/Desktop/Nowcasting/7_ocenjevanje_BDP/" # laptop
#pot1 <- "Q:/Big_Data/ESSNET Big Data/WP6_Combining BD Sources/Nowcasting/7_ocenjevanje_BDP/" # my computer
pot1 <- "/home/manca/BigDataManca/" # server
pot2 <- "1_Priprava_podatkov/mikro"
pot3 <- "1_Priprava_podatkov/Y"
pot4 <- "2_Obdelava"
pot5 <- "1_Priprava_podatkov/drugi"

# Time frame:
# - beginning of enterprise data and series of interest:
#zacetek <- "2008M01"
#zacetek <- "2008M02"
zacetek <- "2008Q1"
#zacetek <- "2008Q2"
# - endings of enterprise data, possible endings of series of interest:
#v_konec <- c("2015M12")
#v_konec <- c("2015M11")
#v_konec <- c("2013M01")
#v_konec <- c("2013M08")
#v_konec <- c("2015Q4")
#v_konec <- c("2015Q3")
v_konec <- c("2016Q4")
# - possible endings of series of interest (periods previous to konec)
#v_konec_prej <- c("2015M11")
#v_konec_prej <- c("2015M10")
#v_konec_prej <- c("2012M12")
#v_konec_prej <- c("2013M07")
#v_konec_prej <- c("2015Q3")
#v_konec_prej <- c("2015Q2")
v_konec_prej <- c("2016Q3")

# Data sources
# - enterprise data: possible format: csv or sas7bdat; data must all be either monthly or quarterly; two possible forms:
#   - first form: obdobje, P..., ...
#   - second form: obdobje, spr, pris_datum, id, tip_pod
#v_pod_vir <- c("podatki_vir1.csv")
#v_pod_vir <- c("podatki_vir1.sas7bdat")
#v_pod_vir <- c("podatki_vir2.sas7bdat")
#v_pod_vir <- c("podatki_test1su.sas7bdat")
#v_pod_vir <- c("podatki_vir1su_04JAN2017.csv")
#v_pod_vir <- c("podatki_vir1su_04jan2017.sas7bdat")
v_pod_vir <- c("podatki_vir1su_2017_15_02.sas7bdat")
v_pod_vir_ref1 <- 12 # the beginning of reference substring
v_pod_vir_ref2 <- 12 # the end of reference substring
# - series of interest data:
#vr_vir <- "ind_2008M01_2015M12.csv"
#vr_vir <- "ind_2008M01_2015M10_test.csv"
#vr_vir <- "bdp_sc_2008Q1_2015Q4.csv"
vr_vir <- "bdp_sc_2008Q1_2016Q4.csv"
# - data of other predictors: possible format: csv or sas7bdat; data must all be either monthly or quarterly;
#   first variable must be obdobje
#v_dr_vir <- character() # empty vector means there are no other predictors
#v_dr_vir <- c("klima3_2008M01_2015M12.csv")
#v_dr_vir <- c("klima_2008M01_2015M12.csv","ind_2008M01_2015M12.csv")
v_dr_vir <- c("klima3_2008M01_2016M12.csv")

# Which type of data do you want to use - decide for each element of v_pod_vir:
# - only raw data: s;
# - only edited data: u;
# - both raw data and edited data: su.
#v_tip <- c("s")
v_tip <- c("u")
#v_tip <- c("su")
#v_tip <- c("s","su")

# Parameters for optimization functions:

# - izberi_prve_pogoj can be: "Kaiser", "zadnja5", "po20", "po10", "po15" or a string containing a number (e.g. "80")
#v_izberi_prve_pogoj <- c("Kaiser", "zadnja5", "po7", "po8", "po10", "po15", "po20, "70", "75", "80", "85", "90")
#v_izberi_prve_pogoj <- c("zadnja5", "po7", "po8", "po10", "po15", "po20, "70", "75", "80", "85", "90")
v_izberi_prve_pogoj <- c("80")
#v_izberi_prve_pogoj <- c("zadnja5")
#v_izberi_prve_pogoj <- c("po15")
                         
# - optim_regresija_smer can be: "backward", "forward", "both"
#v_optim_regresija_smer <- c("backward","forward","both")
v_optim_regresija_smer <- c("backward")
#v_optim_regresija_smer <- c("forward")
#v_optim_regresija_smer <- c("both")

# - optim_regresija_pogoj can be: "AIC", "BIC", "NO"
#v_optim_regresija_pogoj <- c("AIC")
#v_optim_regresija_pogoj <- c("BIC")
v_optim_regresija_pogoj <- c("NO")

# - sezona can be: "no", "stl"
#v_sezona <- c("no", "stl")
#v_sezona <- c("no")
v_sezona <- c("stl")

# Tests:

# - Do you want to include the test for assumptions of linear regression? Possible value: YES, NO.
#   (If you include this test, there might be an error; if so, not all possible combinations
#   of parameters for optimization functions will be included in the first ouput file ...)
#vkljuci_test_predp <- "YES"
vkljuci_test_predp <- "NO"

#######################################################################################
# 1. PACKAGES AND PATHS
#######################################################################################

# Include packages
library(missMDA)
library(Formula)
library(Hmisc)
library(foreign)
library(graphics)
library(ggplot2)
library(gvlma)
library(car)
library(lmtest)
library(stats) # Package stats is available by default, so I can't install it.
library(haven)
library(dplyr)
library(reshape)

# Working directory
setwd(paste(pot1, pot4, sep=""))
getwd()

#######################################################################################
# 2. DEFINITION OF FUNCTIONS
#######################################################################################

# Definition of function: preveri_obdobje
# This function checks if the values of obdobje are consistent
# (i.e. they are all either monthly or quarterly, they have the same length, they have the correct form (e.g. 2008M01, 2008Q1))
# and if there are no inappropriate gaps (e.g. sequence 2008M01, 2008M03 is wrong, because 2008M02 is missing).
preveri_obdobje <- function(obdobje){
  obdobje <- as.character(obdobje) # obdobje must be a character vector because of function nchar
  # If we sort obdobje, we should get the same vector.
  if(length(which(sort(obdobje) != obdobje)) > 0){
    stop("Variable obdobje doesn't have appropriate values. The variable isn't sorted correctly.")
  }
  # Monthly or quarterly
  P <- substr(obdobje[1],5,5)
  if(P == "M"){
    if(length(which(substr(obdobje,5,5) != "M")) > 0){
      stop("Variable obdobje doesn't have appropriate values. The fifth character should be M.")
    }
    if(length(which(nchar(obdobje) != 7)) > 0){
      stop("Variable obdobje doesn't have appropriate values. All values should have 7 characters.")
    }
    if (!length(which(substr(obdobje,6,7) %in% c("01","02","03","04","05","06","07","08","09","10","11","12"))) > 0){
      stop("Variable obdobje doesn't have appropriate values. The sixth and seventh character should be between 01 and 12.")
    }
    obdobje1 <- obdobje[-length(obdobje)] # obdobje without the last value
    leto1 <- as.numeric(substr(obdobje1,1,4))
    mesec1 <- as.numeric(substr(obdobje1,6,7))
    obdobje2 <- obdobje[-1] # obdobje without the first value
    leto2 <- as.numeric(substr(obdobje2,1,4))
    mesec2 <- as.numeric(substr(obdobje2,6,7))
    if(length(which(!(((leto1 == leto2) & (mesec1+1 == mesec2)) | ((leto1+1 == leto2) & (mesec1 == 12) & (mesec2 == 1))))) > 0){
      stop("Variable obdobje doesn't have appropriate values. There are inappropriate gaps between values.")
    }    
  }else if(P == "Q"){
    if(length(which(substr(obdobje,5,5) != "Q")) > 0){
      stop("Variable obdobje doesn't have appropriate values. The fifth character should be Q.")
    }
    if(length(which(nchar(obdobje) != 6)) > 0){
      stop("Variable obdobje doesn't have appropriate values. All values should have 6 characters.")
    }
    if (!length(which(substr(obdobje,6,6) %in% c("1","2","3","4"))) > 0){
      stop("Variable obdobje doesn't have appropriate values. The sixth character should be between 1 and 4.")
    }
    obdobje1 <- obdobje[-length(obdobje)] # obdobje without the last value
    leto1 <- as.numeric(substr(obdobje1,1,4))
    cetrtletje1 <- as.numeric(substr(obdobje1,6,6))
    obdobje2 <- obdobje[-1] # obdobje without the first value
    leto2 <- as.numeric(substr(obdobje2,1,4))
    cetrtletje2 <- as.numeric(substr(obdobje2,6,6))
    if(length(which(!(((leto1 == leto2) & (cetrtletje1+1 == cetrtletje2)) | ((leto1+1 == leto2) & (cetrtletje1 == 4) & (cetrtletje2 == 1))))) > 0){
      stop("Variable obdobje doesn't have appropriate values. There are inappropriate gaps between values.")
    }        
  }else{
    stop("Variable obdobje doesn't have appropriate values. The fifth character should be M or Q.")
  }
  return(TRUE)
}

# Definition of function: odstrani_obdobje_zacetek
# This function removes all the rows before the beginning (zacetek).
odstrani_obdobje_zacetek <- function(pod){
  zacetek_vr <- which(pod[,1] == zacetek) # actual beginning of enterprise data
  if(length(zacetek_vr) == 0){
    stop("The beginning (zacetek) isn't present in the data.")
  }else{
    if(zacetek_vr==1){
      pod2 <- pod
    }else{
      pod2 <- pod[-(1:zacetek_vr-1),]
    }
  }
  return(pod2)
}

# Definition of function: odstrani_obdobje_konec
# This function removes all the rows after the ending (konec).
odstrani_obdobje_konec <- function(pod,konec){
  konec_vr <- which(pod[,1] == konec) # ending of the data
  if(length(konec_vr) == 0){
    stop("The ending (konec) isn't present in the data.")
  }else{
    if(konec_vr==dim(pod)[1]){
      pod2 <- pod
    }else{
      pod2 <- pod[-(konec_vr+1:dim(pod)[1]),]
    }
  }
  return(pod2)
}

# Definition of function: odstrani_nepopolne
# This function makes a full matrix: removes columns with any NAs.
odstrani_nepopolne <- function(pod){
  prazni <- numeric()
  for(i in 1:ncol(pod)){
    # number of NAs in the i-th column of pod is added to prazni
    prazni <- append(prazni, length(which(is.na(pod[,i]))))
  }
  # columns with more than 0 NAs are removed
  odstrani <- which(prazni > 0)
  if(length(odstrani) > 0){
    pod2 <- pod[,-odstrani]
  }else{
    pod2 <- pod
  }
  
  return(pod2)
}

# Definition of function: periodika
# This function returns 12 in case of monthly data and 4 in case of quarterly data.
periodika <- function(pod){
  p <- substring(pod[1,1],5,5)
  switch(p,
    M={frek <- 12},
    Q={frek <- 4},
    stop("The data is not monthly nor quarterly.")
  )
  return(frek)
}

# Definition of function: izberi_prve
# This function takes a vector of eigenvalues of covariance matrix (vred), condition (pogoj) and
# the interesting time series (vrsta)
# and returns the number of how many first eigenvalues (principal components) we have to choose.
# Possible values for pogoj:
# - "xx" (e.g. "80"): take enough p. c. to explaint xx% (or a bit more) of the variability of the enterprise data
# - "Kaiser": take every p. c., whose eigenvalue is greater or equal to 1
# - "zadnja5": take every p. c., whose eigenvalue's share among all eigenvalues is greater or equal to 5%
# - "po7": take only as many p. c. to have at least 7 cases (time points) per independent variable later in the linear regression
# - "po8": ... at least 8 cases ...
# - "po10": ... at least 10 cases ...
# - "po15": ... at least 15 cases ...
# - "po20": ... at least 20 cases ...
izberi_prve <- function(vred,pogoj,vrsta){
  delez <- vred/sum(vred) # shares
  c_delez <- cumsum(vred)/sum(vred) # cumulative shares
  switch(pogoj,
         Kaiser={n <- max(1, length(vred[vred >= 1]))},
         zadnja5={n <- max(1, length(delez[delez >= 0.05]))},
         po7={n <- max(1, floor(dim(vrsta)[1]/7))},
         po8={n <- max(1, floor(dim(vrsta)[1]/8))},
         po10={n <- max(1, floor(dim(vrsta)[1]/10))},
         po15={n <- max(1, floor(dim(vrsta)[1]/15))},
         po20={n <- max(1, floor(dim(vrsta)[1]/20))},
         # case else (in our examples "xx", e.g. "80")
         {meja <- as.numeric(pogoj)/100
         n <- length(c_delez[c_delez < meja]) + 1}
  )
  return(n)
}

# Definition of function: optim_regresija
# This function takes a linear regression (regresija), a direction (smer), a condition (pogoj) and length of enterprise data (l_podatki).
# It returns optimal linear regression (i. e. optimal factors are selected)
# Possible values for pogoj:
# - "AIC": genuine AIC is used.
# - "BIC": BIC is used.
# - "NO": regresija is returned.
# - else: an error is retuned.
# Possible values for smer: "backward", "forward", "both".
# Function step is in package stats.
optim_regresija <- function(regresija,smer,pogoj,l_podatki){
  switch(pogoj,
         AIC={regresija2 <- step(regresija, direction=smer, k=2)},
         BIC={regresija2 <- step(regresija, direction=smer, k=log(l_podatki-1))},
         NO={regresija2 <- regresija},
         # case else
         stop("The value of pogoj isn't valid. Choose a different option for selection of optimal factors.")
  )
  return(regresija2)
}

# Definition of function: ime_tip
# This function returns the name and the type separately, e.g. for "my.car.csv" we get "my.car" and "csv".
ime_tip <- function(vir){
  a <- strsplit(vir, split="[.]")[[1]]
  ime <- paste(a[-length(a)], collapse=".")
  tip <- a[length(a)]
  return(c(ime,tip))
}

# Definition of function: referenca
# This function returns the string of parts of vector elements:
# - part starts at ref1;
# - part ends at ref2;
# - separator is locilo.
referenca <- function(vektor,ref1,ref2,locilo){
  podnizi <- substr(vektor,ref1,ref2)
  ref <- paste(podnizi, collapse=locilo)
  return(ref)
}

# Definition of function: zadnji_dan
# This function takes date, e.g. 2013-02-15, 2008M03, 2008Q2, and returns the last date of this period, e.g. 2013-02-28, 2008-03-31, 2008-06-30.
zadnji_dan <- function(datum){
  if(substr(datum,5,5)=="-"){ # e.g. "2013-02-15"
    datum2 <- base::as.Date(datum)
  }else if(substr(datum,5,5)=="M"){ # e.g. "2008M03"
    leto <- substr(datum,1,4)
    mesec <- substr(datum,6,7)
    datum1 <- paste(leto, mesec, "28", sep="-")
    datum2 <- base::as.Date(datum1)
  }else if(substr(datum,5,5)=="Q"){ # e.g. "2008Q1"
    leto <- substr(datum,1,4)
    cetrtletje <- substr(datum,6,6)
    if(cetrtletje=="1"){
      mesec <- "03"
    }else if(cetrtletje=="2"){
      mesec <- "06"
    }else if(cetrtletje=="3"){
      mesec <- "09"
    }else if(cetrtletje=="4"){
      mesec <- "12"
    }else {
      stop("The form of date (period) isn't appropriate.")
    }
    datum1 <- paste(leto, mesec, "28", sep="-")
    datum2 <- base::as.Date(datum1)
  }else{ # error
    stop("The form of date (period) isn't appropriate.")
  }
  for(i in 1:31){
    if(format(datum2 + i, '%d')=="01"){
      datum3 <- datum2 + i - 1
      break
    }
  }
  return(datum3)
}

# Definition of function: mes_v_cet
# This function should be used only for monthly data. It returns quarterly data.
# Possible values for fja are: sum, mean. (Other values could simply be added.)
# If e.g. a variable has value for 2008M01 and 2008M02, but no value for 2008M03 (i.e. NA), then 2008Q1 has no value (i.e. NA).
# (This change might take a long time to process.)
mes_v_cet <- function(podatki,fja){
  # This function should be used only for monthly data.
  if(!(substr(podatki$obdobje[1],5,5) == "M")){
    stop("Function mes_v_cet should be used only for monthly data.")
  }
  # Different process in case of different form of data
  if("pris_datum" %in% colnames(podatki)){ # in case of second form
    # Check if all the variables are present
    if(!("obdobje" %in% colnames(podatki) & "spr" %in% colnames(podatki) & "pris_datum" %in% colnames(podatki) & "id" %in% colnames(podatki) & "tip_pod" %in% colnames(podatki))){
      stop("The data don't include all the necessary variables obdobje, spr, pris_datum, id, tip_pod.")
    }
    podatki1 <- mutate(podatki,
                       leto = substr(obdobje,1,4),
                       mesec = as.numeric(substr(obdobje,6,7)),
                       cetrtletje = floor((mesec-1)/3)+1,
                       obdobje1 = paste(leto, "Q", as.character(cetrtletje), sep=""),
                       st_pod = 1
    )
    podatki2 <- select(podatki1, -obdobje, obdobje=obdobje1, -leto, -mesec, -cetrtletje)
    podatki3 <- podatki2[,c("obdobje","id","tip_pod","spr","pris_datum","st_pod")]
    podatki4 <- arrange(podatki3, obdobje, id, tip_pod, desc(pris_datum))
    # Last date of arrival
    pris_datum1 <- podatki4[,c("obdobje","id","tip_pod","pris_datum")]
    pris_datum2 <- arrange(pris_datum1, obdobje, id, tip_pod, desc(pris_datum))
    pris_datum3 <- distinct(pris_datum2, obdobje, id, tip_pod, .keep_all=TRUE) # if there are many rows with the same obdobje, id and tip_pod, keep the first one; keep all variables
    # Joint value of spr
    switch(fja,
           sum={podatki5 <- summarise_each(group_by(podatki4[,c("obdobje","id","tip_pod","spr")],obdobje,id,tip_pod), funs(sum))}, # summarise or summarize ...
           mean={podatki5 <- summarise_each(group_by(podatki4[,c("obdobje","id","tip_pod","spr")],obdobje,id,tip_pod), funs(mean))}, # summarise or summarize ...
           stop("The function isn't specified yet.")
    )
    # Number of data for each quarter
    st_pod1 <- summarise_each(group_by(podatki4[,c("obdobje","id","tip_pod","st_pod")],obdobje,id,tip_pod), funs(sum))
    # Produce final quarterly data
    podatki6 <- merge(podatki5,pris_datum3,by=c("obdobje","id","tip_pod")) # merge can be used only for two data frames
    podatki7 <- merge(podatki6,st_pod1,by=c("obdobje","id","tip_pod"))
    podatki8 <- mutate(podatki7,
                       spr = ifelse(st_pod==3,spr,NA),
                       pris_datum = ifelse(st_pod==3,pris_datum,NA),
                       pris_datum = base::as.Date(pris_datum)
    )
    podatki9 <- podatki8[,c("obdobje","id","tip_pod","spr","pris_datum")]
  }else{ # in case of first form
    if(!(colnames(podatki)[1]=="obdobje")){
      stop("The first variable should be obdobje and the second variable should start with P.")
    }
    podatki1 <- mutate(podatki,
                       leto = substr(obdobje,1,4),
                       mesec = as.numeric(substr(obdobje,6,7)),
                       cetrtletje = floor((mesec-1)/3)+1,
                       obdobje1 = paste(leto, "Q", as.character(cetrtletje), sep="")
    )
    podatki2 <- select(podatki1, -obdobje, obdobje=obdobje1, -leto, -mesec, -cetrtletje)
    red <- append(colnames(podatki2)[length(colnames(podatki2))],colnames(podatki2)[-length(colnames(podatki2))])
    podatki3 <- podatki2[,red]
    switch(fja,
           sum={podatki9 <- summarise_each(group_by(podatki3,obdobje), funs(sum))}, # summarise or summarize ...
           mean={podatki9 <- summarise_each(group_by(podatki3,obdobje), funs(mean))}, # summarise or summarize ...
           stop("The function isn't specified yet.")
    )
  }
  return(podatki9)
}

# Definition of function: preoblikuj_pris
# This function takes data frame with variables obdobje, spr, pris_datum, id, tip_pod
# and returns data frame obdobje x id | spr for data available before date datum and of chosen type (raw, edited, or both).
preoblikuj_pris <- function(podatki,tip,datum){
  # Variables obdobje, spr, pris_datum, id, tip_pod must be included.
  if(!("obdobje" %in% colnames(podatki) & "spr" %in% colnames(podatki) & "pris_datum" %in% colnames(podatki) & "id" %in% colnames(podatki) & "tip_pod" %in% colnames(podatki))){
    stop("The data don't include all the necessary variables obdobje, spr, pris_datum, id, tip_pod.")
  }
  if (tip=="s" | tip=="u") {
    podatki1 <- filter(podatki, podatki$tip_pod == tip)
  } else if (tip=="su") {
    podatki1 <- filter(podatki, podatki$tip_pod == "s" | podatki$tip_pod == "u")
  } else {
    stop("The value of v_tip isn't appropriate.")
  }
  podatki2 <- filter(podatki1, podatki1$pris_datum < datum)
  podatki3 <- arrange(podatki2, obdobje, id, desc(pris_datum), desc(tip_pod)) # first, we prefer data that came later; second, we prefer edited data
  podatki4 <- distinct(podatki3, obdobje, id, .keep_all=TRUE) # if there are many rows with the same obdobje and id, keep the first one; keep all variables
  podatki5 <- podatki4[,c("obdobje","id","spr")]
  podatki6 <- cast(podatki5, obdobje~id, value="spr")
  return(podatki6)
}

# Definition of function: spremeni_datum
# This function converts date datum e.g. 02JUN2009 to 2009-06-02.
spremeni_datum <- function(datum){
  if(!nchar(datum)==9){stop("The date variable doesn't have appropriate length. The length should be 9, e.g. 01JUN2009.")}
  dan_char <- substr(datum,1,2)
  mesec_char_b <- substr(datum,3,5)
  switch(mesec_char_b,
         JAN={mesec_char <- '01'},
         FEB={mesec_char <- '02'},
         MAR={mesec_char <- '03'},
         APR={mesec_char <- '04'},
         MAY={mesec_char <- '05'},
         JUN={mesec_char <- '06'},
         JUL={mesec_char <- '07'},
         AUG={mesec_char <- '08'},
         SEP={mesec_char <- '09'},
         OCT={mesec_char <- '10'},
         NOV={mesec_char <- '11'},
         DEC={mesec_char <- '12'},
         stop("The value for month isn't appropriate.")
  )
  leto_char <- substr(datum,6,9)
  datum_char <- paste(leto_char,mesec_char,dan_char,sep="-")
  return(datum_char)
}

# Definition of function: zdruzi_podatke
# This function returns the data frame, in which all given data frames are joint together by a common variable obdobje.
# Parameter konec is an element of v_konec.
# Parameter poti is the second part of path (e.g. pot2).
zdruzi_podatke <- function(v_pod_vir,v_tip,poti,konec){
  N <- length(v_pod_vir)
  if(length(v_tip) != N){
    stop("Vector v_tip should have the same length as vector v_pod_vir.")
  }
  for(i in 1:N){
    pod_vir_ime <- ime_tip(v_pod_vir[i])[1]
    pod_vir_tip <- ime_tip(v_pod_vir[i])[2]
    if(pod_vir_tip=="csv"){
      pod_vir <- read.csv(file=paste(pot1, poti, "/", v_pod_vir[i], sep=""))
      names(pod_vir)
      if("pris_datum" %in% colnames(pod_vir)){ # give the variables the same format as they would have if they came from sas7bdat
        pod_vir <- mutate(pod_vir, obdobje = as.character(obdobje))
        pod_vir <- mutate(pod_vir, tip_pod = as.character(tip_pod))
        pod_vir <- mutate(pod_vir, id = as.character(id))
        pod_vir <- mutate(pod_vir, pris_datum = as.character(pris_datum))
        pod_vir <- mutate(pod_vir, pris_datum = sapply(pris_datum, FUN=spremeni_datum))
        pod_vir <- mutate(pod_vir, pris_datum = as.Date(pris_datum))
        mode(pod_vir$obdobje) # should be character
        class(pod_vir$obdobje) # should be character
        mode(pod_vir$spr) # should be numeric
        class(pod_vir$spr) # should be numeric
        mode(pod_vir$pris_datum) # should be numeric
        class(pod_vir$pris_datum) # # should be Date
        mode(pod_vir$tip_pod) # should be character
        class(pod_vir$tip_pod) # should be character
        mode(pod_vir$id) # should be character
        class(pod_vir$id) # should be character
      }
    }
    if(pod_vir_tip=="sas7bdat"){pod_vir <- read_sas(paste(pot1, poti, "/", v_pod_vir[i], sep=""))} # obdobje is imported as character
    tip <- v_tip[i]
    if("pris_datum" %in% colnames(pod_vir)){ # in case of second form
      # 0. Check t_plus
      if(!is.numeric(t_plus)){
        stop("Data is in second form but t_plus equals NA.")
      }else{
        if(t_plus <= 0){
          stop("Data is in second form but t_plus is smaller or equal to zero.")
        }
      }
      # 1. Check if all the variables are present
      if(!("obdobje" %in% colnames(pod_vir) & "spr" %in% colnames(pod_vir) & "pris_datum" %in% colnames(pod_vir) & "id" %in% colnames(pod_vir) & "tip_pod" %in% colnames(pod_vir))){
        stop("The data don't include all the necessary variables obdobje, spr, pris_datum, id, tip_pod.")
      }
      # 2. In case of quarterly data for time series of interest and monthly enterprise data,
      # change monthly enterprise data to quarterly enterprise data using e.g. mean.
      # (This change might take a long time to process.)
      if(substring(pod_vir$obdobje[1],5,5)=="M" & substring(vrsta_vir$obdobje[1],5,5)=="Q"){
        pod_vir1 <- mes_v_cet(pod_vir,"mean")
      }else{
        pod_vir1 <- pod_vir
      }
      # 3. Remove unnecessary values for obdobje
      pod_vir2 <- filter(pod_vir1, obdobje>=zacetek & obdobje<=konec)
      # 4. Change into the first form, taking into account pris_datum
      pod_vir3 <- preoblikuj_pris(pod_vir2, tip, zadnji_dan(konec) + t_plus)
    }else{ # in case of first form
      # 1. Check the first variable
      if(!(colnames(pod_vir)[1]=="obdobje")){
        stop("The first variable should be obdobje and the second variable should start with P.")
      }
      # 2. In case of quarterly data for time series of interest and monthly enterprise data,
      # change monthly enterprise data to quarterly enterprise data using e.g. mean.
      # (This change might take a long time to process.)
      if(substring(pod_vir$obdobje[1],5,5)=="M" & substring(vrsta_vir$obdobje[1],5,5)=="Q"){
        pod_vir1 <- mes_v_cet(pod_vir,"mean")
      }else{
        pod_vir1 <- pod_vir
      }
      # 3. Remove unnecessary values for obdobje
      pod_vir2 <- filter(pod_vir1, obdobje>=zacetek & obdobje<=konec)
      # 4. Copy
      pod_vir3 <- pod_vir2
    }
    # 5. Check obdobje
    preveri_obdobje(pod_vir3$obdobje)
    # obdobje should also be from zacetek ...
    if(pod_vir3$obdobje[1] != zacetek){
      stop("Variable obdobje doesn't begin with zacetek!")
    }
    # ... to konec ...
    if(pod_vir3$obdobje[length(pod_vir3$obdobje)] != konec){
      stop("Variable obdobje doesn't end with konec!")
    }        
    # ... and there should be at least one column P... without any NAs
    st_na <- colSums(is.na(pod_vir3)) # vector with number of NAs for each column
    if(length(which(st_na==0)) <= 1){
      stop("Variable obdobje is the only variable without NAs. So this source is useless.")
    }
    # In general, there could be columns that have only NAs. But we don't remove them.
    # 6. Different sources
    if(i==1){ # first source
      podatki_vir <- pod_vir3
    }else{ # second etc. source
      podatki_vir <- merge(podatki_vir,pod_vir3,by="obdobje")
    }
  }
  return(podatki_vir)
}

#######################################################################################
# 3. THE MAIN FUNCTION OCENI_VRSTO STEP BY STEP
#######################################################################################

# Some parameters for the main function: oceni_vrsto
konec <- v_konec[1]
konec_prej <- v_konec_prej[1]
izberi_prve_pogoj <- v_izberi_prve_pogoj[1]
optim_regresija_smer <- v_optim_regresija_smer[1]
optim_regresija_pogoj <- v_optim_regresija_pogoj[1]
sezona <- v_sezona[1]

# Import time series of interest:

# - Import the series of interest (the data of the time series I wish to estimate)
vrsta_vir <- read.csv(file=paste(pot1, pot3, "/", vr_vir, sep=""))
#vrsta_vir

  #######################################################################################
  # DATA
  #######################################################################################

  # Names and extensions of files
  vr_vir_ime <- ime_tip(vr_vir)[1]
  vr_vir_tip <- ime_tip(vr_vir)[2]

  # Time series of interest data: vrsta_vir  
  # Remove all the rows before the beginning (zacetek) and after the ending (konec, in case konec is present
  # in the data, or konec_prej, in case konec isn't present in the data but konec_prej is).
  # If zacetek or konec_prej are not present in the data, an error is given, e.g.:
  konec_vr <- which(vrsta_vir[,1] == konec)
  konec_prej_vr <- which(vrsta_vir[,1] == konec_prej)
  if(length(konec_prej_vr) == 0){
    stop("The ending (konec_prej_vr and konec) isn't present in the series of interest.")
  }else{
    if(length(konec_vr) == 0){
      vrsta <- odstrani_obdobje_konec(odstrani_obdobje_zacetek(vrsta_vir),konec_prej)
    }else{
      vrsta <- odstrani_obdobje_konec(odstrani_obdobje_zacetek(vrsta_vir),konec)
    }
  }

  # All enterprise data: podatki_vir
  podatki_vir <- zdruzi_podatke(v_pod_vir,v_tip,pot2,konec)

  # Reference names for enterprise data
  podatki_vir_ref <- referenca(v_pod_vir,v_pod_vir_ref1,v_pod_vir_ref2,"-")
  v_pod_vir_ref <- paste(v_pod_vir, collapse="-")
  v_tip_ref <- paste(v_tip, collapse="-")
  
  # Other predictors (i.e. all other predictors, except for seasonality)
  if(length(v_dr_vir)>0){
    v_dr_tip <- rep(1, times=length(v_dr_vir)) # create an unused parameter for zdruzi_podatke
    drugi_vir <- zdruzi_podatke(v_dr_vir,v_dr_tip,pot5,konec)
    # Remove all the rows before the beginning (zacetek), after the ending (an element of v_konec) and remove columns with any NAs.
    # If zacetek or konec are not present in the data, an error is given, e.g.:
    # Error in odstrani_obdobje_zacetek(podatki_vir1) : The beginning (zacetek) isn't present in the data.
    drugi <- odstrani_nepopolne(odstrani_obdobje_konec(odstrani_obdobje_zacetek(drugi_vir),konec))
    #dim(drugi)
  }
  
  # Reference name for other predictors
  v_dr_vir_ref <- paste(v_dr_vir, collapse="-")
  
  # Enterprises that have data for the period konec in the source file
  # - Data
  podatki_vir_konec <- podatki_vir[which(podatki_vir$obdobje==konec),]
  podatki_vir_konec
  if(dim(podatki_vir_konec)[1] == 0){
    stop("The ending (konec) isn't present in the original enterprise data (i.e. at least one source doesn't contain the ending - exclude such source from v_pod_vir).")
  }
  # - Number of enterprises
  st_podj_konec <- length(podatki_vir_konec[-which(is.na(podatki_vir_konec))]) - 1
  st_podj_konec
  # - Sum of enterprise values
  #vsota_podj_konec <- sum(podatki_vir_konec[-1], na.rm=TRUE)
  vsota_podj_konec <- sum(as.numeric(podatki_vir_konec[-1]), na.rm=TRUE)
  vsota_podj_konec

  # Remove all the rows before the beginning (zacetek), after the ending (konec) and remove columns with any NAs.
  # If zacetek or konec are not present in the data, an error is given, e.g.:
  # Error in odstrani_obdobje_zacetek(podatki_vir1) : The beginning (zacetek) isn't present in the data.
  podatki <- odstrani_nepopolne(odstrani_obdobje_konec(odstrani_obdobje_zacetek(podatki_vir),konec))
  dim(podatki)
  # - Data
  podatki_konec <- podatki[which(podatki$obdobje==konec),]  
  podatki_konec
  # - Number of enterprises
  st_podj_podatki <- dim(podatki)[2] - 1
  st_podj_podatki
  # - Sum of enterprise values
  vsota_podj_podatki <- sum(podatki_konec[-1], na.rm=TRUE)
  vsota_podj_podatki
  
  # Unweighted share (in %) of number of enterprises from podatki in podatki_vir for period konec
  perc_st_podj <- round((st_podj_podatki/st_podj_konec)*100, digits=2)
  perc_st_podj
  
  # Unweighted share (in %) of data from podatki in podatki_vir for period konec
  perc_vsota_podj <- round((vsota_podj_podatki/vsota_podj_konec)*100, digits=2)
  perc_vsota_podj
  
  # Beginning: year, month
  zacetek_leto <- as.integer(substring(zacetek,1,4)) # year
  zacetek_mesec <- as.integer(substring(zacetek,6,nchar(zacetek))) # month or quarter
  
  # Periodicity (monthly/quarterly): 12 for monthly, 4 for quarterly data
  per <- periodika(podatki)
  per
  
  # Length
  l_podatki <- dim(podatki)[1]
  l_podatki
  l_vrsta <- dim(vrsta)[1]
  l_vrsta
  
  #######################################################################################
  # PRINCIPAL COMPONENT ANALYSIS (PCA)
  #######################################################################################
  
  # Standardize the enterprise data: each enterprise variable has the average of 0 and the variance of 1
  # The first variable (obdobje) needs to be removed because for standardization, we can have only numeric variables.
  dim(podatki)
  podatki1 <- scale(podatki[,-1])
  dim(podatki1)
  
  # Test if the first enterprise's variable is standardized
  mean(podatki1[,1]) # should be (approximately) 0
  var(podatki1[,1]) # should be (approximately) 1
  
  # Time series for enterprise data and interesting data
  ts_podatki <- ts(podatki1, start=c(zacetek_leto,zacetek_mesec), freq=per)
  # ts_podatki
  ts_vrsta <- ts(vrsta[,-1], start=c(zacetek_leto,zacetek_mesec), freq=per)
  #ts_vrsta
  
  # Check how many missing values are in the last row of enterprise data (just for safety) - it should be 0
  #sum(is.na(ts_podatki[nrow(podatki1),]))
  
  # Eigenvalues and eigenvectors of covariance matrix (= variance-covariance matrix)
  e <- eigen(ts_podatki%*%t(ts_podatki))
  
  # Eigenvalues
  #e$values # they are given in descending order
  #par(mfrow = c(1, 1))
  #plot(e$values) # plot
  
  # How many first principal components to choose?
  # Use function izberi_prve(vred,pogoj,vrsta), where vred is a vector of eigenvalues and
  # pogoj can be "Kaiser", "zadnja5", "po20", "po10", "po15" or a string containing a number (e.g. "80").
  n_pca <- izberi_prve(e$values,izberi_prve_pogoj,vrsta)
  n_pca
  
  # How much variability of the enterprise data do the chosen p. c. explain?
  perc_pca <- round(sum(e$values[1:n_pca])/sum(e$values)*100, digits=2) # rounding: e.g. 86.745 is rounded to 86.74, because 4 is an even number 
  perc_pca
  
  # Choose principal components 
  komponente <- e$vectors[,1:n_pca]
  komponente <- as.matrix(komponente) # If n_pca > 1, komponente doesn't change.
  # If n_pca = 1, this is a transformation from a row to a column; if we don't do it, we get an error.

  #######################################################################################
  # SEASONALITY
  #######################################################################################  
  
  # Seasonality: if sezona = stl: decomposition of vrsta with function stl, standardization of seasonal component
  if(sezona=="stl"){
    # Time series of interest
    vrsta
    ts_vrsta
    
    # Time series of interest from zacetek to konec_prej
    vrsta_prej <- odstrani_obdobje_konec(vrsta,konec_prej)
    vrsta_prej
    ts_vrsta_prej <- ts(vrsta_prej[,-1], start=c(zacetek_leto,zacetek_mesec), freq=per)
    ts_vrsta_prej
    
    # Decomposition with function stl in package stats
    ts_vrsta_prej_raz <- stl(ts_vrsta_prej, s.window="periodic")
    ts_vrsta_prej_raz
    plot(ts_vrsta_prej_raz)
    
    # Seasonal component
    ts_vrsta_prej_s <- ts_vrsta_prej_raz$time.series[,1]
    ts_vrsta_prej_s # values for a certain month are the same for all years
    plot(ts_vrsta_prej_s)
    length(ts_vrsta_prej_s)
    
    # The value for konec is added to the seasonal component (the same as the value 1 year ago).
    ts_vrsta_s <- append(ts_vrsta_prej_s, ts_vrsta_prej_s[length(ts_vrsta_prej_s)-per+1])
    ts_vrsta_s
    length(ts_vrsta_s)
    
    # Standardized seasonal component
    vrsta_s <- scale(ts_vrsta_s)
    vrsta_s
    mean(vrsta_s) # should be (approximately) 0
    var(vrsta_s) # should be (approximately) 1
    dim(vrsta_s)
  }

  #######################################################################################
  # OTHER PREDICTORS
  #######################################################################################
  
  # If relevant, remove all the rows after the ending (konec)
  if(length(v_dr_vir)>0){
    if(dim(drugi)[2]>1){
      drugi1 <- as.matrix(odstrani_obdobje_konec(drugi,konec)[,-1])
      dim(drugi1)
      drugi_ref <- paste(names(drugi)[-1], collapse="-")
    }else{
      drugi_ref <- NA
    }
  }else{
    drugi_ref <- NA
  }
  
  #######################################################################################
  # LINEAR REGRESSION
  #######################################################################################
  
  # Estimate linear regression:
  #       Y = vrsta between zacetek and konec_prej
  #       X = prediktorji between zacetek and konec_prej
  # - vrsta has periods between zacetek and konec or between zacetek and konec_prej (i.e. 1 period before konec).
  # - prediktorji has periods between zacetek and konec.
  # - In linear regression, we use vrsta and prediktorji only for periods between zacetek and konec_prej.
  #   After calculating linear regression coefficients, we use these coefficients to calculate
  #   the estimate for vrsta for period konec from prediktorji for period konec.
  #   If we have a true value for vrsta for period konec, we also calculate the residual (difference) ...

  # Predictors for data from zacetek to konec
  if(sezona=="stl"){
    if(length(v_dr_vir)>0){
      if(dim(drugi)[2]>1){
        prediktorji <- scale(cbind(komponente, vrsta_s, drugi1)) # predictors are standardized
      }else{
        prediktorji <- scale(cbind(komponente, vrsta_s)) # predictors are standardized
      }
    }else{
      prediktorji <- scale(cbind(komponente, vrsta_s)) # predictors are standardized
    }
  }else{
    if(length(v_dr_vir)>0){
      if(dim(drugi)[2]>1){
        prediktorji <- scale(cbind(komponente, drugi1)) # predictors are standardized
      }else{
        prediktorji <- komponente # predictors are not standardized;
        # but they have the average of approximately zero and all the same variance, so we get the same results
        # as if they would be standardized (or there might be diferences on late decimals)
      }
    }else{
      prediktorji <- komponente # predictors are not standardized
    }
  }
  prediktorji

  # Linear regression for data from zacetek to konec_prej
  vrsta1 <- vrsta[1:(l_podatki-1),2]
  prediktorji1 <- prediktorji[1:(l_podatki-1),]
  regresija1 <- lm(vrsta1~prediktorji1)
  summary(regresija1)
  rsqadj1 <- summary(regresija1)$adj.r.squared
  if(sezona=="stl"){
    p_sezona <- round(summary(regresija1)$coefficients[n_pca+2,4], digits=4)
    if(length(v_dr_vir)>0){
      if(dim(drugi)[2]>1){
        p_drugi <- round(summary(regresija1)$coefficients[(n_pca+3):dim(summary(regresija1)$coefficients)[1],4], digits=4)
        p_drugi_s <- paste(p_drugi, collapse="-")
        p_drugi_d <- round(100*(length(which(p_drugi<0.05))/length(p_drugi)), digits=2)
      }else{
        p_drugi <- NA
        p_drugi_s <- NA
        p_drugi_d <- NA
      }
    }else{
      p_drugi <- NA
      p_drugi_s <- NA
      p_drugi_d <- NA
    }
  }else{
    p_sezona <- NA
    if(length(v_dr_vir)>0){
      if(dim(drugi)[2]>1){
        p_drugi <- round(summary(regresija1)$coefficients[(n_pca+2):dim(summary(regresija1)$coefficients)[1],4], digits=4)
        p_drugi_s <- paste(p_drugi, collapse="-")
        p_drugi_d <- round(100*(length(which(p_drugi<0.05))/length(p_drugi)), digits=2)
      }else{
        p_drugi <- NA
        p_drugi_s <- NA
        p_drugi_d <- NA
      }
    }else{
      p_drugi <- NA
      p_drugi_s <- NA
      p_drugi_d <- NA
    }
  }
  p_sezona
  p_drugi
  p_drugi_s
  p_drugi_d

  # Optimization:
  # - possible values for smer: "backward", "forward", "both";
  # - possibile values of pogoj: "AIC", "BIC", "NO".
  regresija <- optim_regresija(regresija1,optim_regresija_smer,optim_regresija_pogoj,l_podatki)
  # Correction for output value of the parameter optim_regresija_smer
  if(optim_regresija_pogoj == "NO"){
    optim_regresija_smer <- "/"
  }
  #summary(regresija)
  rsqadj <- summary(regresija)$adj.r.squared
  koeficienti <- regresija$coefficients
  #koeficienti
  # - Number of predictors after optimization: smaller or equal to n_pca
  n_pred <- length(koeficienti) - 1
  n_pred
  
  # Estimates (predictions)
  napovedi <- predict(regresija)
  #napovedi
  
  # -Residuals
  napake <- -summary(regresija)$residuals # opposite of residuals
  #napake
  #napovedi - vrsta[1:(l_podatki-1),2]# the same as napake

  napake_maxabs <- max(abs(napake))
  napake_maxabs
  napake_meanabs <- mean(abs(napake))
  napake_meanabs
  
  # Histogram of -residuals
  napake_povp <- mean(napake)
  napake_povp
  napake_stod <- sd(napake)
  napake_stod
  par(mfrow = c(1, 1))
  hist(napake)
  hist(napake, breaks=40, xlim=c(-200,200), freq=FALSE)
  curve(dnorm(x, mean=0, sd=napake_stod), col="red", add=TRUE)
  
  #par(mfrow = c(2, 2))
  #plot(regresija)
  # Check if your assumptions are met: on 'Residuals vs Fitted', there should be no pattern.
  # Check normality of the residuals' distribution: on 'Normal Q-Q', the points should fall on the line.
  # (See: https://www.r-bloggers.com/checking-glm-model-assumptions-in-r)

  # Relative -residuals ... in percentages
  if(length(vrsta1[abs(vrsta1) < 0.000001]) == 0){
    rel_napake <- (napake/vrsta1)*100
    rel_napake_maxabs <- max(abs(rel_napake))
    rel_napake_maxabs
    rel_napake_meanabs <- mean(abs(rel_napake))
    rel_napake_meanabs  
    rel_napake_stod <- sd(rel_napake)
    rel_napake_stod
  }else{
    rel_napake_maxabs <- NA
    rel_napake_meanabs <- NA
    rel_napake_stod <- NA
  }
  
  # Create a constat variable, bind the constant variable and predictors
  ena <- c(rep(1,l_podatki))
  X <- cbind(ena, prediktorji)
  #X

  # The estimate (prediction) for vrsta for konec
  napoved <- X[l_podatki,]%*%koeficienti
  napoved
  
  # All the estimates (predictions) for vrsta
  vrsta_hat <- X%*%koeficienti
  #vrsta_hat # the same as append(napovedi, napoved)
  ts_vrsta_hat <- ts(vrsta_hat, start=c(zacetek_leto,zacetek_mesec), freq=per)
  #ts_vrsta_hat

  # The value, residual, relative residual, growth for vrsta for konec
  if (dim(vrsta)[1] == dim(podatki)[1]){
    
    vrednost <- vrsta[l_podatki,2]
    napaka <- napoved - vrednost
    napaka_abs <- abs(napaka)

    if(abs(vrsta[l_podatki-1,2]) > 0.000001){
      vrednost_rast_1 <- ((vrednost/vrsta[l_podatki-1,2])-1)*100
      napoved_rast_1 <- ((napoved/vrsta[l_podatki-1,2])-1)*100
    }else{
      vrednost_rast_1 <- NA
      napoved_rast_1 <- NA
    }
    if(abs(vrsta[l_podatki-per,2]) > 0.000001){
      vrednost_rast_per <- ((vrednost/vrsta[l_podatki-per,2])-1)*100
      napoved_rast_per <- ((napoved/vrsta[l_podatki-per,2])-1)*100
    }else{
      vrednost_rast_per <- NA
      napoved_rast_per <- NA
    }
    rast_1_absraz <- abs(vrednost_rast_1 - napoved_rast_1)
    rast_per_absraz <- abs(vrednost_rast_per - napoved_rast_per)        
    
    if(abs(vrednost) > 0.000001){
      rel_napaka <- (napaka/vrednost)*100
      rel_napaka_abs <- abs(rel_napaka)
      rel_napoved <- (napoved/vrednost)*100
    }else{
      rel_napaka <- NA
      rel_napaka_abs <- NA
      rel_napoved <- NA
    }
    
  }else{
    
    vrednost <- NA
    napaka <- NA
    napaka_abs <- NA
    
    vrednost_rast_1 <- NA
    vrednost_rast_per <- NA
    if(abs(vrsta[l_podatki-1,2]) > 0.000001){
      napoved_rast_1 <- ((napoved/vrsta[l_podatki-1,2])-1)*100
    }else{
      napoved_rast_1 <- NA
    }
    if(abs(vrsta[l_podatki-per,2]) > 0.000001){
      napoved_rast_per <- ((napoved/vrsta[l_podatki-per,2])-1)*100
    }else{
      napoved_rast_per <- NA
    }
    rast_1_absraz <- NA
    rast_per_absraz <- NA
    
    rel_napaka <- NA
    rel_napaka_abs <- NA
    rel_napoved <- NA
    
  }
  vrednost
  napaka
  napaka_abs
  vrednost_rast_1
  napoved_rast_1
  vrednost_rast_per
  napoved_rast_per
  rast_1_absraz
  rast_per_absraz
  rel_napaka
  rel_napaka_abs
  rel_napoved
  
#   # A simple plot
#   primerjava <- data.frame(ts_vrsta, ts_vrsta_hat)
#   primerjava
#   ts.plot(primerjava)
#   
#   # A better plot
  y1 <- as.numeric(vrsta[,2])
  df1<-data.frame(x=1:length(y1),y=y1)
  y2 <- as.numeric(vrsta_hat)
  df2<-data.frame(x=1:length(y2),y=y2)
  ime <- "BDP_SC" # !!!
  # First option: English graph !!!
#   ggplot(df1,aes(x,y))+geom_line(aes(color="original value")) +
#     geom_line(data=df2,aes(color="estimate")) +
#     labs(color="Legend", x="Time", y="Value") +
#     ggtitle(paste("Comparison for", ime, sep=" "))
  # Second option: Slovenian graph !!!
  ggplot(df1,aes(x,y))+geom_line(aes(color="originalna vrednost")) +
    geom_line(data=df2,aes(color="ocena")) +
    labs(color="Legenda", x="Čas", y="Vrednost") +
    ggtitle(paste("Primerjava za", ime, sep=" "))
    
  # Global test of model assumptions
  # http://www.statmethods.net/stats/rdiagnostics.html
  # You might include this test or not!
  if(vkljuci_test_predp == "YES"){
    test_predp <- gvlma(regresija)
    # Value of Global Stat: possible results for test_predp2:
    # - "Assumptions acceptable."
    # - "Assumptions NOT satisfied!"
    test_predp1 <- summary(test_predp)
    test_predp2 <- test_predp1$Decision[1]    
  }else{
    test_predp2 <- "/"
  }
  
  # Test for Autocorrelated Errors:
  # e.g. p=0.208, i.e. no correlation among residuals (the null hypothesis (no correlation
  # among residuals from a linear model) is not rejected at 5% level of statistical significance)
  # In package car, the function durbinWatsonTest gives different results for p-value but the same results
  # for Autocorrelation and D-W Statistic every time, so I find it inappropriate.
  # durbinWatsonTest(regresija) # only for lag 1
  # durbinWatsonTest(regresija, max.lag=per) # for lags from 1 to per
  
  # Durbin-Watson normality test in R: function dwtest in package lmtest
  # See help or https://cran.r-project.org/web/packages/lmtest/lmtest.pdf
  # The null hypothesis of this test is that the autocorrelation of disturbances is 0.
  # So, if p-value < 0.05, we can reject independence of the residuals.
  #dwtest(regresija)
  p_dw <- dwtest(regresija)$p.value
  p_dw

  # Durbin-Watson normality test in R: function durbinWatsonTest in package car
  # Test for Autocorrelated Errors:
  # e.g. p=0.208, i.e. no correlation among residuals (the null hypothesis (no correlation
  # among residuals from a linear model) is not rejected at 5% level of statistical significance)
  # Seed is set, because otherwise we get different results for p-value, but the same results for Autocorrelation and D-W Statistic, each time we compute the statistics.
  set.seed(1)
  #durbinWatsonTest(regresija) # only for lag 1
  dwt <- durbinWatsonTest(regresija, max.lag=per) # for lags from 1 to per
  p_dw_1 <- dwt$p[1]
  p_dw_1
  p_dw_per <- dwt$p[per]
  p_dw_per
    
  # Shapiro-Wilks normality test in R: function shapiro.test in package stats
  # http://www.dummies.com/programming/r/how-to-test-data-normality-in-a-formal-way-in-r/
  # Interpretation of p-value of this test:
  # This p-value tells you what the chances are that the sample comes from a normal distribution.
  # when the p-value is lower than 0.05 (typical cut-off), one can conclude that the sample deviates from normality.
  #shapiro.test(napake)
  p_shapiro <- shapiro.test(napake)$p.value
  p_shapiro
  
  # Kolmogorov-Smirnov test: function ks.test in package stats
  # In our case, the null hypothesis is that napake is distributed normally.
  # If the p-value is lower than 0.05 (typical cut-off), one can conclude that the sample deviates from normality.
  #ks.test(napake,"pnorm", 0, sd(napake))
  p_ks <- ks.test(napake,"pnorm", 0, sd(napake))$p.value
  p_ks

  #######################################################################################
  # WHAT TO RETURN
  #######################################################################################  
  
  rezultati <- data.frame(verzija,pot1,pot2,pot3,pot4,pot5,zacetek,konec,konec_prej,v_pod_vir_ref,v_tip_ref,podatki_vir_ref,t_plus,vr_vir,izberi_prve_pogoj,optim_regresija_smer,optim_regresija_pogoj,sezona,p_sezona,v_dr_vir_ref,drugi_ref,p_drugi_s,p_drugi_d,per,l_podatki,l_vrsta,st_podj_konec,st_podj_podatki,perc_st_podj,perc_vsota_podj,n_pca,perc_pca,n_pred,rsqadj1,rsqadj,napake_maxabs,napake_meanabs,napake_stod,rel_napake_maxabs,rel_napake_meanabs,rel_napake_stod,vrednost,napoved,napaka,napaka_abs,rel_napaka,rel_napaka_abs,vrednost_rast_1,vrednost_rast_per,napoved_rast_1,napoved_rast_per,rast_1_absraz,rast_per_absraz,test_predp2,p_dw,p_dw_1,p_dw_per,p_shapiro,p_ks)
  rezultati  

# Again: (reference) names and extensions of data files
vr_vir_ime <- ime_tip(vr_vir)[1]
vr_vir_tip <- ime_tip(vr_vir)[2]
podatki_vir_ref <- referenca(v_pod_vir,v_pod_vir_ref1,v_pod_vir_ref2,"-")

# Save the results:
if(izhod_pripona==""){
  izhod <- paste("OBDELAVA_enkrat", verzija, vr_vir_ime, podatki_vir_ref, paste(v_izberi_prve_pogoj, collapse="-"), paste(v_optim_regresija_smer, collapse="-"), paste(v_optim_regresija_pogoj, collapse="-"), paste(v_sezona, collapse="-"), vkljuci_test_predp, rezultati$drugi_ref[1], sep="_")
}else{
  izhod <- paste("OBDELAVA_enkrat", verzija, vr_vir_ime, izhod_pripona, sep="_")
}
# - to RData:
#save(rezultati, file=paste(izhod, ".RData", sep=""))
# - to csv:
write.csv2(x=rezultati, file=paste(izhod, ".csv", sep=""), row.names=FALSE)

# Remove the results:
#rm(rezultati)

# Load the results:
#load(file=paste(izhod, ".RData", sep=""))

#######################################################################################
# 4. CALCULATE SOME COMMON DIAGNOSTICS
#######################################################################################

# Definition of function: skupna_ocena
# This function returns some common diagnostics of the simulation.
# Use delni=FALSE in case of skupna. Use delni=TRUE in case of skupna2.
skupna_ocena <- function(rez,delni){
  
  diag_st_podj_podatki_range <- range(rez$st_podj_podatki)
  diag_perc_st_podj_range <- range(rez$perc_st_podj)
  diag_perc_vsota_podj_range <- range(rez$perc_vsota_podj)
  
  diag_n_pca_range <- range(rez$n_pca)
  diag_perc_pca_range <- range(rez$perc_pca)
  diag_n_pred_range <- range(rez$n_pred)
  diag_rsqadj_range <- round(range(rez$rsqadj), digits=4)
  
  diag_napake_maxabs <- round(max(rez$napake_maxabs), digits=4)
  diag_rel_napake_maxabs <- round(max(rez$rel_napake_maxabs), digits=4)
  
  diag_napake_meanabs <- round(mean(rez$napake_meanabs), digits=4)
  diag_rel_napake_meanabs <- round(mean(rez$rel_napake_meanabs), digits=4)
  
  diag_napaka_maxabs <- round(max(abs(rez$napaka)), digits=4)
  diag_rel_napaka_maxabs <- round(max(abs(rez$rel_napaka)), digits=4)
  
  diag_napaka_meanabs <- round(mean(abs(rez$napaka)), digits=4)
  diag_napaka_meansq <- round(mean((rez$napaka)^2), digits=4)
  diag_rel_napaka_meanabs <- round(mean(abs(rez$rel_napaka)), digits=4)
  diag_rel_napaka_meansq <- round(mean((rez$rel_napaka)^2), digits=4)
  
  d <- round(length(rez$napaka[which(abs(rez$napaka) <= rez$napake_maxabs)])/dim(rez)[1], digits=4)
  diag_d_napake_maxabs <- c(d, 1-d)
  
  d <- round(length(rez$napaka[which(abs(rez$napaka) <= rez$napake_meanabs)])/dim(rez)[1], digits=4)
  diag_d_napake_meanabs <- c(d, 1-d)
  
  d <- round(length(rez$napaka[which(abs(rez$napaka) <= rez$napake_stod)])/dim(rez)[1], digits=4)
  diag_d_napake_stod <- c(d, 1-d)
  
  diag_rast_1_absraz_range <- round(range(rez$rast_1_absraz), digits=4)
  diag_rast_1_absraz_mean <- round(mean(rez$rast_1_absraz), digits=4)
  diag_rast_1_absraz_meansq <- round(mean((rez$rast_1_absraz)^2), digits=4)
  diag_rast_per_absraz_range <- round(range(rez$rast_per_absraz), digits=4)
  diag_rast_per_absraz_mean <- round(mean(rez$rast_per_absraz), digits=4)
  diag_rast_per_absraz_meansq <- round(mean((rez$rast_per_absraz)^2), digits=4)
  
  if(vkljuci_test_predp == "YES"){
    d <- round(length(rez$test_predp2[which(rez$test_predp2 == "Assumptions acceptable.")])/dim(rez)[1], digits=4)
    diag_d_test_predp2 <- c(d, 1-d)    
  }else{
    diag_d_test_predp2 <- NA  
  }
  
  d <- round(length(rez$p_dw[which(rez$p_dw >= 0.05)])/dim(rez)[1], digits=4)
  diag_d_p_dw <- c(d, 1-d)
  
  d <- round(length(rez$p_dw_1[which(rez$p_dw_1 >= 0.05)])/dim(rez)[1], digits=4)
  diag_d_p_dw_1 <- c(d, 1-d)
  
  d <- round(length(rez$p_dw_per[which(rez$p_dw_per >= 0.05)])/dim(rez)[1], digits=4)
  diag_d_p_dw_per <- c(d, 1-d)
  
  d <- round(length(rez$p_shapiro[which(rez$p_shapiro >= 0.05)])/dim(rez)[1], digits=4)
  diag_d_p_shapiro <- c(d, 1-d)
  
  d <- round(length(rez$p_ks[which(rez$p_ks >= 0.05)])/dim(rez)[1], digits=4)
  diag_d_p_ks <- c(d, 1-d)
  
  if(rez$sezona[1]=="stl"){
    d <- round(length(rez$p_sezona[which(rez$p_sezona < 0.05)])/dim(rez)[1], digits=4)
    diag_d_p_sezona <- c(d, 1-d)
  }else{
    diag_d_p_sezona <- NA
  }
  
  diag_p_drugi_d_mean <- round(mean(rez$p_drugi_d), digits=2)
  
  diag_verzija <- as.character(rez$verzija[1])
  diag_pot1 <- as.character(rez$pot1[1])
  diag_pot2 <- as.character(rez$pot2[1])
  diag_pot3 <- as.character(rez$pot3[1])
  diag_pot4 <- as.character(rez$pot4[1])
  diag_pot5 <- as.character(rez$pot5[1])
  
  diag_zacetek <- as.character(rez$zacetek[1])
  diag_zadnji_konec <- v_konec[length(v_konec)]
  diag_st_koncev <- length(v_konec)
  
  diag_v_pod_vir_ref <- as.character(rez$v_pod_vir_ref[1])
  diag_v_tip_ref <- as.character(rez$v_tip_ref[1])
  diag_podatki_vir_ref <- as.character(rez$podatki_vir_ref[1])
  diag_t_plus <- as.numeric(rez$t_plus[1])
  diag_vr_vir <- as.character(rez$vr_vir[1])
  
  if(delni){ # for skupna2
    diag_izberi_prve_pogoj <- as.character(rez$izberi_prve_pogoj[1])
    diag_optim_regresija_pogoj <- as.character(rez$optim_regresija_pogoj[1])
    diag_optim_regresija_smer <- as.character(rez$optim_regresija_smer[1])
    diag_sezona <- as.character(rez$sezona[1])
  }else{ # for skupna
    diag_izberi_prve_pogoj <- paste(v_izberi_prve_pogoj, collapse="-")
    diag_optim_regresija_pogoj <- paste(v_optim_regresija_pogoj, collapse="-")
    if(diag_optim_regresija_pogoj == "NO"){
      diag_optim_regresija_smer <- "/"
    }else{
      diag_optim_regresija_smer <- paste(v_optim_regresija_smer, collapse="-")
    }
    diag_sezona <- paste(v_sezona, collapse="-")
  }
  
  diag_v_dr_vir_ref <- as.character(rez$v_dr_vir_ref[1])
  diag_drugi_ref <- as.character(rez$drugi_ref[1])
  diag_per <- as.numeric(rez$per[1])
  diag_l_vrsta_range <- as.numeric(c(min(rez$l_vrsta),max(rez$l_vrsta))) # c(min(rez$l_vrsta),max(rez$l_vrsta)) and range(rez$l_vrsta) both return e.g. 31:32
  
  skupna <- list(diag_verzija,diag_pot1,diag_pot2,diag_pot3,diag_pot4,diag_pot5,diag_zacetek,diag_zadnji_konec,diag_st_koncev,diag_v_pod_vir_ref,diag_v_tip_ref,diag_podatki_vir_ref,diag_t_plus,diag_vr_vir,diag_izberi_prve_pogoj,diag_optim_regresija_smer,diag_optim_regresija_pogoj,diag_sezona,diag_v_dr_vir_ref,diag_drugi_ref,diag_per,diag_l_vrsta_range,diag_st_podj_podatki_range,diag_perc_st_podj_range,diag_perc_vsota_podj_range,diag_n_pca_range,diag_perc_pca_range,diag_n_pred_range,diag_rsqadj_range,diag_napake_maxabs,diag_rel_napake_maxabs,diag_napake_meanabs,diag_rel_napake_meanabs,diag_napaka_maxabs,diag_rel_napaka_maxabs,diag_napaka_meanabs,diag_napaka_meansq,diag_rel_napaka_meanabs,diag_rel_napaka_meansq,diag_d_napake_maxabs,diag_d_napake_meanabs,diag_d_napake_stod,diag_rast_1_absraz_range,diag_rast_1_absraz_mean,diag_rast_1_absraz_meansq,diag_rast_per_absraz_range,diag_rast_per_absraz_mean,diag_rast_per_absraz_meansq,diag_d_test_predp2,diag_d_p_dw,diag_d_p_dw_1,diag_d_p_dw_per,diag_d_p_shapiro,diag_d_p_ks,diag_d_p_sezona,diag_p_drugi_d_mean)
  imena <- c("verzija","pot1","pot2","pot3","pot4","pot5","zacetek","zadnji_konec","st_koncev","v_pod_vir_ref","v_tip_ref","podatki_vir_ref","t_plus","vr_vir","izberi_prve_pogoj","optim_regresija_smer","optim_regresija_pogoj","sezona","v_dr_vir_ref","drugi_ref","per","diag_l_vrsta_range","diag_st_podj_podatki_range","diag_perc_st_podj_range","diag_perc_vsota_podj_range","diag_n_pca_range","diag_perc_pca_range","diag_n_pred_range","diag_rsqadj_range","diag_napake_maxabs","diag_rel_napake_maxabs","diag_napake_meanabs","diag_rel_napake_meanabs","diag_napaka_maxabs","diag_rel_napaka_maxabs","diag_napaka_meanabs","diag_napaka_meansq","diag_rel_napaka_meanabs","diag_rel_napaka_meansq","diag_d_napake_maxabs","diag_d_napake_meanabs","diag_d_napake_stod","diag_rast_1_absraz_range","diag_rast_1_absraz_mean","diag_rast_1_absraz_meansq","diag_rast_per_absraz_range","diag_rast_per_absraz_mean","diag_rast_per_absraz_meansq","diag_d_test_predp2","diag_d_p_dw","diag_d_p_dw_1","diag_d_p_dw_per","diag_d_p_shapiro","diag_d_p_ks","diag_d_p_sezona","diag_p_drugi_d_mean")
  names(skupna) <- imena
  return(skupna)
}

# Call the function for common diagnostics
skupna <- skupna_ocena(rezultati,FALSE)
skupna

# Save the results:
# - to RData:
#save(skupna, file=paste(izhod, "_skupna",".RData", sep=""))
# - to csv:
write.csv2(x=skupna, file=paste(izhod, "_skupna", ".csv", sep=""), row.names=FALSE)
