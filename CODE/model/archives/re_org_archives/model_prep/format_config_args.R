##############################################
# Code author: Erin Stearns
# Code objective: Formatting config args
# Date: 5.7.2019
#############################################

######################################################################################################
# ----------------------------------------------- formatting CRS -------------------------------------
######################################################################################################
message("From format_config_args.R script: Formatting coordinate reference system")
crs_proj <- as.numeric(crs_proj)

######################################################################################################
# ----------------------------------------------- formatting years for model -------------------------
######################################################################################################
message("From format_config_args.R script: Formatting config years")
year_start <- as.numeric(year_start)
year_end <- as.numeric(year_end)
year_span <- year_start:year_end

######################################################################################################
# ----------------------------------------------- formatting race/ethnicity --------------------------
######################################################################################################
message("From format_config_args.R script: Formatting config race/ethnicity")
race_eth <- strsplit(race_eth, " ")
race_eth <- race_eth[[1]][race_eth[[1]] != "+"]
race_eth <- c(as.numeric(race_eth))

######################################################################################################
# ----------------------------------------------- formatting recode_binary ---------------------------
######################################################################################################
message("From format_config_args.R script: Formatting config recode binary")

suppressWarnings(
  if (recode_binary == "nonbinary"){
    message("You have chosen not to recode race/eth into a binary variable.")
  }
)

suppressWarnings(
  if (recode_binary == "black"){
    binary_code <- 2
    message(paste0("You specified ", recode_binary, " as the race/ethnicity to recode as binary. ", binary_code, 
                   " is the assigned race/ethnicity encoding."))
  }
  
)
suppressWarnings(
  if (recode_binary == "hispanic"){
    binary_code <- 3
    message(paste0("You specified ", recode_binary, " as the race/ethnicity to recode as binary. ", binary_code, 
                   " is the assigned race/ethnicity encoding."))
  }
)

######################################################################################################
# ----------------------------------------------- formatting knn k arg -------------------------------
######################################################################################################
if(sp_weights_method == "knn"){
  message("From format_config_args.R script: Formatting KNN k arg")
  #converting from character to numeric
  k_numneighbors <- as.numeric(k)
}

######################################################################################################
# ----------------------------------------------- formatting random intercept and slope --------------
######################################################################################################
message("From format_config_args.R script: Formatting model namimg descriptors")

#formatting for model naming
if (random_slope == FALSE){
  ran_slope <- ""
} else {
  ran_slope <- "rs_"
}

if (random_intercept == FALSE){
  ran_int <- ""
} else {
  ran_int <- "ri_"
}

######################################################################################################
# ----------------------------------------------- formatting model family ---------------------------
######################################################################################################
message("From format_config_args.R script: Formatting model formula from config predictors")
inla_formula <- as.formula(formula)


######################################################################################################
# ----------------------------------------------- formatting model predictors ------------------------
######################################################################################################
message("From format_config_args.R script: Formatting model formula from config predictors")
inla_formula <- as.formula(formula)
if(is.formula(inla_formula) == TRUE){
  message("Congrats! Your formula is a real formula.")
} else {
  message("Eeks. Your aspiring formula appears to still be aspiring. Please check your config formula arg.")
}

