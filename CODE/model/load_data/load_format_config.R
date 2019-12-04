##############################################
# Code author: Erin Stearns
# Code objective: Loading, formatting & setting defaults for config
# Date: 5.7.2019
#############################################

#Message code:
message("From `load_format_config.R` script: Loading config file now")

######################################################################################################
# ----------------------------------------------- calling config function ----------------------------
######################################################################################################
# Read config file and save all parameters in memory
config <- load_config(data_repo = data_repo)

######################################################################################################
# ----------------------------------------------- setting config defaults ----------------------------
######################################################################################################
#Create defaults
#Message code:
message("From `load_format_config.R` script: Setting config defaults if any args left empty")

# outcome
if(is.na(outcome)){
  outcome <- "ptb"
}

# denominator
if(is.na(denominator)){
  if(outcome != "etb"){
    denominator <- "births"
  } else {
    denominator <- "term"
  }
}

#geography
if(is.na(geography)){
  geography <- "all"
}

#projection
if(is.na(crs_proj)){
  crs_proj <- "102003"
}

#model type
if(is.na(model_type)){
  model_type <- "inla"
}

#family
if(is.na(family)){
  family <- "poisson"
}

#spatial weighting methods
if(is.na(sp_weights_method)){
  sp_weights_method <- "soi"
}

# model formula -- this one cannot be empty
if(is.na(formula)){
  message("You have not provided a formula in your config file. You need to do so to run a model.")
  stop()
}

#year start
if(is.na(year_start)){
  year_start <- "2007"
}

#year end
if(is.na(year_end)){
  year_start <- "2017"
}

#race/ethnicity
if(is.na(race_eth)){
  race_eth <- "1 + 2"
}

#recode binary using which race/ethnicity?
if(is.na(recode_binary)){
  recode_binary <- "black"
}

#create sf object for model area?
if(is.na(create_sf_obj)){
  create_sf_obj <- "FALSE"
}

#k - how many neighbors needed for KNN method, if specified
if(is.na(k)){
  k <- "6"
}

#create a new spatial weights matrix for the model area?
if(is.na(create_spwts)){
  create_spwts <- "FALSE"
}

# predictors - though only used for naming, this too will stop model if empty
if(is.na(predictors)){
  message("You have not provided a set of predictors in your config file. You need to do so to run a model.")
  stop()
}

#random slope - though only used for naming, this too will stop model if empty
if(is.na(random_slope)){
  message("You have not provided a random slope arg in your config file. You need to do so to run a model.")
  stop()
}

#random slope - though only used for naming, this too will stop model if empty
if(is.na(random_intercept)){
  message("You have not provided a random intercept arg in your config file. You need to do so to run a model.")
  stop()
}

#visualization produced at end of model run?
if(is.na(visualize)){
  visualize <- "FALSE"
}

#save model object as a .RDS file at end of model run?
if(is.na(save_mod)){
  save_mod <- "FALSE"
}


# ---------------------------------------------- FORMATTING CONFIG ARGS --------------------------------------------

######################################################################################################
# ----------------------------------------------- formatting CRS -------------------------------------
######################################################################################################
message("From `load_format_config.R` script: Formatting coordinate reference system")
crs_proj <- as.numeric(crs_proj)

######################################################################################################
# ----------------------------------------------- formatting years for model -------------------------
######################################################################################################
message("From `load_format_config.R` script: Formatting config years")
year_start <- as.numeric(year_start)
year_end <- as.numeric(year_end)
year_span <- year_start:year_end

######################################################################################################
# ----------------------------------------------- formatting race/ethnicity --------------------------
######################################################################################################
message("From `load_format_config.R` script: Formatting config race/ethnicity")
race_eth <- strsplit(race_eth, " ")
race_eth <- race_eth[[1]][race_eth[[1]] != "+"]
race_eth <- c(as.numeric(race_eth))

######################################################################################################
# ----------------------------------------------- formatting recode_binary ---------------------------
######################################################################################################
message("From `load_format_config.R` script: Formatting config recode binary")

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
  message("From `load_format_config.R` script: Formatting KNN k arg")
  #converting from character to numeric
  k_numneighbors <- as.numeric(k)
}

######################################################################################################
# ----------------------------------------------- formatting random intercept and slope --------------
######################################################################################################
message("From `load_format_config.R` script: Formatting model naming descriptors")

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
message("From `load_format_config.R` script: Formatting model formula from config predictors")
inla_formula <- as.formula(formula)


######################################################################################################
# ----------------------------------------------- formatting model predictors ------------------------
######################################################################################################
message("From `load_format_config.R` scriptt: Formatting model formula from config predictors")
inla_formula <- as.formula(formula)
if(is.formula(inla_formula) == TRUE){
  message("Congrats! Your formula is a real formula.")
} else {
  message("Eeks. Your aspiring formula appears to still be aspiring. Please check your config formula arg.")
}


######################################################################################################
# ----------------------------------------------- validating config args -----------------------------
######################################################################################################
#Validate that year_end comes after year_start

message("From `load_format_config.R` script: Validating config args")
