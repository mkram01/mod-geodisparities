##############################################
# Code author: Erin Stearns
# Code objective: Formatting config args
# Date: 5.7.2019
#############################################


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
  if (recode_binary == "False"){
    message("You have chose not to recode race/eth into a binary variable.")
  }
)

suppressWarnings(
  if (recode_binary == "Black"){
    binary_code <- 3
    message(paste0("You specified ", recode_binary, " as the race/ethnicity to recode as binary. ", binary_code, 
                   " is the assigned race/ethnicity encoding."))
  }
  
)
suppressWarnings(
  if (recode_binary == "Hispanic"){
    binary_code <- 2
    message(paste0("You specified ", recode_binary, " as the race/ethnicity to recode as binary. ", binary_code, 
                   " is the assigned race/ethnicity encoding."))
  }
)

######################################################################################################
# ----------------------------------------------- formatting model predictors ------------------------
######################################################################################################
message("From format_config_args.R script: Formatting model formula from config predictors")

# get selected covs
#selected_covs <- strsplit(fixed_effects," ")
#selected_covs <- selected_covs[[1]][selected_covs[[1]] != "+"]