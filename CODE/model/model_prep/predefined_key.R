##############################################
# Code author: Kevin Weiss, Erin Stearns
# Code objective: predefined objects & their keys
# Date: 5.7.2019
#############################################

#Message code:
message("From predefined_key.R script: Assigning FIPS codes to geography specified in config")

######################################################################################################
# ---------------------------------- Defining geography by predefined FIPS Code lists -------------- #
######################################################################################################
# ---- National
suppressWarnings(
  if(geography == "all"){
    message("From predefined_key.R script: You have selected all of the US as your model geography. Assigning vector of FIPS codes now!")
    geo_fips <- c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", "16",
                  "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
                  "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42",
                  "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56", "72")
  }
)

# ---- Census Regions
suppressWarnings(
if(geography == "south"){
  message("From predefined_key.R script: You have selected the South as your model geography. Assigning vector of FIPS codes now!")
  geo_fips <- c('10', '11', '12','13', '24', '37', '45', '51', '54', '01', '21', '28', '47', '05', '22', '40', '48')
  }
)

suppressWarnings(
if(geography == "west"){
  message("From predefined_key.R script: You have selected the West as your model geography. Assigning vector of FIPS codes now!")
  geo_fips <-  c('04', '08', '16', '30', '32', '35', '49', '56', '02', '06', '15', '41', '53')
  }
)

suppressWarnings(
if(geography == "midwest"){
  message("From predefined_key.R script: You have selected the Midwest as your model geography. Assigning vector of FIPS codes now!")
  geo_fips <-  c('17', '18', '26', '39', '55', '19', '20', '27', '29', '31', '38', '46')
  }
)

suppressWarnings(
if(geography == "northeast"){
  message("From predefined_key.R script: You have selected the Northeast as your model geography. Assigning vector of FIPS codes now!")
  geo_fips <- c('09', '23', '25', '33', '44', '50', '34', '36', '42')
}
)


# ---- Census Divisions
suppressWarnings(
if(geography == "newengland"){
  message("From predefined_key.R script: You have selected New England as your model geography. Assigning vector of FIPS codes now!")
  geo_fips <- c('09', '23', '25', '33', '44', '50')
  }
)

suppressWarnings(
if(geography == "midatlantic"){
  message("From predefined_key.R script: You have selected the Mid-Altantic as your model geography. Assigning vector of FIPS codes now!")
  geo_fips <- c('34', '36', '42')
  }
)

suppressWarnings(
if(geography == "southatlantic"){
  message("From predefined_key.R script: You have selected the South Altantic as your model geography. Assigning vector of FIPS codes now!")
  geo_fips <- c('10', '11', '12','13', '24', '37', '45', '51', '54')
  }
)

suppressWarnings(
if(geography == "esc"){
  message("From predefined_key.R script: You have selected the ESC as your model geography. Assigning vector of FIPS codes now!")
  geo_fips <- c('01', '21', '28', '47')
  }
)

suppressWarnings(
if(geography == "wsc"){
  message("From predefined_key.R script: You have selected the WSC as your model geography. Assigning vector of FIPS codes now!")
  geo_fips <- c('05', '22', '40', '48')
  }
)

suppressWarnings(
if(geography == "enc"){
  message("From predefined_key.R script: You have selected the ENC as your model geography. Assigning vector of FIPS codes now!")
  geo_fips <- c('17', '18', '26', '39', '55')
  }
)

suppressWarnings(
if(geography == "wnc"){
  message("From predefined_key.R script: You have selected the WNC as your model geography. Assigning vector of FIPS codes now!")
  geo_fips <- c('19', '20', '27', '29', '31', '38', '46')
  }
)

suppressWarnings(
if(geography == "pac"){
  message("From predefined_key.R script: You have selected the PAC as your model geography. Assigning vector of FIPS codes now!")
  geo_fips <- c('02', '06', '15', '41', '53')
  }
)

suppressWarnings(
if(geography == "mountain"){
  message("From predefined_key.R script: You have selected the PAC as your model geography. Assigning vector of FIPS codes now!")
  geo_fips <- c('04', '08', '16', '30', '32', '35', '49', '56')
  }
)











