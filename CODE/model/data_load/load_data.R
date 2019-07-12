##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: load data
# Date: 5.7.2019
#############################################

###################################3
# To-do items:

#name the adjacency file seeking
adjfilename <- paste0("knn_", geography, "_", k_numneighbors, '.adj')

#Conditionally load or create KNN
if (file.exists(paste0(data_repo, '/spatial/', adjfilename)) & create_knn_obj == FALSE){
  message(paste0("From load_data.R script: The KNN adjacency matrix for your geography already exists and you have elected not to recreate it. Loading it now!"))
  #load adjacency matrix
  model_knn <- inla.read.graph(paste0(data_repo, '/spatial/', adjfilename))
} else {
  message(paste0("From load_data.R script: Either the KNN adjacency matrix for your geography does not exist or you have elected to recreate it. Creating it now!"))
  source("CODE/model/data_load/create_data.R")
}


message("From load_data.R script: Finished loading and prepping data!")
