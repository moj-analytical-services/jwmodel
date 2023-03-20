# set working directory on local R
setwd("C:/Users/hnc39o/Desktop/Modelling/code/jwmodel")

#set working directory on the AP
#setwd("~/jwmodel")

# Load packages
#renv::status() can be used to check for any packages missing from the snapshot
source("R/packages.R")

# Load jwmodel package
purrr::map(dir_ls("R/model", recurse = TRUE, glob = "*.R"), source)


# Load pivot table functions.
purrr::map(dir_ls("R/outputs", recurse = TRUE, glob = "*.R"), source)


# Path to assumptions.
# NOTE: file path below may need to be changed depending on use of AP or local R
file <- "~/jwmodel/R/2022-09-21 Assumptions Master.xlsx"

# Initialise jwmodel object.
obj <- jwmodel()

# Populate jwmodel object with assumptions.
obj <- load_from_file.jwmodel(obj, file)

# Initialise model objective function, decision variables and constraints (based on assumptions).
obj <- initialise.jwmodel(obj)

# Solve model.
obj <- optimise.jwmodel(obj)

# Populate jwmodel object with pivot tables and graphs.
obj <- days_sat_yj.jwmodel(obj)
obj <- available_yj.jwmodel(obj)
obj <- recruited_yj.jwmodel(obj)
obj <- departed_yj.jwmodel(obj)

# Put pivot tables and graphs into an XLSX file.
report_xlsx(obj)


  
  