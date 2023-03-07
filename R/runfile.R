library(readxl)
library(dplyr)
library(tidyr)
library(lpSolveAPI)
library(janitor)

# Load jwmodel package.
source("R/jwmodel.R")
source("R/load.R")
source("R/utility-functions.R")
source("R/initialise.R")
source("R/optimise.R")

# Load pivot table functions.
source("R/days_sat_yj.R")
source("R/available_yj.R")

# Path to assumptions.
file <- "data/2022-09-21 Assumptions.xlsx"

# Initialise jwmodel object.
obj <- jwmodel()

# Populate jwmodel object with assumptions.
obj <- load_from_file.jwmodel(obj, file)

# Initialise model objective function, decision variables and constraints (based on assumptions).
obj <- initialise.jwmodel(obj)

# Solve model.
obj <- optimise.jwmodel(obj)

# Populate jwmodel object with pivot tables.
obj <- days_sat_yj.jwmodel(obj)
obj <- available_yj.jwmodel(obj)



  
 


  
  
  



