library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(lpSolveAPI)
library(janitor)
library(ggplot2)
library(viridis)
library(rlang)

# Load jwmodel package.
source("R/model/jwmodel.R")
source("R/model/load.R")
source("R/model/utility-functions.R")
source("R/model/initialise.R")
source("R/model/optimise.R")

# Load pivot table functions.
source("R/outputs/days_sat_yj.R")
source("R/outputs/available_yj.R")
source("R/outputs/recruited_yj.R")
source("R/outputs/departed_yj.R")
source("R/outputs/report_xlsx.R")
# source("R/outputs/utils.R")

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

# Populate jwmodel object with pivot tables and graphs.
obj <- days_sat_yj.jwmodel(obj)
obj <- available_yj.jwmodel(obj)
obj <- recruited_yj.jwmodel(obj)
obj <- departed_yj.jwmodel(obj)

# Put pivot tables and graphs into an XLSX file.
report_xlsx(obj)






