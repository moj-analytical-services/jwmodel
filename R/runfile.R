# Load packages.
renv::status() # checks for any packages missing from the snapshot.
source("R/packages.R")

# Load jwmodel package.
purrr::map(dir_ls("R/model", recurse = TRUE, glob = "*.R"), source)

# Load pivot table functions.
purrr::map(dir_ls("R/outputs", recurse = TRUE, glob = "*.R"), source)

# Load data from S3.
file <- "data/2022-09-21 Assumptions Master.xlsx"

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

# Run Rmarkdown report. 
rmarkdown::render(input = "R/outputs/summary_report.Rmd", 
                  output_format = "html_document")
  
  