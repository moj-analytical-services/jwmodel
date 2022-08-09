### jwmodel direct run ### 
### Henryk 09/08/2022 ### 

# ---- 0. Packages ----
# ---- Install package manager renv ----
# see instructions in moj analytical

# install Rs3tools package to load data from s3 
# renv::install("moj-analytical-services/Rs3tools")

# library(remotes)  # connect to GitHub
# library(botor)    # connect to cloud
# renv::install("readxl")
# renv::install("janitor")
library(tidyverse)
library(readxl)
library(openxlsx)
library(vctrs) # dplyr not loading correctly in local env unless this loaded first!!
library(dplyr)
library(tidyr)
library(officer)  # manipulate Word and PowerPoint documents
library(zip)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(DT)       # DataTables
library(gt) # requires rlang 0.4.6 which is NOT available from Conda for R3.5.1
library(colourpicker)
library(colorspace)
library(sf) # used for processing Region map data
library(validate) # used by jwmodel to undertake data validation
library(lpSolveAPI)
# library(rlang)
library(janitor)
# save snapshot to latest packages for this program
renv::snapshot()
# renv::restore()

# ---- I. Loading assumptions file ----
input_file_name <- "Judicial S&D input hf.xlsx"
input_file <- paste0("alpha-testhf/jmw_assumptions/", input_file_name)
# loading data input as input_file.xlsx from s3 bucket (working)
# Rs3tools::download_file_from_s3("alpha-testhf/jmw_assumptions/2022-08-04 Assumptions JCG Sept.xlsx", "input_file.xlsx", overwrite =TRUE)
Rs3tools::download_file_from_s3(input_file, "input_file.xlsx", overwrite =TRUE)
# Rs3tools::download_file_from_s3("alpha-testhf/jmw_assumptions/Judicial S&D input.xlsx", "input_file.xlsx", overwrite =TRUE)

# show all files in s3 bucket testhf 
botor::s3_ls("s3://alpha-testhf")

# Defining an S3 class for jwmodel 
source("R/jwmodel.R")

obj <- jwmodel()

# source the load component of jwmodel 
source("R/load.R")

# load obj, the list containing all worksheets
obj <- load_from_file.jwmodel(obj, "input_file.xlsx")

# ---- II. Optimization Function -----
# source utility functions, set up model, optimization
source("R/utility-functions.R")
source("R/initialise.R")
source("R/optimise.R")

# function to initialise optimisation
obj <- initialise.jwmodel(obj)

# run optimization
obj <- optimise(obj)

# ---- III. Results ----
# all results of simulation
result_temp <- obj$outputs

# ---- Allocation result ----
df <- as.data.frame(obj$outputs$allocation_output)

# save to csv file
write_csv(df, "./result/result_df.csv")

# analyze data
dfa <- as.data.frame(janitor::clean_names(df)) %>% 
  mutate(allocated_days = round(allocated, digits =0)) 

dfb <- dfa %>% select(year, jurisdiction, judge, allocated_days) %>% 
  group_by(jurisdiction, year ) %>%
  summarize(alloc = sum(allocated_days))
  
# plot result allocations
ggplot(dfb, aes(x = year, y = alloc, group = jurisdiction)) + 
         geom_line(aes(linetype=jurisdiction, color=jurisdiction))+
         geom_point(aes(color=jurisdiction))+
         theme(legend.position="top")         


# ---- Resource result ----         
# resource result (judge, status, fte)
df_res <- as.data.frame(obj$outputs$resource_output)

# save to csv file
write_csv(df_res, "./result/result_df_resource.csv")

# analyze data
dfa_res <- as.data.frame(janitor::clean_names(df_res)) %>% 
  mutate(fte = round(fte, digits =0)) 

dfb_res <- dfa_res %>% select(year, judge, status,  fte) %>% 
  group_by(judge, status, year ) %>%
  summarize(fte = sum(fte))

# plot result allocations
ggplot(dfb_res, aes(x = year, y = fte, group = judge)) + 
  geom_line(aes(linetype=judge, color=judge))+
  geom_point(aes(color=judge))+
  theme(legend.position="top")   
