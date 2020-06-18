# Defining an S3 class for Judicial Workforce Modelling


#source("utility-functions.R")

# Constructor function ----------------------------------------------------
#' Creates a jwmodel object
#' 
#' Recommended process to create and populate a \code{jwmodel} is to use this function
#' with default values to first create it, then use \code{\link{load_from_file}}
#' to populate it with parameter information contained in an appropriately 
#' formatted Excel file.
#' 
#' @param metadata extendable named list of model metadata, includes model 'name'
#'        and 'description' by default
#' @param judge_types data frame containing list of different judge types
#' @param jurisdictions list of jurisdictions; default empty
#' @param years list of years model will be run for, in accending order; default 
#'        empty
#' @param n_judges data frame of initial number of judges by judge type
#' @param judge_departures data frame of number of judges leaving the profession
#'        by judge type by year
#' @param sitting_days data frame of the sitting day capacity (avg, min, max) for
#'        each WTE judge, by judge type, by year
#' @param demand data frame of number of sitting days predicted to be required by 
#'        jurisdiction by year
#' @param judge_progression data frame holding the mapping of the proportion of 
#'        judges of one type who are recruited from the pool of another type
#' @param fixed_costs data frame... default empty
#' @param variable_costs data frame... default empty
#' @param penalty_costs data frame... default empty
#' @param constraints list of constraints (not used)
#' @param outputs list of ouputs (not used)
#' @param lpmodel default NULL
#' 
#' @return Returns an object of type \code{lpmodel}
#' @export
#' @examples 
#' # creates new, empty jwmodel object
#' mymodel <- jwmodel()
jwmodel <- function(metadata = list(name = "", description = ""),
                    judge_types = data.frame(), 
                    jurisdictions = c(),
                    years = c(), 
                    n_judges = data.frame(), 
                    judge_departures = data.frame(),
                    sitting_days = data.frame(), 
                    demand = data.frame(),
                    # TODO add scenarios
                    judge_progression = data.frame(),
                    fixed_costs = data.frame(),
                    variable_costs = data.frame(),
                    penalty_costs = data.frame(),
                    constraints = list(),
                    outputs = list(),
                    lpmodel = NULL
){
  
  value <- list(
    metadata = metadata,
    judge_types = judge_types,
    jurisdictions = jurisdictions,
    years = years,
    n_judges = n_judges,
    judge_departures = judge_departures,
    sitting_days = sitting_days,
    demand = demand,
    judge_progression = judge_progression,
    fixed_costs = fixed_costs,
    variable_costs = variable_costs,
    penalty_costs = penalty_costs,
    constraints = constraints,
    outputs = outputs,
    lpmodel = NULL
  )
  
  attr(value, "class") <- "jwmodel"
  
  return(value)
  
}


