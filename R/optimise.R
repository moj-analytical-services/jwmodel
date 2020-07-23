
#' Runs the optimisation for a jwmodel
#' 
#' Requires the jwmodel to be initialised. Executes the LP optimisation and
#' rights results into the jwmodel object which is returned.
#' 
#' @param obj jwmodel object to be optimised
#' @return Returns an object of type \code{jwmodel}
#' @export
#' @examples 
#' \dontrun{
#' mymodel <- optimise(mymodel)
#' }
optimise <- function(obj) {
  UseMethod("optimise")
}

#' @export
optimise.default <- function(obj) {
  cat("'optimise' function can only be used on jwmodel objects")
}

#' @importFrom rlang .data
#' @export
optimise.jwmodel <- function(obj) {
  if (is.null(obj$lpmodel)) {
    initialise(obj)
  }
  
  # add datetime stamp to model metadata
  obj$metadata$lastrun$date <- Sys.time()
  
  # solve
  timed <- system.time({
    solve_outcome <- solve(obj$lpmodel)
  })
  
  # record metadata on the optimisation
  obj$metadata$lastrun$duration <- timed
  
  obj$metadata$lastrun$outcome <- solve_outcome
  
  status_codes <- c(
    "0" = "optimal solution found",
    "1" = "the model is suboptimal",
    "2" = "the model is infeasible",
    "3" = "the model is unbounded",
    "4" = "the model is degenerate",
    "5" = "numerical failure encountered",
    "6" = "process aborted",
    "7" = "timeout",
    "9" = "the model was solved by presolve",
    "10" = "the branch and bound routine failed",
    "11" = "the branch and bound was stopped because of a break-at-first or break-at-value",
    "12" = "a feasible branch and bound solution was found",
    "13" = "no feasible branch and bound solution was found"
  )
  
  obj$metadata$lastrun$outcomedesc <-status_codes[[as.character(solve_outcome)]]
  
  # parse results into dataframes for easy onward analysis
  
  allocation_vars <- allocation_vars_template(obj)
  resource_vars <- resource_vars_template(obj)
  n_alloc <- nrow(allocation_vars)
  n_res <- nrow(resource_vars)
  
  allocation_output <- allocation_vars %>%
    dplyr::mutate(Allocated = lpSolveAPI::get.variables(obj$lpmodel)[1:n_alloc]) %>%
    dplyr::inner_join(obj$variable_costs, by = c("Jurisdiction", "Judge")) %>%
    dplyr::inner_join(obj$sitting_days, by = c("Judge" = "Judge Type", "Year")) %>%
    dplyr::left_join(obj$judge_types, by = c("Judge" = "Judge Type")) %>%
    dplyr::mutate(`Total Sitting Days` = .data$Allocated * .data$`Avg Sitting Days`,
                  `Total Fee Cost` = .data$`Total Sitting Days` * .data$`Avg Sitting Day Cost`)
  
  # ensure Year, Jurisdiction and Judge variables are factors
  allocation_output$Year <- factor(allocation_output$Year, 
                                    levels = levels(obj$years$Years))
  allocation_output$Jurisdiction <- factor(allocation_output$Jurisdiction, 
                                    levels = levels(obj$jurisdictions$Jurisdiction))
  allocation_output$Judge <- factor(allocation_output$Judge, 
                                    levels = levels(obj$judge_types$`Judge Type`))
  
  obj$outputs$allocation_output <- allocation_output
  
  resource_output <- resource_vars %>%
    dplyr::mutate(
      status = dplyr::case_when(
        io == "E" ~ "Available",
        io == "I" ~ "Hired (in)",
        io == "O" ~ "Departed (out)"
      ),
      FTE = lpSolveAPI::get.variables(obj$lpmodel)[(n_alloc + 1):(n_alloc + n_res)],
    ) %>%
    dplyr::select(-.data$io)
  
  obj$outputs$resource_output <- resource_output
  
  return(obj)
}
