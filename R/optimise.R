
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
  # TODO replace this check
  # if (is.null(obj$lpmodel)) {
  #   initialise(obj)
  # }
  
  # create lpSolveAPI object from contraints previously defined in 'initialise'
  
  n_vars <- length(obj$constraints$obj$coefficients)
  
  lp.wmodel <- lpSolveAPI::make.lp(0, n_vars)
  
  lp_rownames <- c()
  
  # construct the actual lp model: objective function + constraints
  # (NB uses a vectorised method of looping through list of constraints)
  sapply(
    seq_along(obj$constraints),
    function(x) {
      name <- names(obj$constraints)[[x]]
      if (name == "obj") {
        # create the objective function
        coeffs <- obj$constraints[[x]]$coefficients
        lpSolveAPI::set.objfn(lp.wmodel, obj = coeffs)
      } else {
        # create constraints
        constraint_names <- sapply(
          obj$constraints[[x]], function(y) {
            lpSolveAPI::add.constraint(
              lp.wmodel, 
              xt = y$coefficients,
              type = y$type, 
              rhs = y$rhs
            )
          
            # name constraint (if NULL, default will be used)
            y$name
          }
        )
        
        lp_rownames <<- c(lp_rownames, constraint_names)
      }
    }
  )
  
  # apply row (constraint) names
  lp_dimnames <- dimnames(lp.wmodel)
  lp_dimnames[[1]] <- unlist(lp_rownames)
  dimnames(lp.wmodel) <- lp_dimnames
  
  
  # add lower bounds
  sapply(
    obj$bounds$lower,
    function(x) {
      lpSolveAPI::set.bounds(lp.wmodel, lower = x$coefficients, columns = x$indices)
    }
  )
  
  # add upper bounds
  sapply(
    obj$bounds$upper,
    function(x) {
      lpSolveAPI::set.bounds(lp.wmodel, upper = x$coefficients, columns = x$indices)
    }
  )
  
  # add slack constraints for the following constraint types
  # EQ-008 Constrain the maximum number of Judges recruited in one year
  # EQ-009 Set limits on the proportion of demand allocated to different types of judge
  add_slack_constraints_for <- c("EQ008", "EQ009")
  slack_coefficient <- c(-1, 1)
  slack_constraint_cost <- 1000000
  
  # for each constraint type for which slack should be added
  for (c in 1:length(add_slack_constraints_for)) {
    indices <- startsWith(lp_dimnames[[1]], add_slack_constraints_for[c]) %>% which()
    coeffs <- rep(slack_coefficient[c], length(indices))
    
    # add objective function coefficient
    indices <- c(0, indices)
    coeffs <- c(slack_constraint_cost, coeffs)
    
    # add column for slack
    lpSolveAPI::add.column(lp.wmodel, x = coeffs, indices = indices)
  }
  
  
  # add datetime stamp to model metadata
  obj$metadata$lastrun$date <- Sys.time()
  
  # solve
  timed <- system.time({
    solve_outcome <- solve(lp.wmodel)
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
    dplyr::mutate(Allocated = lpSolveAPI::get.variables(lp.wmodel)[1:n_alloc]) %>%
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
      FTE = lpSolveAPI::get.variables(lp.wmodel)[(n_alloc + 1):(n_alloc + n_res)],
    ) %>%
    dplyr::select(-.data$io)
  
  # ensure Year and Judge variables are factors
  resource_output$Year <- factor(resource_output$Year, 
                                   levels = levels(obj$years$Years))
  resource_output$Judge <- factor(resource_output$Judge, 
                                    levels = levels(obj$judge_types$`Judge Type`))
  
  obj$outputs$resource_output <- resource_output
  
  return(obj)
}
