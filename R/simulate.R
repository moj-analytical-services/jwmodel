#' Runs simulation on an optimised jwmodel
#' 
#' An optimised jwmodel contains a recommended hiring profile as output. Running 
#' a simulation means taking this hiring profile (and hence future total judicial 
#' staff WTE) as fixed, and simulating variations in demand. In each alternate 
#' demand scenario, allocation of judges to jursidictions is still optimised, but
#' no alternations to overall headcount are made. 
#' 
#' The simulation creates a user-defined number of alternative demand scenarios 
#' centered around the original expected demand, but varying to account for:
#' random variation; uncertainty in underlying demand growth; random chances of 
#' a step-change in demand (e.g. as a result of legislative change).
#' 
#' @param obj jwmodel object to be subject to simulation
#' @param iterations (optional) integer specifying the total number of demand 
#' scenarios to be simulated; defaults to 100
#' @param seed_num (optional) integer to be used as the seed for the random 
#' number generator which underpins generation of demand scenarios. Specify if 
#' you want to re-run a simulation with the same range of demand scenarios. 
#' 
#' @return Returns an object of type \code{jwmodel}
#' @export
#' @examples 
#' \dontrun{
#' mymodel <- simulate(mymodel, 10000)
#' }
simulate <- function(obj, iterations, seed_num) {
  UseMethod("simulate")
}

#' @export
simulate.default <- function(obj, iterations, seed_num) {
  cat("'simulate' function can only be used on jwmodel objects")
}

#' @export
simulate.jwmodel <- function(obj, iterations = 100, seed_num = NULL) {
  
  # set & record random generator's seed number (for reproducability)
  if (is.null(seed_num) | !is.numeric(seed_num)) {
    # if no seed given, create one and record it
    seed <- as.numeric(Sys.time())
    obj$metadata$seed <- seed
  } else {
    seed <- as.integer(seed_num)
  }
  
  allocations <- list()
  solver_return_values <- c()
  
  # generate demand scenarios to be simulated
  base_demand_est <- obj$demand$`Sitting Days`
  scenarios <- list()
  
  for (i in 1:iterations) {
    # TODO modify code to simulate here to include four summed components:
    # (1) original baseline demand prediction +
    # (2) variation related to differences in average growth rate +
    # (3) variation related to random step change chances +
    # (4) variation related to random noise
    
    noise <- stats::runif(length(base_demand_est), 10000, 25000)
    
    s <- base_demand_est + noise
    
    # prevent demand value from ever being negative
    s[s<0] <- 0
    
    scenarios <- append(scenarios, list(s))
  }
  
  # fix available resources variables at the levels they were originally optimised to
  n_alloc_vars <- nrow(obj$years) * nrow(obj$jurisdictions) * 
    (nrow(obj$judge_types) + 1)
  n_resource_vars <- nrow(obj$years) * nrow(obj$judge_types) * 3
  
  fixed_resources <- obj$outputs$resource_output
  fixed_resources <- fixed_resources$FTE[seq(1, n_resource_vars, 3)]
  
  indices <- (n_alloc_vars + 1):(n_alloc_vars + n_resource_vars)
  indices <- indices[seq(1, n_resource_vars, 3)]
  
  lpSolveAPI::set.bounds(obj$lpmodel, lower = fixed_resources, upper = fixed_resources,
                         columns = indices)
  
  
  # re-run optmisation, optimising only for allocation based on fixed resources,
  # once for each scenario generated
  for (i in 1:iterations) {
    
    # overwrite demand constraints with scenario's demand values
    lpSolveAPI::set.rhs(obj$lpmodel, scenarios[[i]], obj$constraints$demand)
    
    # (re)optimise model
    obj <- optimise(obj)
    solver_return_values <- c(solver_return_values, obj$metadata$lastrun$outcome)
    
    # save simulation output (allocation variable values)
    allocations <- append(allocations, list(obj$outputs$allocation_output$Allocated))
    
  }
  
  # TODO save outputs for posterity
  
  return(obj)
}
