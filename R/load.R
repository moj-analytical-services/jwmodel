#' Loads data from a file into a jwmodel object
#' 
#' Loads data from the specified Excel file into a jwmodel object, priming it
#' for use. The Excel file must contain the expected worksheets with data in the
#' expected format.
#' 
#' @param obj a jwmodel object
#' @param filepath a string giving the full filepath name of the Excel file
#'        containing the parameter information to be loaded
#' 
#' @return Returns an object of type \code{lpmodel}
#' @export
#' @examples 
#' \dontrun{
#' mymodel <- load_from_file(mymodel, "modelparams.xlsx")
#' }
load_from_file <- function(obj, filepath) {
  UseMethod("load_from_file")
}

#' @export
load_from_file.default <- function(obj, filepath) {
  cat("'load_from_file' function can only be used on jwmodel objects")
}

#' @importFrom rlang .data
#' @export
load_from_file.jwmodel <- function(obj, filepath) {
  
  readxl::excel_sheets(filepath)
  
  wslist <- c("Judge Types", "Jurisdictions", "Years", "Number of Judges", 
              "Expected Departures", "Sitting Day Capacity", "Baseline Demand",
              "Judge Progression", "Fixed Costs", "Variable Costs")
  
  if (length(wslist) > length(wslist %in% readxl::excel_sheets(filepath))) {
    warning("Incomplete file: data not loaded")
    return(obj)
  }
  
  # load judge types, converting into a factor with order as loaded
  df <- readxl::read_excel(filepath, sheet = wslist[1])
  judge_levels <- c(unique(df$`Judge Type`), "U")
  df$`Judge Type` <- factor(df$`Judge Type`, levels = judge_levels)
  obj$judge_types <- df
  
  # load jurisdictions, converting into a factor with order as loaded
  df <- readxl::read_excel(filepath, sheet = wslist[2])
  jurisdiction_levels <- unique(df$Jurisdiction)
  df$Jurisdiction <- factor(df$Jurisdiction, levels = jurisdiction_levels)
  obj$jurisdictions <- df
  
  # load years
  df <- readxl::read_excel(filepath, sheet = wslist[3])
  year_levels <- unique(df$Years)
  df$Years <- factor(df$Years, levels = year_levels)
  obj$years <- df
  
  # load number of judges initialy employed and available
  df <- readxl::read_excel(filepath, sheet = wslist[4])
  df$`Judge Type` <- factor(df$`Judge Type`, levels = judge_levels)
  obj$n_judges <- df
  
  # load expected departures data
  df <- readxl::read_excel(filepath, sheet = wslist[5])
  df$`Judge Type` <- factor(df$`Judge Type`, levels = judge_levels)
  df$Year <- factor(df$Year, levels = year_levels)
  obj$judge_departures <- df
  
  # load sitting day capacity data
  df <- readxl::read_excel(filepath, sheet = wslist[6])
  df$`Judge Type` <- factor(df$`Judge Type`, levels = judge_levels)
  df$Year <- factor(df$Year, levels = year_levels)
  obj$sitting_days <- df
  
  # load baseline demand data
  df <- readxl::read_excel(filepath, sheet = wslist[7])
  df$Jurisdiction <- factor(df$Jurisdiction, levels = jurisdiction_levels)
  df$Year <- factor(df$Year, levels = year_levels)
  obj$demand <- df
  
  # load judge progression
  df <- readxl::read_excel(filepath, sheet = wslist[8])
  df$`Recruited Into` <- factor(df$`Recruited Into`, levels = judge_levels)
  df$`Recruited From` <- factor(df$`Recruited From`, levels = judge_levels)
  obj$judge_progression <- df
  
  # load fixed cost data
  df <- readxl::read_excel(filepath, sheet = wslist[9])
  df$`Judge Type` <- factor(df$`Judge Type`, levels = judge_levels)
  obj$fixed_costs <- df
  
  # load variable cost data
  df <- readxl::read_excel(filepath, sheet = wslist[10])
  df$Judge <- factor(df$Judge, levels = judge_levels)
  df$Jurisdiction <- factor(df$Jurisdiction, levels = jurisdiction_levels)
  obj$variable_costs <- df
  
  if (c("Penalty Costs") %in% readxl::excel_sheets(filepath)) {
    # load penalty costs from file if they are defined there
    df <- readxl::read_excel(filepath, sheet = "Penalty Costs")
    df$Jurisdiction <- factor(df$Jurisdiction, levels = jurisdiction_levels)
    names(df)[2] <- "Penalty Cost"
    df$Judge <- factor("U", levels = judge_levels)
    obj$penalty_costs <- df
    
  } else {
    # derive sensible penalty costs = double max fee for given jurisdiction
    df <- obj$variable_costs %>%
      dplyr::group_by(.data$Jurisdiction) %>%
      dplyr::summarise(`Penalty Cost` = 2 * max(.data$`Avg Sitting Day Cost`))
    df$Judge <- factor("U", levels = judge_levels)
    obj$penalty_costs <- df
  }
  
  if (c("Model Info") %in% readxl::excel_sheets(filepath)) {
    df <- readxl::read_excel(filepath, sheet = "Model Info", 
                             col_names = c("field", "value"))
    # TODO refactor code to be more flexible and add other info into metadata
    # should the user specify any
    obj$metadata$name <- df[1,2]
    obj$metadata$description <- df[2,2]
  } else {
    # name it after load datetime if no name is otherwise specified
    obj$metadata$name <- as.character(Sys.time())
  }
  
  return(obj)
}