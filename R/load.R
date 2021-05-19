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
#' @return Returns an object of type \code{jwmodel}
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
  
  sheets_in_file <- readxl::excel_sheets(filepath)
  
  # load model metadata first, if present (so we know whether this is Mags data)
  if (c("Model Info") %in% sheets_in_file) {
    # load any and all meta data contained in the "Model Info" tab
    # assumes data is present in two columns only: label + value
    df <- readxl::read_excel(filepath, sheet = "Model Info", 
                             col_names = c("field", "value"))
    # force fieldnames to be lowercase to avoid case-sensitivity issues later
    df <- df %>%
      dplyr::mutate(field = tolower(field))
    
    sapply(1:length(df$field), function(n) {
      obj$metadata[df$field[n]] <<- df$value[n]
    })
  }
  
  # will return "magistrates" if this is a Mags model, anything else including
  # NULL will be assumed to mean this is a regular Courts model
  model_type <- if (!is.null(obj$metadata$type)) {
    tolower(obj$metadata$type)
  } else {
    "courts"
  }
  
  # the must-have list of worksheet names
  if (model_type == "magistrates") {
    # if it's the Magistrate version of the model
    wslist <- c("Judge Types", "Jurisdictions", "Years", "Regions",  
                "Number of Judges", "Expected Departures", "Sitting Day Capacity", 
                "Baseline Demand", "Judge Progression", "Fixed Costs", "Variable Costs",
                "Recruitment Limits", "PerSittingDay", "Allocation Limits")
  } else {
    # if it's the original Courts model version
    wslist <- c("Judge Types", "Jurisdictions", "Years", "Number of Judges", 
                "Expected Departures", "Sitting Day Capacity", "Baseline Demand",
                "Judge Progression", "Fixed Costs", "Variable Costs",
                "Recruitment Limits", "Allocation Limits")
  }
  
  # check that all required sheets are present
  if (length(wslist) > length(wslist %in% readxl::excel_sheets(filepath))) {
    warning("Incomplete file: data not loaded")
    return(obj)
  }
  
  # name it after load datetime if no name is otherwise specified
  if (is.null(obj$metadata$name)) {
    obj$metadata$name <- as.character(Sys.time())
  }
  
  # load all required data
  
  # NB partially sensitive to sheet order *in wslist*: "Judge Types" must preceed  
  # other sheets which refer to Judge Types, ditto for "Jurisdictions" and "Years".
  judge_levels <- c()
  jurisdiction_levels <- c()
  year_levels <- c()
  region_levels <- c()
  
  sapply(wslist, function(sheet_name) {
    
    if (sheet_name == "Judge Types") {
      # load judge types, converting into a factor with order as loaded
      df <- readxl::read_excel(filepath, sheet = sheet_name)
      judge_levels <<- c(unique(df$`Judge Type`), "U")
      df$`Judge Type` <- factor(df$`Judge Type`, levels = judge_levels)
      obj$judge_types <<- df
      
    } else if (sheet_name == "Jurisdictions") {
      # load jurisdictions, converting into a factor with order as loaded
      df <- readxl::read_excel(filepath, sheet = sheet_name)
      jurisdiction_levels <<- unique(df$Jurisdiction)
      df$Jurisdiction <- factor(df$Jurisdiction, levels = jurisdiction_levels)
      obj$jurisdictions <<- df
      
    } else if (sheet_name == "Years") {
      # load years
      df <- readxl::read_excel(filepath, sheet = sheet_name)
      year_levels <<- unique(df$Years)
      df$Years <- factor(df$Years, levels = year_levels)
      obj$years <<- df
      
    } else if (sheet_name == "Regions") {
      # load Regions data (only applicable for Magistrates)
      df <- readxl::read_excel(filepath, sheet = sheet_name)
      region_levels <<- unique(df$Region)
      df$Region <- factor(df$Region, levels = region_levels)
      obj$regions <<- df
      
    } else if (sheet_name == "Number of Judges") {
      # load number of judges initialy employed and available
      df <- readxl::read_excel(filepath, sheet = sheet_name)
      df$`Judge Type` <- factor(df$`Judge Type`, levels = judge_levels)
      if (model_type == "magistrates") {
        df$Region <- factor(df$Region, levels = region_levels)
      }
      obj$n_judges <<- df
      
    } else if (sheet_name == "Expected Departures") {
      # load expected departures data
      df <- readxl::read_excel(filepath, sheet = sheet_name)
      df$`Judge Type` <- factor(df$`Judge Type`, levels = judge_levels)
      df$Year <- factor(df$Year, levels = year_levels)
      if (model_type == "magistrates") {
        df$Region <- factor(df$Region, levels = region_levels)
      }
      obj$judge_departures <<- df
      
    } else if (sheet_name == "Sitting Day Capacity") {
      # load sitting day capacity data
      df <- readxl::read_excel(filepath, sheet = sheet_name)
      df$`Judge Type` <- factor(df$`Judge Type`, levels = judge_levels)
      df$Year <- factor(df$Year, levels = year_levels)
      if (model_type == "magistrates") {
        df$Region <- factor(df$Region, levels = region_levels)
      }
      obj$sitting_days <<- df
      
    } else if (sheet_name == "Baseline Demand") {
      # load baseline demand data
      df <- readxl::read_excel(filepath, sheet = sheet_name)
      df$Jurisdiction <- factor(df$Jurisdiction, levels = jurisdiction_levels)
      df$Year <- factor(df$Year, levels = year_levels)
      if (model_type == "magistrates") {
        df$Region <- factor(df$Region, levels = region_levels)
      }
      obj$demand <<- df
      
    } else if (sheet_name == "Judge Progression") {
      # load judge progression
      df <- readxl::read_excel(filepath, sheet = sheet_name)
      df$`Recruited Into` <- factor(df$`Recruited Into`, levels = judge_levels)
      df$`Recruited From` <- factor(df$`Recruited From`, levels = judge_levels)
      if (model_type == "magistrates") {
        df$Region <- factor(df$Region, levels = region_levels)
      }
      if (nrow(df)==0) {
        df$Proportion <- numeric()
      }
      obj$judge_progression <<- df
      
    } else if (sheet_name == "Fixed Costs") {
      # load fixed cost data
      df <- readxl::read_excel(filepath, sheet = sheet_name)
      df$`Judge Type` <- factor(df$`Judge Type`, levels = judge_levels)
      if (model_type == "magistrates") {
        df$Region <- factor(df$Region, levels = region_levels)
      }
      obj$fixed_costs <<- df
      
    } else if (sheet_name == "Variable Costs") {
      # load variable cost data
      df <- readxl::read_excel(filepath, sheet = sheet_name)
      df$Judge <- factor(df$Judge, levels = judge_levels)
      df$Jurisdiction <- factor(df$Jurisdiction, levels = jurisdiction_levels)
      if (model_type == "magistrates") {
        df$Region <- factor(df$Region, levels = region_levels)
      }
      obj$variable_costs <<- df
      
    } else if (sheet_name == "Recruitment Limits") {
      # load recruitment limits data
      df <- readxl::read_excel(filepath, sheet = sheet_name)
      df$`Judge Type` <- factor(df$`Judge Type`, levels = judge_levels)
      df$Year <- factor(df$Year, levels = year_levels)
      if (model_type == "magistrates") {
        df$Region <- factor(df$Region, levels = region_levels)
      }
      obj$recruit_limits <<- df
      
    } else if (sheet_name == "Allocation Limits") {
      # load allocation limits data
      df <- readxl::read_excel(filepath, sheet = sheet_name)
      df$Judge <- factor(df$Judge, levels = judge_levels)
      df$Jurisdiction <- factor(df$Jurisdiction, levels = jurisdiction_levels)
      if (model_type == "magistrates") {
        df$Region <- factor(df$Region, levels = region_levels)
      }
      obj$alloc_limits <<- df
      
    } else if (sheet_name == "PerSittingDay") {
      # load PerSittingDay data (Magistrates only)
      df <- readxl::read_excel(filepath, sheet = sheet_name)
      df$`Judge Type` <- factor(df$`Judge Type`, levels = judge_levels)
      df$Jurisdiction <- factor(df$Jurisdiction, levels = jurisdiction_levels)
      df$Year <- factor(df$Year, levels = year_levels)
      obj$per_sitting_day <<- df
    }
    
  })
  
  # load remaining optional sheets, if present
  
  if (c("Penalty Costs") %in% sheets_in_file) {
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
  
  if (c("Override Hiring") %in% sheets_in_file) {
    # load user-specified numbers of judges hired which will be used to 'force'
    # the model to hire exactly the number of judges given
    # NB this can easily create a infeasible problem in conjunction with other
    # constraints if not careful
    df <- readxl::read_excel(filepath, sheet = "Override Hiring")
    df$Judge <- factor(df$Judge, levels = judge_levels)
    df$Year <- factor(df$Year, levels = year_levels)
    if (model_type == "magistrates") {
      df$Region <- factor(df$Region, levels = region_levels)
    }
    obj$override_hiring <- df
  }
  
  # if no Regions defined, treat as if there is one single region "National"
  if (is.null(obj$regions)) {
    
    # create "national" region
    national <- as.factor("National")
    
    obj$regions <- dplyr::tibble(
      Region = "National",
      Description = "England & Wales"
    )
    obj$regions$Region <- national
    
    # Prepend "National" Region column to all required input dataframes
    #
    # "prepend" is a custom function; could use a tibble function but this saves
    # adding another package dependency and is more certain to work with older
    # versions of tidyverse
    
    # Number of judges
    obj$n_judges <- prepend(obj$n_judges, national, "Region")
    
    # Expected Departures
    obj$judge_departures <- prepend(obj$judge_departures, national, "Region")
    
    # Sitting Day Capacity
    obj$sitting_days <- prepend(obj$sitting_days, national, "Region")
    
    # Baseline Demand
    obj$demand <- prepend(obj$demand, national, "Region")
    
    # Judge Progression (non-Mags only)
    obj$judge_progression <- prepend(obj$judge_progression, national, "Region")
    
    # Fixed Costs
    obj$fixed_costs <- prepend(obj$fixed_costs, national, "Region")
    
    # Variable Costs
    obj$variable_costs <- prepend(obj$variable_costs, national, "Region")
    
    # Recruitment Limits
    obj$recruit_limits <- prepend(obj$recruit_limits, national, "Region")
    
    # Allocation Limits
    obj$alloc_limits <- prepend(obj$alloc_limits, national, "Region")
    
  }
  
  if (model_type != "magistrates") {
    # create empty PerSittingDay (prevents NULL errors downstream)
    obj$per_sitting_day <- dplyr::tibble(
      "Judge Type" = factor(character(), levels = judge_levels),
      "Jurisdiction" = factor(character(), levels = jurisdiction_levels),
      "Year" = factor(character(), levels = year_levels),
      "Required Per Sitting Day" = numeric()
    )
    
    # add "National" region to Override Hiring
    if (!is.null(obj$override_hiring)) {
      if (is.null(obj$override_hiring$Region)) {
        obj$override_hiring <- prepend(obj$override_hiring, national, "Region")
      }
    }
  }
  
  return(obj)
}