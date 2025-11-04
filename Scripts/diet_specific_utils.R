
 # Function specific to nutrient dataset

#############################################################################################################################################################    Standardization / 1000 Kcal adjustment


# Function to standardize each specified column by total energy intake per 1000 kcal

  standardize <- function(data, cols_names, EI_col_name) {
    data_standardized <- data %>%
    mutate(across(all_of(cols_names), ~ . / (!!sym(EI_col_name) / 1000)))
  
  return(data_standardized)
    
 }


############################################################################################################################################################      Categorize eating consumption 

# define non-eaters (Ne) and categories of consumption among eaters

  categorize_consumption <- function(data, cols_names, num_categories, labels_categories) {  #labels categories must be c("", "", ...)
    
    # Ensure the number of labels matches num_categories to avoid errors
    if (length(labels_categories) != num_categories) {
      stop("The length of labels_categories must match num_categories.")
    }
  
    data.cat <- data %>%
    mutate(across(all_of(cols_names), 
                ~ case_when(              #case-when allows for more flexible conditions without trying to simplify factor levels 
                  . == 0 ~ "Non-Eater",
                  TRUE ~ factor(ntile(., num_categories), labels = labels_categories) #T means --> ALL other conditions diff from 0
                ), 
                .names = "{.col}_cat")) %>%
    dplyr::select(-all_of(cols_names))      # Remove original columns if intended
  
  return(data.cat)
  
  }


  #############################################################################################################################################################     Residual lm adjustment
  
  # General function to apply the residual model on nutrient data
  adjust_nutrients_lm <- function(data, energy_col, nutrient_cols) {
    
    # Step 1: Apply linear models (residuals) on each nutrient with energy intake as a predictor
    nutrient_lm <- lapply(data[, nutrient_cols], function(x) lm(x ~ data[[energy_col]], na.action = na.exclude))
    
    # Step 2: Extract residuals for each nutrient
    nutrient_residuals <- sapply(nutrient_lm, function(model) model[["residuals"]]) %>% as.data.frame()
    
    # Step 3: Extract fitted values for performance and fitting checks
    fitted_values <- sapply(nutrient_lm, function(model) fitted(model)) %>% as.data.frame()
    
    # Step 4: Calculate mean for each nutrient and adjust residuals
    nutrient_means <- colMeans(data[, nutrient_cols], na.rm = TRUE)
    nutrient_adj <- sweep(nutrient_residuals, 2, nutrient_means, FUN = "+")
    
    # Step 5: Concatenate adjusted nutrient data with original non-nutrient columns
    data_adj <- cbind(data[, !(colnames(data) %in% nutrient_cols)], nutrient_adj)
    
    # Return results as a list
    return(list(data_adjusted_lm = data_adj, residuals_lm = nutrient_residuals, fitted_values_lm = fitted_values))
  }
  
 
  
  
  
  

  