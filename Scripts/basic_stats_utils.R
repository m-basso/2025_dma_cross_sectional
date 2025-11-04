# Stat utils

# ------------------------------------------------------------------------------------------------------- Function to calculate descriptive statistics including mean (SD), median (IQR), and custom functions

calculate_ds_stat <- function(data, group_var, cols_mean = NULL, cols_med = NULL, custom_stats = NULL) {
  
  # Summarise the data
  result <- data %>%
    group_by({{ group_var }}) %>%
    summarise(
      # Mean (SD) for specified columns
      across(
        all_of(cols_mean),
        ~ paste0(round(mean(.), 1), " (", round(sd(.), 1), ")"),
        .names = "{col}_mean_sd"
      ),
      # Median (IQR) for specified columns
      across(
        all_of(cols_med),
        ~ paste0(round(median(.), 1), " (", round(IQR(.), 1), ")"),
        .names = "{col}_median_iqr"
      ),
      # Custom summary statistics if provided
      if (!is.null(custom_stats)) {
        across(
          all_of(custom_stats$columns),
          custom_stats$functions,
          .names = "{col}_{fn}"
        )
      }
    )
  
  # Convert result to data frame and transpose
  result <- result %>% t() %>% as.data.frame()

  
  return(result)
}



#across with Anonymous Function: The tilde ~ before paste0 is used to define an anonymous function 
#that applies paste0 to the median and IQR calculations. This allows you to use the same operation across multiple columns efficiently
#to avoid long script as in the AVANA NI script
#.names specifies the names of the resulting columns (should anyways do that by default)

  # ------------------------------------------------------------------------------------------------------------------- Function to fix col names
 
fix_column_names <- function(result, col_1, col_2) { 
  # Remove the first row and keep the rest
  result <- result[2:nrow(result), ]
  
  # Set the column names for HEI clusters
  colnames(result) <- c(col_1, col_2)
  # Return the modified result
  return(result)
}
  


# --------------------------------------------------------------------------------------------------------------------  Function to apply Wilcoxon or t test across cols 

 apply_t_w_test_across <- function(data, cols_t = NULL, cols_w = NULL, IV) {
   # Convert IV to a symbol to be used as a column reference
   IV <- ensym(IV)
   
   # Run t-tests across specified columns
   p_values_t <- NULL

   
   if (!is.null(cols_t)) {
     p_values_t <- sapply(cols_t, function(col) {
       t.test(data[[col]] ~ data[[as_label(IV)]], exact = FALSE)$p.value
     })
     names(p_values_t) <- paste0(cols_t, "_t_test_p")
     
   }  
   
   # Run Wilcoxon tests across specified columns
   p_values_w <- NULL
   
   if (!is.null(cols_w)) {
     p_values_w <- sapply(cols_w, function(col) {
       wilcox.test(data[[col]] ~ data[[as_label(IV)]], exact = FALSE)$p.value
     })
     names(p_values_w) <- paste0(cols_w, "_wilcox_test_p")
   }
     
    
   # Combine results as a named vector
   p_values <- c(p_values_t, p_values_w)
  
   
   # Return as a named vector for easy assignment
   return(p_values)
 }

 
 # -------------------------------------------------------------------------------------------------------------------- Function to calculate summary stats for categorical and ordinal variables
 
 calculate_prop_t <- function(data, group_var, cols_names) {
   
   # Create lists to store results
   proportion_r <- list()
   chisq_fisher_Cochran_r <- list()  # Correctly initialize this list
   test_type <- list()  # To store the type of test used
   pairwise_r <- list()
   
   # Loop through each variable
   for (var in cols_names) {
     
     # Create contingency table
     c_table <- table(data[[group_var]], data[[var]])
     
     # Calculate proportions
     p_table <- prop.table(c_table, margin = 1) * 100
     p_table <- round(p_table, 2)
     
     # Check if variables are ordinal
     is_ordinal <- is.ordered(data[[var]])
     
     cat("Variable:", var, "is ordered:", is.ordered(data[[var]]), "\n")
     
     if (is_ordinal) {
       # Use Cochran-Armitage Trend Test
       test_results <- DescTools::CochranArmitageTest(c_table)
       test_type[[var]] <- "Cochran-Armitage"
       
     } else {
       # Check if any cell count is too small
       expected_counts <- outer(rowSums(c_table), colSums(c_table)) / sum(c_table)
       if (any(expected_counts < 5 - .Machine$double.eps^0.5)) {
         # Use Fisher's exact test on c table
         test_results <- fisher.test(c_table)
         test_type[[var]] <- "Fisher"
         
         # Calculate pairwise Fisher test if p-value < 0.05
         if (test_results$p.value < 0.05) {
           pairwise_results <- pairwise_fisher_test(c_table, p.adjust.method = "bonferroni")
           pairwise_r[[var]] <- pairwise_results
         }
         
       } else {
         # Use Chi-squared test on c table
         test_results <- chisq.test(c_table)
         test_type[[var]] <- "Chi-squared"
         
         # Calculate pairwise Chi-squared test if p-value < 0.05
         if (test_results$p.value < 0.05) {
           raw_pairwise_pvals <- matrix(NA, nrow = ncol(c_table), ncol = ncol(c_table),
                                        dimnames = list(colnames(c_table), colnames(c_table)))
           adjusted_pairwise_pvals <- raw_pairwise_pvals
           
           for (i in 1:(ncol(c_table) - 1)) {
             for (j in (i + 1):ncol(c_table)) {
               pairwise_table <- c_table[, c(i, j)]
               pairwise_test <- chisq.test(pairwise_table)
               raw_pairwise_pvals[i, j] <- pairwise_test$p.value
               raw_pairwise_pvals[j, i] <- pairwise_test$p.value
             }
           }
           
           adjusted_pvals <- p.adjust(raw_pairwise_pvals[!is.na(raw_pairwise_pvals)], method = "bonferroni")
           adjusted_pairwise_pvals[!is.na(raw_pairwise_pvals)] <- adjusted_pvals
           
           pairwise_r[[var]] <- list(
             raw_pvals = raw_pairwise_pvals,
             adj_pvals = adjusted_pairwise_pvals
           )
         }
       }
     }
     
     # Store results
     proportion_r[[var]] <- p_table
     chisq_fisher_Cochran_r[[var]] <- list(
       test_results = test_results,
       p_value = test_results$p.value
     )
   }
   
   # Print which tests were used
   cat("Tests used for each variable:\n")
   print(test_type)
   
   # Return results as a list
   return(list(proportions = proportion_r, tests = chisq_fisher_Cochran_r, test_type = test_type, pairwise = pairwise_r))
 }
 

 # --------------------------------------------------------------------------------------------------------------  Function to create table from prop list
 
 
 
 prop.as.data.frame <- function(list, Preferred.Oil_var = NULL, Var.S, Var.E, all_categories) {  # input Var as var "names" # all_categories e.g., = c("high", "low", "medium", "Non-Eater")
   
   # Initialize preferred oil df as NULL
   preferred_oil_df <- NULL
   
   # if preferred oil col is provided
   if (!is.null(Preferred.Oil_var)) {
     
   # create preferred oil df
     preferred_oil_df <- as.data.frame(list[[Preferred.Oil_var]]) %>%
       pivot_wider(
         names_from = c(Var1, Var2),
         values_from = Freq)
     
     # Add a variable name column
     preferred_oil_df$Variable = Preferred.Oil_var
     
     # Reorder columns
     preferred_oil_df <- preferred_oil_df %>%
       relocate(Variable, .before = everything())
     
   }
   
   # define all poss categories for other vars
   all_categories <-  all_categories
   
   # create empty df
   other_var_df <- data.frame()
   
   # define start and end
   start <- which(names(list) == Var.S)
   end <- which(names(list) == Var.E)
   
   # loop through all variables except the first
   for (var in names (list)[start:end])  {
     # Extract the proportion table as df
     prop_df <- as.data.frame(list[[var]])
     # Ensure consistent Var2 categories for each variable
     prop_df <- prop_df %>%
       complete(Var1 = unique(prop_df$Var1), Var2 = all_categories, fill = list(Freq = 0)) # complete fill missing combinations with freq = 0 
     # reshape
     prop_df <- prop_df %>%
       pivot_wider(
         names_from = c(Var1, Var2), 
         values_from = Freq, 
         values_fill = list(Freq = 0))
     # Add a column for the variable name
     prop_df$Variable <- var
     # append to df
     other_var_df <- bind_rows(other_var_df, prop_df)
   }
   
   # Move 'Variable' column to the front
   other_var_df <- other_var_df %>%
     relocate(Variable, .before = everything())
   
   return(list(preferred_oil_prop = preferred_oil_df, other_var_prop = other_var_df))
    
   
 }
   

 # -------------------------------------------------------------------------------------------------------------- # function to calculate cols with zero variance # MFA
 
 remove_zero_variance <- function(df) {
   # Identify zero-variance columns
   zero_var_cols <- apply(df, 2, function(col) var(as.numeric(col), na.rm = TRUE) == 0)
   
   # Print names of columns with zero variance
   if (any(zero_var_cols)) {
     cat("Columns with zero variance:\n")
     print(names(zero_var_cols[zero_var_cols]))
   } else {
     cat("No columns with zero variance.\n")
   }
   
   # Remove zero-variance columns
   df <- df[, !zero_var_cols]
   
   # Return the cleaned dataframe
   return(df)
 }
 
 
 # ---------------------------------------------------------------------------------------------- k-means clustering [column] - retrieve centroids
 # Function to apply k-means on positive values for a single column
 kmeans_column <- function(column, centers) {
   # Identify positive values
   positive_values <- column[column > 0]
   
   # Apply k-means if there are enough positive values
   if (length(positive_values) > centers) {
     # Reshape positive values into a matrix (kmeans requires a matrix)
     positive_values <- matrix(positive_values, ncol = 1)
     
     # Perform k-means clustering
     kmeans_result <- kmeans(positive_values, centers = centers)
     
     # Extract centroids and cluster assignments
     centroids <- as.vector(kmeans_result$centers)  # Convert centroids to vector
     cluster_assignments <- kmeans_result$cluster  # Cluster indices
     
     # Replace positive values in the column with their centroids
     column[column > 0] <- centroids[cluster_assignments]
   }
   
   return(column)  # Return updated column
 }
 
 # ---------------------------------------------------------------------------------------------- k-Medoids clustering [uses median instead of mean] - retrieve medoids and cluster assign
 
 
 kmed_column <- function(column, centers) {
   # Initialize cluster assignments with NA
   cluster_assignments <- rep(NA, length(column))
   
   # Identify positive values
   positive_values <- column[column > 0]
   
   # Apply k-medoids clustering if there are enough positive values
   if (length(positive_values) > centers) {
     # Reshape positive values into a matrix
     positive_values <- matrix(positive_values, ncol = 1)
     
     # Perform k-medoids clustering
     kmed_results <- pam(positive_values, k = centers)
     
     # Extract medoids and cluster assignments
     medoids <- as.vector(kmed_results$medoids)  # Convert medoids to vector
     original_assignments <- kmed_results$cluster  # Original cluster indices
     
     # Reorder clusters based on ascending medoid values
     order_mapping <- rank(medoids, ties.method = "first")  # Map medoids to ascending order
     reordered_assignments <- order_mapping[original_assignments]  # Apply new order #Indexing with square brackets retrieves the ranks corresponding to the original assignments
     
     # Replace positive values in the column with their medoids (optional)
     column[column > 0] <- medoids[original_assignments] # Indexing with square brackets retrieves the medoid corresponding to each cluster assignment
     
     # Update cluster assignments with reordered values
     cluster_assignments[column > 0] <- reordered_assignments
     
     # assign 0 to NA 
     cluster_assignments[is.na(cluster_assignments)] <- 0 
   }
   
   return(list(Column = column, Clusters = cluster_assignments))  # Return both
 }
 
 

 

 
 

 
