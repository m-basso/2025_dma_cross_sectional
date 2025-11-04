# EDA utils 

# ---------------------------------------------------------------------------------------------------- define a function for correlation matrix

cocor <- function(data, cor_type = 'pearson') {
  
  # perform xx corr matrix
  cocor_results <- rcorr(as.matrix(data), type = cor_type) 
  # extract r values
  cor_matrix <- cocor_results$r
  # extract p values
  cor_matrix.p <- cocor_results$P
  
  # Check if dimensions match between correlation matrix and p-value matrix
  if (!all(dim(cor_matrix) == dim(cor_matrix.p))) {
    stop("Dimension mismatch between correlation matrix and p-values matrix")
  }
  
  diag(cor_matrix.p) <- 1 
  
  cocor_plot <- corrplot(cor_matrix, method = "color", type = "full", tl.col = "black", tl.srt = 45, 
                 addCoef.col = "black", p.mat = cor_matrix.p, sig.level = 0.05, insig = "blank", 
                 order = "hclust")
  
  return(list(cor_matrix =cor_matrix, cor_p = cor_matrix.p, cocor_plot = cocor_plot ))
}




# ---------------------------------------------------------------------------------------------------- define a function to logp1 skewed vars

log1p_skewed_transform <- function(data, start_col, end_col) {
  
  # Initialize the vector to store transformed column names
  transformed_columns <- c()
  
  for (i in start_col:end_col) {
      
    # Check skewness
    if (abs(skewness(data[, i])) > 1) {
      # Record column name
      transformed_columns <- c(transformed_columns, colnames(data)[i])
      # Apply log1p transformation
      data[, i] <- log1p(data[, i])
    } else {
      # This line is effectively a no-op, but kept to mirror your original code:
      data[, i] <- data[, i]
    }
  }
  
  # Print which columns were transformed
  if (length(transformed_columns) > 0) {
    cat("Log1p transformation applied to columns:", 
        paste(transformed_columns, collapse = ", "), "\n")
  } else {
    cat("No columns required log1p transformation.\n")
  }
  
  # Return the updated data
  return(data)
}





