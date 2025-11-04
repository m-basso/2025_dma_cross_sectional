


############################################################################################################################################# FACETED CORRELATION NI/FI - EI

cor_f <- function(data, cols_names, EI_col_name) {
  
  # Reshape data to long format for faceting
  data_long <- data %>%
    pivot_longer(cols = all_of(cols_names), names_to = "Nutrient/Food", values_to = "Intake") %>%
    mutate(`Nutrient/Food` = factor(`Nutrient/Food`, levels = cols_names))  # Set factor levels to preserve order
  
  # Create faceted correlation plot
  cor_plots_f <- ggplot(data_long, aes(x = !!sym(EI_col_name), y = Intake)) +
    geom_point() +
    facet_wrap(~ `Nutrient/Food`, scales = "free_y") +
    labs(
      x = "Energy Intake",
      y = "Nutrient/Food Intake",
      title = "Scatterplot of Energy Intake vs. Each Nutrient/Food"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 9),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    stat_cor(
      method = "spearman",
      label.x.npc = "left",
      label.y.npc = "top",
      cor.coef.name = "rho",
      digits = 2,
      show.legend = FALSE
    )
  
  return(cor_plots_f)
}



############################################################################################################################################# FACETED VIOLIN - BOXPLOT - BEESWARM


distr_f <- function(data, cols_names) {
  
  # Reshape data to long format for faceting
  data_long <- data %>%
    pivot_longer(cols = all_of(cols_names), names_to = "Variable", values_to = "value") %>%
    mutate(`Variable` = factor(`Variable`, levels = cols_names))  # Set factor levels to preserve order
  
  # Create faceted distribution plot
  distr_plot_f <- ggplot(data_long, aes(x = "", y = value)) +  # Empty x-axis, only focusing on y-axis
    geom_violin(fill = "lightblue") +
    geom_boxplot(width = 0.1, outlier.shape = NA) +
    geom_beeswarm(alpha = 0.5, size = 2.5, cex = 3) +
    facet_wrap(~ `Variable`, scales = "free_y") +  # Adjust faceting variable
    labs(
      y = "Variable",
      title = "Violin Plot of Each Variable's Distribution"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 9),
      axis.text.x = element_blank(),        # Hide x-axis labels for cleaner look
      axis.ticks.x = element_blank()
    )
  
  return(distr_plot_f)
}

# To ensure nothing is printed or displayed when sourcing, make the script "invisible" at the end
return(invisible(NULL))




############################################################################################################################################# FACETED VIOLIN - BOXPLOT for gm features 

distr_f_nobee <- function(data, cols_names) {
  
  # Reshape data to long format for faceting
  data_long <- data %>%
    pivot_longer(cols = all_of(cols_names), names_to = "Variable", values_to = "value") %>%
    mutate(`Variable` = factor(`Variable`, levels = cols_names))  # Set factor levels to preserve order
  
  # Create faceted distribution plot
  distr_plot_f <- ggplot(data_long, aes(x = "", y = value)) +  # Empty x-axis, only focusing on y-axis
    geom_violin(fill = "lightblue") +
    geom_boxplot(width = 0.1, outlier.shape = NA) +
    facet_wrap(~ `Variable`, scales = "free_y") +  # Adjust faceting variable
    labs(
      x = NULL,
      y = "Std Clr-transformed Abudance",
      title = "Violin - Box Plots of Each Feature's Distribution"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 9),
      axis.text.x = element_blank(),        # Hide x-axis labels for cleaner look
      axis.ticks.x = element_blank()
    )
  
  return(distr_plot_f)
}


############################################################################################################################################# FACETED COR RESIDUALS - FITTED VALUES


res_fit_cor_f <- function(res_df, fitted_df) {
  
  # Ensure inputs are data frames
  res_df <- as.data.frame(res_df)
  fitted_df <- as.data.frame(fitted_df)
  
  # Add a unique identifier for each row
  res_df$Subject_ID <- seq_len(nrow(res_df))
  fitted_df$Subject_ID <- seq_len(nrow(fitted_df))
  
  # Combine residuals and fitted values by Subject_ID
  combined_data <- merge(res_df, fitted_df, by = "Subject_ID", suffixes = c("_Residual", "_Fitted"))
  
  # Reshape data into long format
  data_long <- combined_data %>%
    pivot_longer(
      cols = -Subject_ID, 
      names_to = c("Nutrient", "Type"), 
      names_pattern = "(.+)_(Residual|Fitted)" #specify how to split the col name when reshaping, () create the capturing groups
    )                                          # .+ --> any sequence before _ which act as separator
  
  # Reshape from long to wide format
  data_long <- data_long %>%
    pivot_wider(names_from = Type, values_from = value)
  
  # Ensure the 'Fitted' and 'Residual' columns are numeric
  data_long <- data_long %>%
    mutate(
      Fitted = as.numeric(Fitted),
      Residual = as.numeric(Residual)
    )
  
  # Create the plot
  res_plot_f <- ggplot(data_long, aes(x = Fitted, y = Residual)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Horizontal line at y=0
    labs(title = "Residuals vs Fitted values",
         x = "Fitted values",
         y = "Residuals") +
    facet_wrap(~Nutrient, scales = "free") +  # Facet by Nutrient
    theme_minimal()
  
  return(res_plot_f)
}


############################################################################################################################################# FACETED QQ PLOTs


qqplot_f <- function(data) {
  
  # Reshape data into a long format to create QQ plots for each column
  data_long <- data %>%
    pivot_longer(cols = where(is.numeric), names_to = "Variable", values_to = "Value")
  
  # Create QQ plot
  qq_plot <- ggplot(data_long, aes(sample = Value)) +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(~ Variable, scales = "free") +
    labs(title = "QQ Plot for Each Variable", x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  
  return(qq_plot)
  }


############################################################################################################################################# FACETED stacked bar charts

bar_stacked_f <- function(data, variable_col, columns, levels, debug = FALSE) { 
  # levels being c("level1", "level2", etc.)
  
  # Check for required columns
  if (!variable_col %in% names(data)) stop("The data must contain the specified 'variable_col'.")
  
  # Reshape data to long format
  data_long <- data %>%
    select(all_of(columns)) %>%
    pivot_longer(
      cols = -variable_col, # Do not reshape this column
      names_to = c("consumption category", "HEI_group"),
      names_sep = " \\(",
      names_transform = list(HEI_group = ~gsub("\\)", "", .))
    )
  
  # Order levels of factor
  data_long$`consumption category` <- factor(data_long$`consumption category`, levels = levels)
  
  # Debugging outputs
  if (debug) {
    print("Factor levels for consumption category:")
    print(levels(data_long$`consumption category`))
    print("Preview of reshaped data:")
    print(head(data_long))
  }
  
  color_palette <- setNames(scales::hue_pal(l = 80, c = 40)(length(levels)), levels)
  
  # Create stacked bar plot
  stacked_bar_p <- ggplot(data_long, aes(x = HEI_group, y = value, fill = `consumption category`)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = color_palette) +
    labs(title = "Stacked Bar Charts",
         x = "HEI Group",
         y = "Proportion (%)") +
    facet_wrap(vars(.data[[variable_col]])) + 
    theme_minimal() +
    theme(strip.text = element_text(size = 11, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size = 11),
          axis.title.x = element_text(size = 11))
  
  return(stacked_bar_p)
}




############################################################################################################################################# MFA Dim loadings plot cat + con


visualize_loadings_comb <- function(data_con, data_cat, col_con, col_cat, title) {
  
  # Check if the specified columns exist
  stopifnot(
    col_con %in% colnames(data_con),
    col_cat %in% colnames(data_cat)
  )
  
  # Create the combined data frame
  combined_data <- data.frame(
    Variable = c(rownames(data_con), rownames(data_cat)), 
    Loading = c(data_con[[col_con]], data_cat[[col_cat]])
  )
  
  # Add metadata
  combined_data$Sign <- ifelse(combined_data$Loading > 0, "positive", "negative")
  combined_data$Type <- ifelse(combined_data$Variable %in% rownames(data_con), "Continuous", "Categorical")
  
  # Generate the plot
  plot <- ggplot(combined_data, aes(x = reorder(Variable, -Loading), y = Loading, fill = Sign)) +
    geom_bar(stat = "identity", width = 0.8) +
    coord_flip() +
    scale_fill_manual(values = c("positive" = "blue", "negative" = "red")) +
    facet_grid(Type ~ ., scales = "free_y") +
    labs(title = title, x = "Variable", y = "Loading") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 10, hjust = 1),
      legend.position = "none"
    )
  
  # Return both the data and the plot as a list
  return(list(data = combined_data, plot = plot))
}


############################################################################################################################################# MFA Dim loadings plot

visualize_loadings_con <- function(data, col, title, threshold = 0.1) {
  
  # check if col exist
  stopifnot(
    col %in% colnames(data)
  )
  
  # rename col
  data <-  data.frame(
    Variable = rownames(data),
    Loading = data[[col]]
  )
  
  data <- subset(data, abs(Loading) > threshold)
  
  # add sign data
  data$Sign <- ifelse(data$Loading > 0, "positive", "negative")
  
  # generate plot
  plot <- ggplot(data, aes(x = reorder(Variable, -Loading), y = Loading, fill = Sign)) +
    geom_bar(stat = "identity", width = 0.8) +
    coord_flip() +
    scale_fill_manual(values = c("positive" = "blue", "negative" = "red")) +
    labs(title = title, x = "Variable", y = "Loading") +
    theme_minimal() +
    theme(
      # Increase title, axis, and tick label text size
      plot.title   = element_text(size = 16, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x  = element_text(size = 12),
      axis.text.y  = element_text(size = 12),
      legend.position = "none"
  )
  
  # return
  return(plot)
  
}

# ------------------------------------------------------------------------------------- kable table

kable_table <- function(df, align) {   # align = "l" / "c" / "r"
  
  kable_t <- kable(df, format = "html", align = align, table.attr = "class='table table-bordered'") %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
  
  return(kable_t)
}








