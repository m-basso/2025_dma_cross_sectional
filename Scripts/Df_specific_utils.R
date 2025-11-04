#dataset-specific cleaning 

#################################################################################################################################################### Assessment Masterfile

preprocess_data <- function(data, sel_time_points = NULL) {
  
  # Exclude dropouts
  data <- data %>% filter(!Excluded == T)
  
  # Filter by specified time points, if provided
  if (!is.null(sel_time_points)) {
    data <- data %>% filter(time_point %in% sel_time_points)
  }
  
  # Remove unnecessary columns
  cols_to_remove <- intersect(c('EE.d', 'VO2', 'VCO2', 'Fat_substrate_.', 'Cho_substrate_.', 
                      'Pro_substrate_.', 'Hour_stool_sample', 'Day.of.menstrual.cycle',
                      'Av.length.of.menstrual.cycle', 'sleep.time.avg..h.', 
                      'in.bed.time.avg..h.', 'sleep.efficiency', 'IPAQ_0', 'IPAQ_0_cat'), names(data))
  data <- remove_columns(data, cols_to_remove)
  
  # Remove columns that match the pattern 'Specify' using grep
  data <- data[ , -grep("^[Ss]pecify(\\.\\d+)?$", names(data))]
  
  # Tidy character columns
  data <- tidyCh_df(data)
  
  # Fix typos
  data$Physical.exercise.description <- gsub("endurnace|enduance", "endurance", data$Physical.exercise.description)
  data$Physical.exercise.description <- gsub(" ", "", data$Physical.exercise.description)
  data$STAI_t_cat <- gsub(" ", "", data$STAI_t_cat)
  
  # Mutate columns
  data <- data %>%
    mutate(Self.reported.diet = if_else(Self.reported.diet %in% c("flexitarian", "gf"), 
                                        "omnivore", 
                                        if_else(Self.reported.diet == "lacto-veg", 
                                                "vegetarian", 
                                                Self.reported.diet))) %>%
    mutate(`smoking or vaping` = ifelse(smoke | vape, TRUE, FALSE))  %>%
    dplyr::select(-smoke, -vape) %>% # Remove the smoke and vape columns
    relocate(`smoking or vaping`, .after = contraceptives)
  
  #Self.reported.diet %in% c("flexitarian", "gf") checks if the value of Self.reported.diet is either "flexitarian" or "gf". you don't use "|", but the %in% operator 
  #to check a vector of values
  #If the condition is true, if_else assigns "omnivore" to Self.reported.diet.
  
  
#factorise columns
  cols_fact <- c("STAI_t_cat", "Habitual.activity.level", "Mestrual.cycle.phase", "Self.reported.diet")
  data <- factorize_columns(data, cols_fact)
  
  # Reverse the levels of STAI_t_cat after factorization
  data$STAI_t_cat <- fct_rev(data$STAI_t_cat)
  
  # Convert columns to numeric
  cols_num <- c("BMI", "IPAQ.SF.score")
  data <- make_numeric(data, cols_num)
  
  # tidy and factorize STAI-s
  data <- data %>%
    mutate(
      STAI_s_cat = if_else(STAI_s_cat == "moderate", "mod", STAI_s_cat)
    )
  data$STAI_s_cat <- as.factor(data$STAI_s_cat)
  
  
  # SPLIT INTO PERSON DATAFRAME (df_t1_p)
  data_p <- data %>% filter(in.person == TRUE)
  
  return(list(data_all = data, data_p = data_p))  # Return both the full df_t1 and the person-specific df_t1_p

}


############################################################################################################################################################ FFQ

# Function to preprocess FFQ_t1 (specific to FFQ dataset)
preprocess_FFQ <- function(FFQ_original, data_all, data_p, excl_time_point = NULL) {
  
  
  # Remove the first two rows (now redundant)
  FFQ <- FFQ_original[-(1:2), ]
  
  # Ensure Participant IDs are trimmed and converted to lowercase
  FFQ <- tidyCh_df(FFQ)
  
  # Remove participants with specified suffix (e.g., "_T2") in their ID, if provided
  if (!is.null(excl_time_point)) {
    pattern <- paste0("_", excl_time_point, "$")
    FFQ <- FFQ[!grepl(pattern, FFQ$ParticipantId), ]
    print(paste("After excluding participants with", pattern, ", rows remaining:", nrow(FFQ)))
  } else {
    print("No exclusion pattern provided.")
  }
  
  # Remove unnecessary columns
  cols_to_remove <- c('SubmissionDate_Date', 'Please..insert.your.ID.number_NA', 'PersonDetails_Sex_Biological sex')
  FFQ <- remove_columns(FFQ, cols_to_remove)
  
  # Align Participant IDs with data_all and filter out non-matching ones
  FFQ <- FFQ[FFQ$ParticipantId_Units %in% data_all$ID, ]  # Keep rows with matching IDs
  print(paste("After alignment with data_all IDS, rows remaining:", nrow(FFQ)))
  
  # Subset the dataframe to include relevant columns
  cols_of_interest <- c("ParticipantId_Units", "Questionnaire_HEI_HEI_RESULT_NA", "GlycemicLoad_Total_NA", 
                        "Energy_Total_including.alcohol_Kcal", "Carbohydrate_Total_g", "Sugar_Total_g", 
                        "Fiber_Food_g", "Protein_Total_g", "Fat_Total_g", "SaturatedFats_Total_g", 
                        "UnsaturatedFats_Total_g", "Omega3_Total_g", "Omega6_Total_g", "TransFats_Total_g", 
                        "Cholesterol_mg", "Sodium_Total_mg", "Iron_Total_mg", "Calcium_Total_mg", 
                        "Selenium_Total_mcg", "Chromium_mcg", "VitaminA_Total_mcg", "BetaCarotene_mcg", 
                        "Lycopene_mcg", "Riboflavin_Total_mg", "Thiamin_Total_mg", "VitaminB12_Total_mcg", 
                        "VitaminB6_Total_mg", "Folate_Total_mcg", "VitaminC_Total_mg", "VitaminD_Total_mcg", 
                        "VitaminE_Total_mg", "Caffeine_mg")
  
  FFQ <- FFQ[, cols_of_interest]
  
  # Rename columns for readability
  names(FFQ) <- c("ID", "HEI.2020_tot", "Glycemic_Load", "Energy(Kcal)", "Carbs(g)", "Sugars(g)", "Fibers(g)", 
                  "Prot(g)", "Fats(g)", "Sat_FAs(g)", "Unsat_FAs(g)", "Omega3(g)", "Omega6(g)", "TFAs(g)", 
                  "Cholesterol(mg)", "Sodium(mg)", "Iron(mg)", "Calcium(mg)", "Selenium(mcg)", "Chromium(mcg)", 
                  "Vit_A(mcg)", "BetaCarotene(mcg)", "Lycopene(mcg)", "Riboflavin(mg)", "Thiamin(mg)", 
                  "Vit_B12(mcg)", "Vit_B6(mg)", "Folate(mcg)", "Vit_C(mg)", "Vit_D(mcg)", "Vit_E(mg)", 
                  "Caffeine(mg)")
  
  # Convert relevant columns to numeric
  cols_num <- 4:ncol(FFQ)
  FFQ <- make_numeric(FFQ, cols_num)
  
  # Filter in-person dataset and align ID order with data_p
  FFQ_p <- FFQ[FFQ$ID %in% data_p$ID, ]
  FFQ_p <- FFQ_p[match(data_p$ID, FFQ_p$ID), ]
  
  return(list(FFQ_all = FFQ, FFQ_p = FFQ_p))  # Return cleaned and preprocessed FFQ data
}




############################################################################################################################################################ INTAKE24

preprocess_in24 <- function(data, ref_ids) {     #ref_ids: reference IDs vector to match
  
  # ----------------------------------------------------------------------------------------------------------- ### RAW IN24
  # Tidy character columns
  data <- tidyCh_df(data)
  
  # change ID col name to ease comparison between df
  if (!"User.ID" %in% colnames(data)) stop("The column 'User.ID' is missing in the dataset.")
  data <- data %>%
    rename_with(~ "ID", .cols = "User.ID")
  
  
  # set differences in ID col and print (ID need to match)
  excluded_ids <- setdiff(unique(data$ID), ref_ids)
  message("Excluded participants:", paste(excluded_ids, collapse = ", "))
  
  # Remove excluded participant
  data <- filter(data, !ID %in% excluded_ids)
  print(paste("Remaining participants:", length(unique(data$ID))))
  
  #chech IDs match
  unmatched_ids <- setdiff(ref_ids, unique(data$ID))
  
  if (length(unmatched_ids) > 0) {
    warning(
      "Some IDs in ref_ids are missing after exclusion: ", paste(unmatched_ids, collapse = ", ")
    )
  } else {
    message("ll IDs match between data and ref_ids.")
  }
  
  
 # remove cols 
  cols_to_remove <- intersect(c("Survey.ID",  "Device.information..user.agent.", "Submission.time", "Time.to.complete", "Diet", "Food.amount", "Reason.for.unusual.food.amount",
                      "Proxy","Proxy.Issues", "Meal.Index", "Meal.ID", "Meal.name", "Food.source","Food.Index","Search.term","Food.ID",
                      "Description..local.","Nutrient.table.name","Nutrient.table.code", "Food.group..local.","Ready.meal","Brand",
                      "As.served.weight.factor", "Serving.size..g.ml.","Serving.image", "Leftovers..g.ml.", "Leftovers.image", "Reasonable.amount","Missing.food.ID","Missing.food.description",
                      "Missing.food.portion.size", "Missing.food.leftovers", "Sub.group.code"), names(data))
 
  if (length(cols_to_remove) > 0) {
    data <- remove_columns(data, cols_to_remove)
  } 
  
  print("Columns after removal:")
  print(colnames(data))

  #factorize cols
  cols_to_factorize <- intersect(c("Cooking.oil.used", "Food.group.code", "Food.group..en."), names(data))
  
  if (length(cols_to_factorize) > 0) {
    data <- factorize_columns(data, cols_to_factorize)
  }
  
  #group levels
  if ("Cooking.oil.used" %in% colnames(data)) {
  data <- data %>%
    mutate(Cooking.oil.used = if_else(grepl("^other", Cooking.oil.used), #check for every values in the cooking oil col starting with other
                                      "other",  #substitute with other only
                                      if_else(Cooking.oil.used %in% c("vegetable oil (rapeseed)", "sunflower oil"), 
                                              "rapeseed/sunflower oil",
                                              Cooking.oil.used))) %>% #when ^other = false, keep existing values
    mutate(Cooking.oil.used = as.factor(Cooking.oil.used))
  print(levels(data$Cooking.oil.used))
  } else {
    warning("Cooking.oil.used col not found.")
}
  
  #make numeric
  cols_to_make_numeric <- names(data)[9:ncol(data)]
  
  if (length(cols_to_make_numeric) > 0) {
  data <- make_numeric(data, cols_to_make_numeric)
  }
  

  print("Head of the cleaned data:")
  print(head(data))

  # --------------------------------------------------------------------------------------------  ###  IN24_d #ni and #fi 
  
  # in24 BY DAY  
  if ("Start.time" %in% colnames(data)) {
  data_d <- data %>%
    group_by(ID, Start.time) %>%
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = 'drop') %>%
    group_by(ID) %>% 
    mutate(days.adherence = n()) %>% #n() counts the number of days per ID
    relocate(days.adherence, .after = ID)
  print("data_d successfully processed")
  } else {
    warning("'Start.time' column not found in the dataset.")
  }
  
  # ----------------------------------------------------------------------------------------- ###  IN24_avg #ni and #fi 
    
  # in24 AVG
  data_avg <- data_d %>%
    group_by(ID) %>%
    summarise(across(where(is.numeric), \(x) mean(x, na.rm = T)))
  
  
  # Align with the order of ref_ids (ID column from df_p)
  data_avg <- data_avg[match(ref_ids, data_avg$ID), ]
  
  # Ensure alignment and check for any rows with all NAs after matching IDs
  if (any(is.na(data_avg$ID))) {
    warning("Some IDs in ref_ids do not match any ID in data_avg.")
  } else {
    print("All IDs in ref_ids successfully matched.")
  }
  print("Data_ni after matching IDs:")
  print(head(data_avg))
    
  # -------------------------------------------------------------------------------------- ### IN24_fi #subset fi avg
  
  # Select only columns that exist in data_avg (avoids errors)
  data_fi <- data_avg[, colnames(data_avg) %in% c("ID", "days.adherence", "Energy..kcal.", "Fruit", "Dried.fruit", 
                                                  "Fruit.juice", "Smoothie.fruit", "Tomatoes", "Tomato.puree", 
                                                  "Brassicaceae", "Yellow.Red.Green", "Beans", "Nuts", 
                                                  "Other.Vegetables", "Beef", "Lamb", "Pork", "Processed.Red.Meat", 
                                                  "Other.Red.Meat", "Burgers", "Sausages", "Offal", "Poultry", 
                                                  "Processed.Poultry", "Game.Birds", "White.Fish", "Oily.Fish", 
                                                  "Canned.Tuna", "Shellfish", "Cottage.Cheese", "Cheddar.Cheese", 
                                                  "Other.Cheese"), drop = FALSE]
  
  
  # Check if the selected columns are present
  missing_cols <- setdiff(c("ID", "days.adherence", "Energy..kcal.", "Fruit", "Dried.fruit", "Fruit.juice", 
                            "Smoothie.fruit", "Tomatoes", "Tomato.puree", "Brassicaceae", "Yellow.Red.Green", 
                            "Beans", "Nuts", "Other.Vegetables", "Beef", "Lamb", "Pork", 
                            "Processed.Red.Meat", "Other.Red.Meat", "Burgers", "Sausages", "Offal", 
                            "Poultry", "Processed.Poultry", "Game.Birds", "White.Fish", "Oily.Fish", 
                            "Canned.Tuna", "Shellfish", "Cottage.Cheese", "Cheddar.Cheese", "Other.Cheese"), 
                          colnames(data_fi))
  
  if (length(missing_cols) > 0) {
    warning("Missing columns in `data_fi`: ", paste(missing_cols, collapse = ", "))
  } else {
    message("ll expected columns are successfully selected.")
  }
  
  # Add suffix for unit of measure
  cols_g_fi <- intersect(c( "Fruit", "Dried.fruit", "Fruit.juice", 
                            "Smoothie.fruit", "Tomatoes", "Tomato.puree", "Brassicaceae", "Yellow.Red.Green", 
                            "Beans", "Nuts", "Other.Vegetables", "Beef", "Lamb", "Pork", 
                            "Processed.Red.Meat", "Other.Red.Meat", "Burgers", "Sausages", "Offal", 
                            "Poultry", "Processed.Poultry", "Game.Birds", "White.Fish", "Oily.Fish", 
                            "Canned.Tuna", "Shellfish", "Cottage.Cheese", "Cheddar.Cheese", "Other.Cheese"), names(data_fi))
  
  data_fi <- add_suffix(data_fi, cols_g_fi, "g")
  
  print("Column names in data_fi after suffix addition:")
  print(names(data_fi))
  
  # Calculate most frequently used oil and resolve ties with condition
  preferred_oil <- data %>%
    group_by(ID, Cooking.oil.used) %>%
    summarise(Frequency = n(), .groups = "drop") %>%
    group_by(ID) %>%
    filter(Frequency == max(Frequency)) %>%   # Keep only oils with the highest frequency
    summarise(
      Preferred.Oil = if ("did not use" %in% Cooking.oil.used & n() > 1) {
        # If there is a tie and "did not use" is one of the options, exclude it
        paste(Cooking.oil.used[Cooking.oil.used != "did not use"], collapse = ", ")
      } else {
        # Otherwise, keep all oils (including ties)
        paste(Cooking.oil.used, collapse = ", ")
      }
    )
  
  data_fi <- data_fi %>%
    left_join(preferred_oil, by = "ID")

  
  # ---------------------------------------------------------------------------------- ###  IN24_ni #subset ni avg
  
  # in24_avg NI Select nutrient intake columns
  data_ni <- data_avg %>%
    dplyr::select(-Portion.size..g.ml., -Total.nitrogen, -Nitrogen.conversion.factor, -Energy..kJ., -Fruit, 
           -Dried.fruit, -Fruit.juice, -Smoothie.fruit, -Tomatoes, -Tomato.puree, -Brassicaceae, - Other.sugars..UK.,
           -Yellow.Red.Green, -Beans, -Nuts, -Other.Vegetables, -Beef, -Lamb, -Pork, 
           -Processed.Red.Meat, -Other.Red.Meat, -Burgers, -Sausages, -Offal, -Poultry, 
           -Processed.Poultry, -Game.Birds, -White.Fish, -Oily.Fish, -Canned.Tuna, -Shellfish, 
           -Cottage.Cheese, -Cheddar.Cheese, -Other.Cheese)
  
  # Check if nutrient intake columns are selected correctly
  print("Nutrient Intake Columns Selected:")
  print(names(data_ni))
  
  
  # Order data_ni cols grouping similar nutrients
  # Use `intersect` to avoid errors from missing columns
  expected_cols <- c("ID", "days.adherence", "Energy..kcal.", "Protein", "Fat", "Carbohydrate", "Starch", 
                     "Englyst.fibre", "AOAC", "Total.sugars", "Total.FS", 
                     "Non.milk.extrinsic.sugars", "Intrinsic.and.milk.sugars", "Glucose", 
                     "Fructose", "Maltose", "Lactose", "Sucrose", 
                     "FS.Table.sugar", "FS.Other.Added.Sugar", "FS.Honey", "FS.Fruit.Juice", 
                     "FS.Dried.Fruit", "FS.Fruit.Puree", "FS.Stewed.Fruit", "FS.Vegetable.Puree", 
                     "Satd.FA", "Cis.Mon.FA", "Cis.n3.FA", "Cis.n6.FA", "Trans.FA", "Cholesterol", 
                     "Vitamin.A", "Retinol", "Total.carotene", "Alpha.carotene", "Beta.carotene", 
                     "Beta.cryptoxanthin", "Thiamin", "Riboflavin", "Niacin", "Tryptophan.60", 
                     "Niacin.equivalent", "Pantothenic.acid", "Vitamin.B6", "Biotin", "Folate", 
                     "Vitamin.B12", "Vitamin.C", "Vitamin.D", "Vitamin.E", "Sodium", "Potassium", 
                     "Calcium", "Magnesium", "Phosphorus", "Copper", "Zinc", "Chloride", "Manganese", 
                     "Selenium", "Iodine", "Iron", "Haem.iron", "Non.haem.iron", "Alcohol", "Water")
  
  selected_cols <- intersect(expected_cols, names(data_ni))
  
  missing_cols <- setdiff(expected_cols, selected_cols)
  if (length(missing_cols) > 0) {
    warning("The following expected columns are missing from data_ni:", paste(missing_cols, collapse = ", "))
  } else {
    print("All expected columns are successfully selected.")
  }
  
  data_ni <- data_ni[, selected_cols]
  
  # Convert specified columns from µg to mg for nutrient intake
  columns_to_convert <- c("Retinol", "Total.carotene", "Alpha.carotene", "Beta.carotene", 
                          "Beta.cryptoxanthin", "Vitamin.A", "Vitamin.D", 
                          "Vitamin.B12", "Folate", "Biotin", "Iodine", "Selenium")
  
  if (all(columns_to_convert %in% colnames(data_ni))) {
  data_ni <- convert_ug_to_mg(data_ni, columns_to_convert) 
  } else {
    warning("Some columns in 'column to convert' are missing and not converted")
  }
  
  # Add mg suffix for units in nutrient intake data
  columns_mg <- intersect(c("Cholesterol", "Retinol", "Total.carotene", "Alpha.carotene", "Beta.carotene", 
                  "Beta.cryptoxanthin", "Vitamin.A", "Vitamin.D", 
                  "Vitamin.B12", "Folate", "Biotin", "Iodine", "Selenium",
                  "Thiamin", "Riboflavin", "Niacin", "Tryptophan.60", 
                  "Niacin.equivalent", "Vitamin.C", "Vitamin.E", 
                  "Vitamin.B6", "Pantothenic.acid", 
                  "Sodium", "Potassium", "Calcium", 
                  "Magnesium", "Phosphorus", "Iron", 
                  "Haem.iron", "Non.haem.iron", 
                  "Copper", "Zinc", "Chloride", 
                  "Manganese"), names(data_ni))
  
  data_ni <- add_suffix(data_ni, columns_mg, "mg")

  
  # add also _g suffix for clarity
  columns_g <- intersect(c("Water", "Protein", "Fat", 
                 "Carbohydrate", "Starch", "Englyst.fibre", "AOAC",
                 "Total.sugars", "Total.FS", "Non.milk.extrinsic.sugars", 
                 "Intrinsic.and.milk.sugars", "Glucose", "Fructose", 
                 "Maltose", "Lactose", "Sucrose", "Other.sugars..UK.", 
                 "FS.Table.sugar", "FS.Other.Added.Sugar", "FS.Honey", 
                 "FS.Fruit.Juice", "FS.Dried.Fruit", "FS.Fruit.Puree", 
                 "FS.Stewed.Fruit", "FS.Vegetable.Puree", 
                 "Satd.FA", "Cis.Mon.FA", "Cis.n3.FA", "Cis.n6.FA", 
                 "Trans.FA", "Alcohol"), names(data_ni))
  
  data_ni <- add_suffix(data_ni, columns_g, "g")
  
  # Add composite columns to nutrient intake data
  data_ni <- data_ni %>%
    mutate(
      PUFAs_g = Cis.n3.FA_g + Cis.n6.FA_g,
      Unsatd.FA_g = Cis.Mon.FA_g + PUFAs_g,
      Micronutrients_tot_mg = rowSums(cbind(Vitamin.A_mg, Thiamin_mg, Riboflavin_mg, Niacin.equivalent_mg, 
                                            Pantothenic.acid_mg, Vitamin.B6_mg, Biotin_mg, Folate_mg, 
                                            Vitamin.B12_mg, Vitamin.C_mg, Vitamin.D_mg, Vitamin.E_mg), 
                                      na.rm = TRUE),
      Vit_fat_soluble_mg = rowSums(cbind(Vitamin.A_mg, Vitamin.D_mg, Vitamin.E_mg), na.rm = TRUE),
      Vit_water_soluble_mg = rowSums(cbind(Thiamin_mg, Riboflavin_mg, Niacin.equivalent_mg, 
                                           Pantothenic.acid_mg, Vitamin.B6_mg, Biotin_mg, 
                                           Folate_mg, Vitamin.B12_mg, Vitamin.C_mg), 
                                     na.rm = TRUE),
      Minerals_tot_mg = rowSums(cbind(Sodium_mg, Potassium_mg, Calcium_mg, Magnesium_mg, Phosphorus_mg, 
                                      Copper_mg, Zinc_mg, Chloride_mg, Manganese_mg, Selenium_mg, Iodine_mg), 
                                na.rm = TRUE),
      Minerals_salt_mg = Sodium_mg + Chloride_mg,
      Minerals_no_salt_mg = Minerals_tot_mg - Minerals_salt_mg
    )
  
  
  composite_cols <- c("Vit_fat_soluble_mg", "Vit_water_soluble_mg",  "PUFAs_g", "Unsatd.FA_g", "Micronutrients_tot_mg", "Minerals_tot_mg", "Minerals_salt_mg", "Minerals_no_salt_mg")
  
  missing_composites <- setdiff(composite_cols, colnames(data_ni)) #check for the presence of composite_cols ONLY in colnames 
  
  if (length(missing_composites) == 0) {
    print("All composite columns successfully created.")
  } else {
    warning("The following composite columns have not been successfully created:", paste(missing_composites, collapse = ","))
  }
  
  
  # ----------------------------------------------------------------------------------------- ###  IN24_fg #call function
  
  data_fg <- Fg_categories(data, data_fi, data_ni$ID)
  

  
  # Return data frames as a list
  return(list(food_nutrient_intake = data_avg, food_intake = data_fi, nutrient_intake = data_ni, food_group_intake = data_fg))
  
}




