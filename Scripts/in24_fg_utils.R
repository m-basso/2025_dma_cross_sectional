
# categorise food items in food groups and match IDs 

# from data :
#1 Processing category based on 749 items 
#2 Brain_gut_friendly categoty based on 749 items

# from data_fi :
#3 Natural Meat (red + poultry + game birds) #still not distinguishing beween sources and quality 
#4 Processed meat (red, poultry, sausages, burgers)
#5 Fish (white + oily + shellfish + canned tuna)


Fg_categories <- function(data, data_fi, ref_ids) {

  
  # Add a col to categorize food items as 1) minimally processed or 2) processed/ultraprocessed
  data <- data %>%
  mutate(Processing.category = if_else(Description..en. %in% c("mozzarella cheese", "onion, raw", "tomatoes, canned", "scrambled egg", "sourdough bread", 
                                                               "tomato based pasta sauce, home made", "tuna, in spring water, canned", "sweetcorn / sweet corn, canned", "mushrooms", "onions, fried in olive oil", 
                                                               "peanut butter, crunchy (with bits)", "mixed berries", "white basmati rice (including easy-cook)", "spaghetti/tagliatelle (pasta), white or tricolore", 
                                                               "prawns/shrimps, boiled", "natural yoghurt, fat free/very low fat (e.g. total 0% fat greek yoghurt)", "wholemeal (brown) bread", "honey", "natural yoghurt, greek-style", 
                                                               "chia seeds", "semi skimmed milk", "mixed vegetables stir fry (cooked from frozen)", "cucumber", "feta cheese", "olive oil", "dried herbs", "freshly squeezed lemon juice", 
                                                               "black pepper", "chicken curry home made", "cashew nuts, unsalted", "toast, multiseed wholemeal bread", "baby spinach, raw (e.g. in salad)", "sweet potato wedges", "lettuce",
                                                               "cottage pie (beef), home made", "broccoli, boiled", "melon, watermelon", "tomato based pasta sauce, with vegetables", "cherry tomatoes", "raspberries", 
                                                               "red/yellow/orange pepper, raw", "sprouting broccoli, cooked", "mackerel fillet, in a tomato sauce, canned", "mixed fruit juice, 100% fruit (e.g. tropicana)", 
                                                               "peanut butter, smooth", "orange juice, 100% fruit (e.g. tropicana)", "semi skimmed milk, boiled", "almonds", 
                                                               "blueberries / bilberries", "tofu/soya bean stir fry, including vegetables (with noodles)", "carrots, raw", "celery, raw", "mushrooms, raw", 
                                                               "vegetarian bolognese sauce, made with lentils", "whole milk", "olives", "orange", "fresh fruit salad", "stuffed pepper", "spring onions", "sugar-snap peas", 
                                                               "carrots", "red/yellow/orange pepper, cooked", "cauliflower, cooked", "tofu", "mediterranean roasted vegetables", "omelette, with cheese", "butter, unsalted", 
                                                               "green pesto", "trail mix, tropical fruit and nut mix", "lentil dahl / tarka dahl", "tangerines / mandarins / clementines/ satsumas", "avocado", "spinach", "miso soup", 
                                                               "kimchi", "red cabbage, raw", "udon/ramen noodles (e.g. plain, not egg)", "mango, frozen", "roast duck (skin eaten)", "egg noodles", "maple syrup", "green tea", 
                                                               "herbal/ fruit tea", "parmesan cheese", "blackcurrants", "soda bread, wholemeal (brown)", "chicken tikka, grilled", "porridge, made with semi skimmed milk", 
                                                               "beef rump steak, grilled", "sushi, salmon based", "baked potato / jacket potato, skin eaten", "salmon, cold smoked", "salmon, grilled or oven baked", "quinoa", 
                                                               "sweet potato", "granary bread", "roast turkey (skin not eaten)", "rocket leaves", "spaghetti/tagliatelle (pasta), wholemeal", "linseeds (flaxseeds)", "oats (uncooked)", 
                                                               "boiled egg", "parsley, fresh", "peas, cooked from frozen", "green beans/french beans", "sweetcorn / sweet corn, cooked", "parsnip, cooked", "brussels sprouts, cooked", 
                                                               "whole milk, boiled", "omelette, plain", "sundried tomatoes", "garlic", "parsnip, roasted", "carrots, roasted", "roast beef", "mixed vegetables, cooked from frozen", 
                                                               "spinach, cooked from frozen", "soya beans / edamame beans", "chicken fajita (filling only)", "omelette with vegetables and cheese", "vegetable soup", 
                                                               "butternut squash, cooked/roasted with oil", "onions, baked/roasted", "red/yellow/orange pepper, roasted", "couscous (plain)", "vegetable risotto", "haloumi cheese", 
                                                               "goats cheese", "raspberries, frozen", "pasta shapes, wholemeal", "cod/haddock steamed", "plain risotto", "mange tout, cooked", "natural yoghurt, low fat", 
                                                               "multiseed wholemeal bread", "chicken breast, marinated, homemade", "roast/grilled chicken thigh (skin not eaten)", "potatoes", "green beans/french beans, canned",
                                                               "vermicelli / rice noodles (including glass/thread noodles)", "courgette (zucchini), cooked", "cooked egg (baked/boiled)", "kefir", "peas", "coffee, fresh", 
                                                               "pak choi / bok choi, cooked", "tuna steak, grilled or oven baked", "chickpea curry (e.g. chana masala)", "aubergine parmigiana / aubergine bake", "paprika", 
                                                               "chives, fresh", "espresso coffee", "spanish omelette / frittata", "lemon", "tzatziki", "chickpeas, canned", "gouda cheese", "parma ham", "beef sirloin steak, grilled", 
                                                               "beetroot, raw", "passata (strained tomatoes)", "tomatoes, canned (pureed)", "beef burger, 100% beef, grilled (no bun)", "porridge, made with water", 
                                                               "beef sirloin steak, fried", "fruit squash / juice, no added sugar, diluted", "mixed peppers", "sushi, tuna based", "salmon, steamed", "dill, fresh", 
                                                               "vinegar (e.g. malt, balsamic, wine)", "gluten-free pasta (e.g. chickpea/cornflour)", "minced lamb, stewed", "pork casserole", "black eyed beans, canned", 
                                                               "muesli with fruit & nuts", "fried egg", "tomato, grilled", "tenderstem broccoli, cooked", "brown rice", "salmon salad (including pasta/grains/potatoes)", 
                                                               "broccoli, steamed/microwaved", "kale, cooked", "asparagus, cooked", "sea bass, baked or grilled", "poached egg", "barley (e.g pearl barley/wholegrain)", 
                                                               "decaf coffee, instant", "decaf tea", "cumin", "turmeric", "grapefruit", "walnuts", "plain popcorn", "red/yellow/orange pepper, fried", "kidney beans, canned", 
                                                               "peanut butter, no added sugar", "grapeseed oil", "carrots, fried", "cannellini beans", "lentils", "strawberries, frozen", "coriander (cilantro), fresh", 
                                                               "cayenne pepper", "chicken breast, fried", "minced beef, fried", "mushrooms, fried", "white rice, fried (e.g. pilau rice)", "leek, cooked", "nectarine", 
                                                               "raisins", "mixed vegetables, cooked from fresh", "new potatoes, skins eaten", "dried apple", "parsnip soup", "pasta with a vegetable based sauce", "coconut water"), 
                                       "Minimal_Kcal", 
                                       if_else(Description..en. %in% c("n/a","water (from tap, including hot water, filtered water)", "bottled mineral water, still or fizzy"), NA_character_, #account for n/a missing items and water (0Kcal)
                                               "Processed/UPF_Kcal"))) %>%
  relocate(Processing.category, .after = Description..en.)
  
  # Check for successful creation of Processing.category column
 if ("Processing.category" %in% colnames(data)) {
    unique_categories <- unique(data$Processing.category)
    expected_categories <- c("Minimal_Kcal", "Processed/UPF_Kcal", NA)
    
  # Verify if only expected categories are present
  if (all(unique_categories %in% expected_categories)) {
      print("Processing.category column successfully created with expected categories.")
    } else {
      warning("Unexpected categories found in Processing.category column:", 
              paste(setdiff(unique_categories, expected_categories), collapse = ", "))
    }
  } else {
    warning("Processing.category column was not created successfully.")
  }

  # Summarise by day
  data_fg_d <- data %>%
    pivot_wider(
      id_cols = c(ID, Start.time),
      names_from = Processing.category,
      values_from = Energy..kcal.,
      values_fn = list(Energy..kcal. = ~sum(.x, na.rm = TRUE))
    ) %>%
    dplyr::select(ID, Start.time, `Minimal_Kcal`, `Processed/UPF_Kcal`) %>%
    arrange(ID, Start.time)
  
  # Check if pivot was successful
  if (all(c("Minimal_Kcal", "Processed/UPF_Kcal") %in% colnames(data_fg_d))) {
    print("Pivot operation successful, with 'Minimal_Kcal' and 'Processed/UPF_Kcal' columns created.")
  } else {
    warning("Pivot operation did not create expected 'Minimal_Kcal' or 'Processed/UPF_Kcal' columns.")
  }
  
  # Avg by day
  data_fg <- data_fg_d %>%
    group_by(ID) %>%
    summarise(across(where(is.numeric), \(x) mean(x, na.rm = T)))
  
    
  # match ids
  data_fg <- data_fg[match(ref_ids, data_fg$ID), ]
  
  # Check alignment
  if (all(!is.na(data_fg$ID))) {
    print("All IDs matched successfully in data_fg.")
  } else {
    warning("Some IDs in ref_ids could not be matched in data_fg.")
  }

  
  # Add columns to group food items and create composite columns
  data_fg <- data_fg %>%
    left_join(data_fi, by = "ID") %>%
    mutate(
      Processed.meat_g = rowSums(cbind(`Processed.Red.Meat_g`, `Burgers_g`, `Sausages_g`, `Processed.Poultry_g`), na.rm = TRUE),
      Red.meat_g = rowSums(cbind(`Beef_g`, `Lamb_g`, `Pork_g`, `Other.Red.Meat_g`), na.rm = TRUE),
      All.non.processed.meat_g = rowSums(cbind(`Red.meat_g`, `Poultry_g`, `Game.Birds_g`, `Offal_g`), na.rm = TRUE),
      Fish_g = rowSums(cbind(`White.Fish_g`, `Oily.Fish_g`, `Canned.Tuna_g`, `Shellfish_g`), na.rm = TRUE)
    ) %>%
    dplyr::select(ID, days.adherence, Energy..kcal., `Minimal_Kcal`, `Processed/UPF_Kcal`, `Processed.meat_g`, 
                  `Red.meat_g`, `All.non.processed.meat_g`, `Fish_g`)
  
  
  
  # Check if new columns were created
  grouped_columns <- c("Processed.meat_g", "Red.meat_g", "All.non.processed.meat_g", "Fish_g")
  missing_grouped_columns <- setdiff(grouped_columns, colnames(data_fg))
  
  if (length(missing_grouped_columns) == 0) {
    print("All grouped columns successfully created.")
  } else {
    warning(paste("The following grouped columns have not been successfully created:", 
                  paste(missing_grouped_columns, collapse = ", ")))
  }

  
  # Return the final processed data  
  return(data_fg)
      
}