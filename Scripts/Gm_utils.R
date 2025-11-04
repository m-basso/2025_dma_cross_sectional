
# ------------------------------------------------------------------------------------------------ create phylogenetic tree and simplify data rownames

create_phylotree <- function(data, l_level, ranks) {    # ranks = c()
  
  # create taxonomy table
  phylo_tree_l_level <- data.frame(
    row.names = rownames(data), 
    taxonomy = rownames(data),
    stringsAsFactors = FALSE) # safer to ensure no premature factor
  
  # split levels
  phylo_tree_l_level <- phylo_tree_l_level %>%
    tidyr::separate(taxonomy, into = ranks,
                    sep = ";", fill = "right")
  
  
  # Check for missing values after splitting
  cat("Check for missing (NA) values per column:\n")
  print(colSums(is.na(phylo_tree_l_level)))
  
  # now you can simplify rownames as taxonomy is stored 
  rownames(data) <- gsub("^.*;", "", rownames(data))
  rownames(phylo_tree_l_level) <- gsub("^.*;", "", rownames(phylo_tree_l_level))
  
  # Ensure unique rownames after simplification
  rownames(data) <- make.unique(rownames(data))
  rownames(phylo_tree_l_level) <- make.unique(rownames(phylo_tree_l_level))
  
  # explicitely re-order just in case
  phylo_tree_l_level <- phylo_tree_l_level[rownames(data), , drop = FALSE]
  
  # Confirm that rownames match between count table and taxonomy table
  if (identical(rownames(phylo_tree_l_level), rownames(data))) {
    cat("Taxonomy table and count table rownames are already iidentical.\n")
  } else {
    cat("Rownames are not identical!\n")
  }
  
  # factorize
  phylo_tree_l_level[] <- lapply(phylo_tree_l_level, as.factor)
  
  
  return(list(data = data, phylo_tree = phylo_tree_l_level))
}



# ------------------------------------------------------------------------------------------------ create phylogenetic tree, hc object, and simplify data rownames

create_hc_phylotree <- function(data, l_level, ranks) {    # ranks = c()
  
  # create taxonomy table
  phylo_tree_l_level <- data.frame(
    row.names = rownames(data), 
    taxonomy = rownames(data),
    stringsAsFactors = FALSE) # safer to ensure no premature factor
  
  # split levels
  phylo_tree_l_level <- phylo_tree_l_level %>%
    tidyr::separate(taxonomy, into = ranks,
                    sep = ";", fill = "right")
  
  
  # Check for missing values after splitting
  cat("Check for missing (NA) values per column:\n")
  print(colSums(is.na(phylo_tree_l_level)))
  
  # now you can simplify rownames as taxonomy is stored 
  rownames(data) <- gsub("^.*;", "", rownames(data))
  rownames(phylo_tree_l_level) <- gsub("^.*;", "", rownames(phylo_tree_l_level))
  
  # Ensure unique rownames after simplification
  rownames(data) <- make.unique(rownames(data))
  rownames(phylo_tree_l_level) <- make.unique(rownames(phylo_tree_l_level))
  
  # explicitely re-order just in case
  phylo_tree_l_level <- phylo_tree_l_level[rownames(data), , drop = FALSE]
  
  # Confirm that rownames match between count table and taxonomy table
  if (identical(rownames(phylo_tree_l_level), rownames(data))) {
    cat("Taxonomy table and count table rownames are already iidentical.\n")
  } else {
    cat("Rownames are not identical!\n")
  }
  
  # factorize
  phylo_tree_l_level[] <- lapply(phylo_tree_l_level, as.factor)
  
  # One-hot encoding (binary)
  # Create a 0/1 presence-absence matrix from the factor data
  phylo_tree_l_level_encoded <- model.matrix(~ . -1, data = phylo_tree_l_level)
  
  # Make sure the encoded matrix has the same row names as the data frame
  cat("\nRownames check between tree table and encoded table. Size:\n")
  print(identical(rownames(phylo_tree_l_level_encoded), rownames(phylo_tree_l_level)))
  
  # create dist matrix
  dist_l_level <- dist(phylo_tree_l_level_encoded, method = "binary")
  
  # print dim
  cat("\nBinary distance matrix computed. Size:\n")
  print(dim(as.matrix(dist_l_level)))
  
  # Ensure again no differences exist between the tables
  cat("\nRownames check between data and tree table. Size:\n")
  print(identical(rownames(data), rownames(phylo_tree_l_level)))
  cat("\nRownames check between tree table and encoded table. Size:\n")
  print(identical(rownames(phylo_tree_l_level), rownames(phylo_tree_l_level_encoded)))
  
  # hc
  hc_l_level <- hclust(dist_l_level)
  
  # convert into phylo obj
  phylo_tree_l_level_hc <- as.phylo(hc_l_level)
  
  cat("\nCreated phylo_tree_hc with tip labels:\n")
  print(head(phylo_tree_l_level_hc$tip.label))
  
  # final matching check
  cat("\nData rownames and tree tips match:\n")
  identical(rownames(data), phylo_tree_l_level_hc$tip.label)
  
  return(list(data = data, phylo_tree = phylo_tree_l_level, phylo_obj = phylo_tree_l_level_hc))
}



# ------------------------------------------------------------------------------------------------ initialise empty robmed and lmrob df

create_robmed_df <- function(robmed_list, n_raws) {
  data.frame(
    mediator      = character(n_raws),
    path_a        = numeric(n_raws),
    a_pval        = numeric(n_raws),
    ade           = numeric(n_raws),
    ade_pval      = numeric(n_raws),
    acme          = numeric(n_raws),
    acme_bca_lower= numeric(n_raws),
    acme_bca_upper= numeric(n_raws),
    stringsAsFactors = FALSE
  )
}

create_pathb_df <- function(lmrob_list, n_raws) {
  data.frame(
    feature       = character(n_raws),
    std_beta      = numeric(n_raws),
    ci_lower      = numeric(n_raws),
    ci_upper      = numeric(n_raws),
    std_error     = numeric(n_raws),
    r_squared     = numeric(n_raws),
    adj_r_squared = numeric(n_raws),
    pval          = numeric(n_raws),
    stringsAsFactors = FALSE
  )
}

create_gam_df <- function(gam_list, n_raws) {
  data.frame(
    feature       = character(n_raws),
    edf           = numeric(n_raws),
    r_squared     = numeric(n_raws),
    pval          = numeric(n_raws),
    stringsAsFactors = FALSE
  )
}

#k.index       = numeric(n_raws),
#pval          = numeric(n_raws),


# ------------------------------------------------------------------------------------------- df for acme and path a

robmed_acme_table <- function(robmed_model, feats_number) {
  
# initialize df
robmed_acme_df <- create_robmed_df(robmed_model, feats_number)
# number of med
n_meds <- length(robmed_model)

# loop
for (i in seq_along(robmed_model)) {
  
  robmed_acme_df$mediator[i] <- names(robmed_model)[i]
  robmed_acme_df$path_a[i] <- round((summary(robmed_model[[i]]$fit$fit_mx))$coefficients["HEI_cluster_22", "Estimate"], digits = 2)
  robmed_acme_df$a_pval[i] <- round((summary(robmed_model[[i]]$fit$fit_mx))$coefficients["HEI_cluster_22", "Pr(>|t|)"], digits = 3)
  robmed_acme_df$ade[i] <- round(robmed_model[[i]]$direct, digits = 2)
  robmed_acme_df$ade_pval[i] <- round((summary(robmed_model[[i]]$fit$fit_ymx))$coefficients["HEI_cluster_22", "Pr(>|t|)"], digits = 3)
  robmed_acme_df$acme[i] <- round(robmed_model[[i]]$indirect, digits = 2)
  robmed_acme_df$acme_bca_lower[i] <- round(robmed_model[[i]]$ci["Lower"], digits = 2)
  robmed_acme_df$acme_bca_upper[i] <- round(robmed_model[[i]]$ci["Upper"], digits = 2)
}

# inspect CIs incl/ not incl 0 and path a pvals < 0.05 (+ adj)
robmed_acme_df <- robmed_acme_df %>%
  mutate(
    a_qval = round(p.adjust(a_pval, method = "fdr"), digits = 3),
    ade_qval = round(p.adjust(ade_pval, method = "fdr"), digits = 3),
    acme_signif = ifelse(
      (acme_bca_lower > 0 & acme_bca_upper > 0) |
        (acme_bca_lower < 0 & acme_bca_upper < 0),
      "zero not incl",
      "zero incl"
    ),
    a_signif_adj = ifelse(
      a_qval < 0.05, "< 0.05", "> 0.05"
    ),
    ade_signif_adj = ifelse(
      ade_qval < 0.05, "< 0.05", "> 0.05"
    )
  ) %>% relocate(a_qval, .after = a_pval) %>% relocate(ade_qval, .after = ade_pval)

# rownames
rownames(robmed_acme_df) <- robmed_acme_df$mediator
robmed_acme_df <- robmed_acme_df[, -1]

# factorize acme_sign
robmed_acme_df$acme_signif <- factor(robmed_acme_df$acme_signif, levels = c("zero not incl", "zero incl"))

# arrange
robmed_acme_df <- robmed_acme_df %>%
  arrange(acme_signif)

# user friendly table 
print(kable(robmed_acme_df, align = "c"))


return(robmed_acme_df)

}

# -------------------------------------------------------------------------------------------------- df for path b


robmed_b_table <- function(robmed_model, feats_number) {
  
  
# initialise
robmed_b_df <- create_pathb_df(robmed_model, feats_number)

# number of med
n_meds <- length(robmed_model)

# loop
for (i in seq_along(robmed_model)) {
  robmed_b_df$feature[i] <- names(robmed_model)[i]
  robmed_b_df$std_beta[i] <- round((summary(robmed_model[[i]]$fit$fit_ymx))$coefficients[2, "Estimate"], digits = 2) 
  robmed_b_df$ci_lower[i] <- round((confint(robmed_model[[i]]$fit$fit_ymx))[2, "2.5 %"], digits = 2)
  robmed_b_df$ci_upper[i] <- round((confint(robmed_model[[i]]$fit$fit_ymx))[2, "97.5 %"], digits = 2)
  robmed_b_df$std_error[i] <- round((summary(robmed_model[[i]]$fit$fit_ymx))$coefficients[2, "Std. Error"], digits = 2)
  robmed_b_df$pval[i] <- round((summary(robmed_model[[i]]$fit$fit_ymx))$coefficients[2, "Pr(>|t|)"], digits = 3)
  robmed_b_df$r_squared[i] <- round((summary(robmed_model[[i]]$fit$fit_ymx))$r.squared, digits =2)
  robmed_b_df$adj_r_squared[i] <- round((summary(robmed_model[[i]]$fit$fit_ymx))$adj.r.squared, digits = 2)
}

# add sign cols
robmed_b_df <- robmed_b_df %>%
  mutate(
    qval = round(p.adjust(pval, method = "fdr"), digits = 3),
    signif_bef_adj = ifelse(
      pval < 0.05, "< 0.05", "> 0.05"),
    signif_after_adj = ifelse(
      qval < 0.05, "< 0.05", "> 0.05")
  )

# rownames
rownames(robmed_b_df) <- robmed_b_df$feature
robmed_b_df <- robmed_b_df[, -1]

# arrange 
robmed_b_df <- robmed_b_df %>%
  arrange(qval, pval, std_beta)

# friendly table
print(kable(robmed_b_df, align = "c"))

return(robmed_b_df)

}


# -------------------------------------------------------------------------------------------------- df for lmrob


lmrob_table <- function(lmrob_model, feats_number) {
  
  
  # initialise
  lmrob_df <- create_pathb_df(lmrob_model, feats_number)
  
  # number of med
  n_meds <- length(lmrob_model)
  
  # loop
  for (i in seq_along(lmrob_model)) {
    lmrob_df$feature[i] <- names(lmrob_model)[i]
    lmrob_df$std_beta[i] <- round((summary(lmrob_model[[i]]))$coefficients[2, "Estimate"], digits = 2)
    lmrob_df$ci_lower[i] <- round((confint(lmrob_model[[i]]))[2, "2.5 %"], digits = 2)
    lmrob_df$ci_upper[i] <- round((confint(lmrob_model[[i]]))[2, "97.5 %"], digits = 2)
    lmrob_df$std_error[i] <- round((summary(lmrob_model[[i]]))$coefficients[2, "Std. Error"], digits =2)
    lmrob_df$pval[i] <- round((summary(lmrob_model[[i]]))$coefficients[2, "Pr(>|t|)"], digits =3)
    lmrob_df$adj_r_squared[i] <- round((summary(lmrob_model[[i]]))$adj.r.squared, digits = 2)
    lmrob_df$r_squared[i] <- round((summary(lmrob_model[[i]]))$r.squared, digits = 2)
  }
  
  # add sign cols
  lmrob_df <- lmrob_df %>%
    mutate(
      qval = round(p.adjust(pval, method = "fdr"), digits = 3),
      signif_bef_adj = ifelse(
        pval < 0.05, "< 0.05", "> 0.05"),
      signif_after_adj = ifelse(
        qval < 0.05, "< 0.05", "> 0.05")
    )
  
  # rownames
  rownames(lmrob_df) <- lmrob_df$feature
  lmrob_df <- lmrob_df[, -1]
  
  # arrange 
  lmrob_df <- lmrob_df %>%
    arrange(qval, pval, std_beta)
  
  # friendly table
  print(kable(lmrob_df, align = "c"))
  
  return(lmrob_df)
  
}




# -------------------------------------------------------------------------------------------------- df for gam


gam_table <- function(gam_model, feats_number) {
  
  
  # initialise
  gam_df <- create_gam_df(gam_model, feats_number)
  
  # number of med
  n_meds <- length(gam_model)
  
  # loop
  for (i in seq_along(gam_model)) {
    gam_df$feature[i] <- names(gam_model)[i]
    gam_df$edf[i] <- round(summary(gam_model[[i]])$s.table[, 1], digits = 2)
    gam_df$r_squared[i] <- round(summary(gam_model[[i]])$r.sq, digits = 2)
    gam_df$pval[i] <- round(summary(gam_model[[i]])$s.table[, 4], digits = 3)
  }
    
    # add sign cols
    gam_df <- gam_df %>%
      mutate(
        qval = round(p.adjust(pval, method = "fdr"), digits = 3)
      )
    
    # rownames
    rownames(gam_df) <- gam_df$feature
    gam_df <- gam_df[, -1]
    
    # arrange 
    gam_df <- gam_df %>%
      arrange(qval, pval, edf)
    
    # friendly table
    print(kable(gam_df, align = "c"))
    
    return(gam_df)
  
}














