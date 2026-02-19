make_williams_df <- function(blinding_codes,
                             n_ids,
                             id_name = "ID") {
  if (!requireNamespace("crossdes", quietly = TRUE)) {
    stop("Package 'crossdes' is required. Install with install.packages('crossdes').")
  }
  
  k <- length(blinding_codes)
  if (k < 2) stop("Provide at least 2 treatments in 'blinding_codes'.")
  if (n_ids < 1) stop("'n_ids' must be >= 1.")
  
  # Generate Williams design
  wd <- crossdes::williams(k)
  
  # Map numeric treatment codes to blinding codes
  wd_named <- matrix(
    blinding_codes[wd],
    nrow = nrow(wd),
    ncol = ncol(wd)
  )
  
  wd_df <- as.data.frame(wd_named, stringsAsFactors = FALSE)
  
  # Repeat rows to reach n_ids
  idx <- rep(seq_len(nrow(wd_df)), length.out = n_ids)
  out <- wd_df[idx, , drop = FALSE]
  
  # Add ID column first
  out[[id_name]] <- seq_len(nrow(out))
  out <- out[, c(id_name, setdiff(names(out), id_name))]
  
  # Add proper column names for samples
  n_samples <- ncol(out) - 1
  colnames(out) <- c(id_name, paste0("sample", seq_len(n_samples)))
  
  # Remove row names
  rownames(out) <- NULL
  
  # ---- Print position balance check ----
  cat("\nSample position balance check:\n")
  
  sample_only <- out[, -1, drop = FALSE]
  
  counts_matrix <- sapply(
    sample_only,
    function(x) table(factor(x, levels = blinding_codes))
    
  )
  
  colnames(counts_matrix) <- paste0(
    "Position",
    seq_len(ncol(counts_matrix))
  )
  
  print(
    data.frame(
      sample = blinding_codes,
      counts_matrix,
      row.names = NULL
    )
  )
  
  cat("\n")
  
  out
}

# Define your blinding codes in treatment_names
treatment_names <- c(859, 283, 405)

# n_ids = number of treatment sets in the balanced design
wd_df <- make_williams_df(
  blinding_codes = treatment_names,
  n_ids = 300
)

# Write the design file and upload to the Github repository

write.csv(wd_df, "wd.csv")