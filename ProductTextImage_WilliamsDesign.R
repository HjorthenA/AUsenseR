# ---------------------------
# 1) Setup: packages + products table (code, description HTML, image URL)
# ---------------------------
library(tibble)
library(htmltools)


make_williams_products_df <- function(products_tbl,
                                      n_ids,
                                      id_name = "ID",
                                      name_size = "1.3em",
                                      desc_size = "1.0em") {
  
  if (!requireNamespace("crossdes", quietly = TRUE)) stop("Package 'crossdes' is required.")
  if (!all(c("ProductFileName", "ProductName", "ProductBody") %in% names(products_tbl))) {
    stop("products_tbl must contain ProductFileName, ProductName, and ProductBody")
  }
  
  products_tbl <- products_tbl %>%
    mutate(ProductHTML = paste0(
      "<span style='font-size: ", name_size, ";'><strong>", ProductName, "</strong></span><br><br>",
      "<span style='font-size: ", desc_size, ";'>", ProductBody, "</span>"
    ))
  
  product_filenames <- as.character(products_tbl$ProductFileName)
  k <- length(product_filenames)
  wd <- crossdes::williams(k)
  wd_named <- matrix(product_filenames[wd], nrow = nrow(wd), ncol = ncol(wd))
  out <- as.data.frame(wd_named, stringsAsFactors = FALSE)
  idx <- rep(seq_len(nrow(out)), length.out = n_ids)
  out <- out[idx, , drop = FALSE]
  out[[id_name]] <- seq_len(nrow(out))
  out <- out[, c(id_name, setdiff(names(out), id_name))]
  colnames(out) <- c(id_name, paste0("sample", seq_len(ncol(out) - 1)))
  rownames(out) <- NULL
  
  sample_cols <- grep("^sample\\d+$", names(out), value = TRUE)
  
  out %>%
    tidyr::pivot_longer(all_of(sample_cols), names_to = "sample_pos", values_to = "ProductFileName") %>%
    dplyr::left_join(products_tbl, by = "ProductFileName") %>%
    tidyr::pivot_wider(
      id_cols = all_of(id_name),
      names_from = sample_pos,
      values_from = c(ProductFileName, ProductHTML),
      names_glue = "{sample_pos}{ifelse(.value == 'ProductHTML', '_desc', '')}"
    )
}


# Define your design parameters
# ProductFileName = to define the filename of your product images uploaded to the Github repository
# ProductName = to specify product names
# ProductBody = to specify product descriptions

products_tbl <- tibble(
  ProductFileName = c(
    "SOY_LIQ", "DAIRY_LIQ",
    "SOY_SEMI", "DAIRY_SEMI",
    "SOY_SOLID", "DAIRY_SOLID"
  ),
  ProductName = c(
    "Soy milk", "Cow's milk", "Douhua",
    "Yoghurt from cow's milk", "Yuba (tofu skin)", "Cheese from cow's milk"
  ),
  ProductBody = c(
    "This product is a liquid, soy-based food product made from soybeans. It has a uniform, liquid consistency and can be poured and drunk. The product is typically consumed cold or at room temperature.",
    "This product is a liquid, dairy-based food product made from cow's milk. It has a uniform, liquid consistency and can be poured and drunk. The product is typically consumed cold or at room temperature.",
    "This product is a soft, firm, soy-based food product made from coagulated soymilk. It has a soft, firm consistency that holds its shape but can be easily divided with a spoon. The product is typically consumed cold or warm.",
    "This product is a creamy, dairy-based food product made from fermented cow's milk. It has a creamy consistency and can be eaten with a spoon. The product is typically consumed cold.",
    "This product is a rubbery, firm, soy-based food product made from coagulated soy milk. It has a rubbery, firm consistency that holds its shape, and are easy to chew. The product is typically consumed warm.",
    "This product is a firm dairy-based food product made from cow's milk. Often known as cheese for pizza. When heated it has a smooth, creamy to chewy, elastic consistency. The product is typically consumed warm or at room temperature."
  )
)

# n_ids = number of productsets in the balanced design
# id_name = name of the id col
# name_size = text size of the presented ProductName
# desc_size = text size of the presented ProductBody

wd_df <- make_williams_products_df(
  products_tbl = products_tbl,
  n_ids = 500,
  id_name = "ID",
  name_size = "1.5em",
  desc_size  = "1.1em"
)

# Write the design file and upload to the Github repository

write.csv(wd_df, "wd.csv", row.names = FALSE)


