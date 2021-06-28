require(stringr)
require(dplyr)
require(tidyr)
require(Matrix)

# matrix_gen(df, col, missing, add.nodes) - Generate a sociomatrix based on a 
# generic shared attribute

# df - The data frame containing the original data in tabular format
# col - The column of the data frame containing the shared attribute
# missing - The value used to replace missing data for the shared attribute
# add.nodes - Column containing additional nodes with no attribute data
# include.nodes - List of nodes (likely from a subgraph) to match the resulting
#                 sociomatrix to using `include.nodes = V(subgraph)`

matrix_gen <- function (df, col, missing = NA, add.nodes = "MentionedAuthors", include.nodes = F) {
  
  user_attr_list <- df %>%
    select(user = Author, attr = !! sym(col)) 
  
  if (add.nodes != F) {
    # Add additional nodes that may be represented in the network without attrs
    add_user_list <- df %>%
      select(user = !! sym(add.nodes)) %>%
      separate_rows(user, sep = ", ") %>%                
      mutate(user = str_replace_all(user, "@", "")) %>%
      filter(!is.na(user)) %>%
      bind_cols(attr = missing)
    
    user_attr_list <- bind_rows(user_attr_list, add_user_list) %>%
      distinct(user, .keep_all = T) %>%
      mutate(attr = replace_na(attr, missing))
  }
  
  if (typeof(include.nodes) == "integer" && include.nodes != F) {
    user_attr_list <- user_attr_list %>%
      filter(user %in% names(include.nodes))
  }
  
  
  # Create an affiliation matrix connecting users based on a shared attribute
  user_attr_matrix <- user_attr_list %>%
    filter(user != "NA") %>%
    table() %>%
    unclass() %>%
    Matrix()
  
  # Multiply the affiliation matrix by its transpose to create a sociomatrix
  shared_attr_matrix <- user_attr_matrix %*% t(user_attr_matrix)
  return(shared_attr_matrix)
}

# matrix_gen_region(df) - Generate a sociomatrix based on shared region - alias
# for matrix_gen()
matrix_gen_region <- function (df) {
  return(matrix_gen(df, col = "Region", missing = "No Region"))
}

# matrix_gen_acctype(df) - Generate a sociomatrix based on shared account type -
# alias for matrix_gen()
matrix_gen_acctype <- function (df) { 
  return(matrix_gen(df, col = "AccountType", missing = "individual"))
}

# matrix_gen_gender(df) - Generate a sociomatrix based on shared gender - alias
# for matrix_gen()
matrix_gen_gender <- function (df) {
  return(matrix_gen(df, col = "Gender", missing = "unknown"))
}

# matrix_gen_verified(df) - Generate a sociomatrix based on shared verified
# status - alias for matrix_gen()
matrix_gen_verified <- function (df) {
  return(matrix_gen(df, col = "TwitterVerified", missing = "false"))
}

# matrix_gen_multi(df, col, sep, add.nodes) - Generate a sociomatrix based on a 
# generic shared attribute that may contain multiple values

# df - The data frame containing the original data in tabular format
# col - The column of the data frame containing the shared attribute
# sep - The character string separating values of the shared attribute
# add.nodes - Column containing additional nodes with no attribute data
# include.nodes - List of nodes (likely from a subgraph) to match the resulting
#                 sociomatrix to using `include.nodes = V(subgraph)`

matrix_gen_multi <- function (df, col, sep = ", ", add.nodes = "MentionedAuthors", include.nodes = F) {
  
  # Combine listed attributes from multiple tweets by the same user
  user_attr_list <- df %>%
    select(user = Author, attrs = !! sym(col)) %>%
    group_by(user) %>% mutate(attrs = paste(attrs, collapse = sep),
                              attrs = str_split(attrs, sep)) %>%
    rowwise() %>% mutate(attrs = list(unique(attrs)),
                         attrs = list(sort(attrs))) %>%
    distinct()
  
  if (add.nodes != F) {
    # Add additional nodes that may be represented in the network without attrs
    add_user_list <- df %>%
      select(user = !! sym(add.nodes)) %>%
      separate_rows(user, sep = ", ") %>%                
      mutate(user = str_replace_all(user, "@", "")) %>%
      filter(!is.na(user)) %>%
      bind_cols(attrs = NA)
    
    user_attr_list <- bind_rows(user_attr_list, add_user_list) %>%
      distinct(user, .keep_all = T) 
  }
  
  if (typeof(include.nodes) == "integer" && include.nodes != F) {
    user_attr_list <- user_attr_list %>%
      filter(user %in% names(include.nodes))
  }
  
  # Convert the nested list to an affiliation matrix
  attr_matrix <- user_attr_list %>%
    filter(user != "NA") %>%
    unnest(attrs, keep_empty = T) %>%
    table() %>%
    unclass() %>%
    Matrix()
  
  # Multiply the affiliation matrix by its transpose to create a sociomatrix
  shared_attr_matrix <- attr_matrix %*% t(attr_matrix)
  return(shared_attr_matrix)
}

# matrix_gen_htag(df) - Generate a sociomatrix based on shared hashtag usage -
# alias for matrix_gen_multi()
matrix_gen_htag <- function (df) {
  return(matrix_gen_multi(df, col = "Hashtags"))
}

# Generate a sociomatrix based on shared interests - alias for matrix_gen_multi()
matrix_gen_interests <- function (df) {
  return(matrix_gen_multi(df, col = "Interest"))
}

if (require(readxl) && require(writexl)) {
  
  # xlsx_from_keywords(file.in, file.out, pattern) - Generates a smaller sample
  # of an XLS or XLSX dataset based on keyword matching
  
  # file.in - Path of the input file in XLS or XLSX format
  # file.out - Path of the output file to be created
  # pattern - Grep pattern to match based on
  # match.col - Column to match pattern to (defaults to Full Text of tweets)

  xlsx_from_keywords <- function (file.in, file.out, pattern, match.col = "FullText") {
    df_original <- read_excel(path = file.in) %>%
      rename_all(~str_replace_all(., "\\s+", "")) # Remove whitespace from variable names
    
    df_sub <- df_original %>% 
      filter(str_detect(!! sym(match.col), pattern))
    
    write_xlsx(df_sub, path = file.out, col_names = T)
}}