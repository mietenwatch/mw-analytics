
# column names and values in character columns
# with specified encodings

convert_to_encoding <- 
  function(x, from_encoding = "UTF-8", to_encoding = "cp1250"){
    
    # names of columns are encoded in specified encoding
    my_names <- 
      iconv(names(x), from_encoding, to_encoding) 
    
    # if any column name is NA, leave the names
    # otherwise replace them with new names
    if(any(is.na(my_names))){
      names(x)
    } else {
      names(x) <- my_names
    }
    
    # get column classes
    x_char_columns <- sapply(x, class)
    # identify character columns
    x_cols <- names(x_char_columns[x_char_columns == "character"])
    
    # convert all string values in character columns to 
    # specified encoding
    x <- 
      x %>%
      mutate_each_(funs(iconv(., from_encoding, to_encoding)), 
                   x_cols)
    # return x
    return(x)
  }
