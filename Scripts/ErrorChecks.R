
df_check <- function(df){
  # The purpose of this function is to trap and erroneous inputs where a 
  # dataframe input is required.
  # 
  # Arguments:
  #   df - the object we are testing
  # 
  # Outputs:
  #   Returns True if df is:
  #     - A dataframe with Length and Age columns named
  #     - Non-empty values in the Length column and that all values are of  
  #       numeric type
  #     - Numeric (non-factor) Age column with at least one unique non-null age and all ages  
  #       are greater or equal to 0. 
  #   Returns False otherwise.
  
  flag = FALSE
  colNames <- colnames(df) # Gets column names from dataframe
  # Tests if req column names exist
  if(is.element("Age", colNames) & is.element("Length", colNames)){
    # Test if columns/values in columns are numeric 
    if(is.numeric(df$Age) & is.numeric(df$Length)){ 
      # Ensures no NAs in Length column 
      if(length(df$Length[is.na(df$Length)]) == 0){  
        # Ensures of at least one unique age catagory 
        if(length(unique(df$Age)) >= 1 & (length(df$Age[is.na(df$Age)]) > 0) ){
          # Tests for negative values in ages
          tmp_ages <- unique(df$Age)
          if(length(tmp_ages) == length(tmp_ages[tmp_ages >= 0])){
            flag = TRUE
        }
      } 
     }
    }
  }
  
  return(flag)
}

boolean_check <- function(bool){
  # The purpose of this function is to trap erroneous inputs where a 
  # boolean is required
  # 
  # Arguments:
  #   bool - the object we are testing
  # 
  # Outputs:
  #   True if bool is a single value of boolean type. False otherwise.
  
  flag = FALSE
  if ((is.logical(bool)) & (length(bool) == 1) & (is.atomic(bool))){
    flag = TRUE  
  }
  return(flag)
}

numeric_Check <- function (x, int_flag = TRUE, pos_flag = TRUE){
  # The purpose of this function is to trap erroneous inputs where a 
  # numeric value is required
  # 
  # Arguments:
  #   x - the object we are testing
  #   int_flag - Boolean flag indicating if x needs to be checked to be an 
  #              integer
  #   pos_flag - Boolean flag indicating if x needs to be checked to be an 
  #              positive number
  # Outputs:
  #   True if x is a single value of numeric type and satisfies the integer 
  #   and positive number conditions if specified. False otherwise.
  
  flag = FALSE
  
  # Check for x to be a single numeric object 
  if ((is.numeric(x)) & (length(x) == 1) & (is.atomic(x))){
    # Check if x is an integer (if needed)
    if (((int_flag == TRUE) & (x%%1 == 0)) | (int_flag == FALSE)){
      # Check if x is positive (if needed)
      if (((pos_flag == TRUE) & (x>=0)) | (pos_flag == FALSE)){
        flag = TRUE
      }
    }
  }
  return(flag)
}
