


df_check <- function(df){
  flag = FALSE
  colNames <- colnames(df)
  if(is.element("Age", colNames) & is.element("Length", colNames)){
    if(is.numeric(df$Age) & is.numeric(df$Length)){ #values are numeric
     if(length(df$Length[is.na(df$Length)]) == 0){ #No nulls in Length 
      if(length(unique(df$Age)) >= 1 & (length(df$Age[is.na(df$Age)]) > 0) ){#Must be at least one Unique age
        flag = TRUE
      } 
     }
    }
  }
  
  return(flag)
}

boolean_check <- function(bool){
  flag = FALSE
  if ((is.logical(bool)) & (length(bool) == 1) & (is.atomic(bool))){
    flag = TRUE  
  }
  return(flag)
}

numeric_Check <- function (x, int_flag = TRUE, pos_flag = TRUE){
  flag = FALSE
  if ((is.numeric(x)) & (length(x) == 1) & (is.atomic(x))){
    if (((int_flag == TRUE) & (x%%1 == 0)) | (int_flag == FALSE)){
      if (((pos_flag == TRUE) & (x>=0)) | (pos_flag == FALSE)){
        flag = TRUE
      }
    }
  }
  return(flag)
}
