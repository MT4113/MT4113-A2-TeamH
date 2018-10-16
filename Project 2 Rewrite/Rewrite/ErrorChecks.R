df_check <- function(df){
  
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
