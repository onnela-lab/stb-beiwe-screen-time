# function to format numeric value to a character
# rounds depending on the value to be formatted (large numbers rounded to 0 decimal
# places, abs()<10 numbers rounded to 3 decimal places)
format_num_to_char <-  function(val){
  val_abs <- abs(val)
  if (val_abs < 10){
    val_f = sprintf("%.3f", val)
  } else if (val_abs >= 10 & val_abs < 100) {
    val_f = sprintf("%.2f", val)
  } else if (val_abs >= 100 & val_abs < 1000){
    val_f = sprintf("%.1f", val)
  } else {
    val_f = sprintf("%.0f", val)
  }
  return(val_f)
}


# https://stackoverflow.com/questions/72090177/how-can-i-know-whether-the-model-is-converged-or-failed-to-converge-in-lme4-with
# Reutrns: 
# - 1 if the model converged normally ie not to a singular fit, 
# - 0 if it converges to a singular fit 
# - (-1) if it fails to converge
merMod_has_converged <- function (mm) {
  if ( !inherits(mm, "merMod")) stop("Error: must pass a lmerMod object")
  retval <- NULL
  if(is.null(unlist(mm@optinfo$conv$lme4))) {
    retval = 1
  }
  else {
    if (isSingular(mm)) {
      retval = 0
    } else {
      retval = -1
    }
  }
  return(retval)
}