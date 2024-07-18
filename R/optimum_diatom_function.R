#############################################################
######## Function for calculate the optimum of diatoms ######
#############################################################


calculate_uk <- function(yik, xi) {
  # yik: vector of the relative abundance of species k in each sample
  # xi: vector of environmental parameter values in each sample

  # Check if the vectors have the same length
  if (length(yik) != length(xi)) {
    stop("The vectors yik and xi must have the same length.")
  }

  # Calculate the numerator: Σ(yik * xi)
  numerator <- sum(yik * xi)

  # Calculate the denominator: Σ(yik)
  denominator <- sum(yik)

  # Calculate uk
  uk <- numerator / denominator

  # Return the calculated value of uk
  return(uk)
}