#' Stream Power
#'
#' This function determines the power of a stream
#' 
#' @param Q stream discharge (m^3/s)
#' @param S channel slope
#' @param rho water density (g/m^3) default = 1000
#' @param g acceleration due to gravity (m/s^2) default = 9.8
#'
#' @return stream power (W)

# Create stream power function
stream_power = function(Q, S, rho=1000, g=9.8) {
  # if inputs are negative, return message "[input] must be greater than zero" using two different kinds of error checking 
  Q = ifelse((Q < 0), return("discharge must be greater than zero"), Q)
  S = ifelse((S < 0), return("channel slope must be greater than zero"), S)
  if (rho < 0) return("rho must be greater than zero")
  # calculate stream power
  result = rho * g * Q * S
  return(result)
}
