# Create stream power function
stream_power = function(Q, S, rho=1000, g=9.8) {
  # if inputs are negative, return message "[input] must be greater than zero"
  Q = ifelse((Q < 0), return("discharge must be greater than zero"), Q)
  S = ifelse((S < 0), return("channel slope must be greater than zero"), S)
  rho = ifelse((rho < 0), return ("rho must be greater than zero"), rho)
  # calculate stream power
  result = rho * g * Q * S
  return(result)
}
