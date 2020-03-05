# Create power generation function
power_gen = function(height, flow, rho=1000, g=9.8, Keff=0.8) {
	# if inputs are negative, return message "[input] must be greater than zero"
  height = ifelse((height < 0), return("height must be greater than zero"), height)
  flow = ifelse((flow < 0), return("flow must be greater than zero"), flow)
  rho = ifelse((rho < 0), return ("rho must be greater than zero"), rho)
	# calculate power
	result = rho * height * flow * g * Keff
	return(result)
}