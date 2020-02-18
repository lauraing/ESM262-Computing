# Create power generation function
power_gen = function(height, flow, rho=1000, g=9.8, Keff=0.8) {
	# make sure inputs are positive
	if (height < 0) return (NA)
	if (flow < 0) return (NA)
	if (rho < 0) return (NA)
	# calculate power
	result = rho * height * flow * g * Keff
	return(result)
	}