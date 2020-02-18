# Create function that converts feet to meters
ft_to_m <- function(ft) {
	# make sure inputs are positive
	if (ft < 0) return (NA)
	# calculate conversion
	result = ft * 0.3048
	return(result)
}
