# Create function that converts feet to meters
ft_to_m <- function(ft) {
	# if input is negative, return "ft must be greater than 0"
	ft = ifelse((ft < 0), return("ft must be greater than zero"), ft)
	# calculate conversion
	result = ft * 0.3048
	return(result)
}
