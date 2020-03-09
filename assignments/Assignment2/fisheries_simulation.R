#' Fisheries Simulation
#' 
#' This function takes the inputs
#' - a table that has prices for different fish
#' - a table that has the number caught for each fish species for each location
#' and creates a data frame of containing
#' - most frequently caught fish in each location
#' - total revenue for each location
#' - total fisheries revenue sum
#' - plot of revenue by location and total revenue (optional, requires package ggplot2)
#' 
#' @param catch_location 
#' @param prices 
#' @param plot TRUE / FALSE to produce plot of revenue by location and total revenue
#' 
#' @author Laura Ingelsrud & Keene Morrow
#' 
#' @return dominantfishery
#' @return totalrevenue
#' @return revenuefishery
#' @return plot

# Create function that simulates fisheries revenue calculations
fisheries_simulation = function(catch_location, prices, plot = F) {
  # calculate the most frequently caught fish in each location
  dominantfish = list(colnames(catch_location), rownames(catch_location)[apply(catch_location, 2, which.max)])
  dominantfish_df = as.data.frame(dominantfish)
  names(dominantfish_df) = c("Location", "Most Frequent Fish Caught")
  # calculate the total revenue for each location
  revenue_location = sum(prices[,1]*catch_location)
  # calculate the total fisheries revenue sum
  revenue_fish = prices[,1]*catch_location
  revenue_fish = colSums(revenue_fish)
  revenue_fish_df = as.data.frame(revenue_fish)
  locations = c("Vermilion City", "Cerulean City", "Pallet Town", "Pewter City", "Cinnabar Island")
  revenue_fish_df$location = locations
  
  # if user requests it graph of revenue by location and total revenue (as text)
  if(plot) {
    require(ggplot2)
    p <- ggplot(revenue_fish_df, aes(location, revenue_fish, fill = location)) +
      geom_col() +
      labs(x = "Location", y = "Revenue (PD)", title = "Pokemon Fishing Revenue By Location", subtitle = sprintf("Total Revenue = $%d", revenue_location)) +
      theme_bw() +
      scale_fill_manual(values =c("salmon", "goldenrod2", "palegreen2", "aquamarine2", "cornflowerblue"))
  }
  else p = NULL
  
  return(list(dominantfishery = dominantfish_df, totalrevenue = revenue_location, revenuefishery = revenue_fish_df, plot = p))
  
}
