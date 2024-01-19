# Define the neutral model function, simulating the interactions within an ecological niche
# where species do not directly compete and the process is neutral
neutralModelCR <- function(turnOverRate, chanceOfExtremeEvent, mortalityRateNative, mortalityRateIntroduced, timeSteps, populationSize) {
  # Initialize the population matrix with native species occupying all sites
  Population <- matrix(0, nrow = populationSize, ncol = timeSteps)
  
  # Iterate over time steps to simulate the demographic processes
  for (t in 1:(timeSteps - 1)) {
    # Propagate the current state
    Population[, t + 1] <- Population[, t]
    
    # Apply natural turnover at each time step
    Population[, t + 1][runif(populationSize) < turnOverRate] <- 0.5
    
    # Determine mortality for each species from extreme events
    native <- Population[, t + 1] == 0
    introduced <- Population[, t + 1] == 1
    mortalityNative <- runif(populationSize) < mortalityRateNative * chanceOfExtremeEvent
    mortalityIntroduced <- runif(populationSize) < mortalityRateIntroduced * chanceOfExtremeEvent
    Population[, t + 1][native & mortalityNative] <- 0.5
    Population[, t + 1][introduced & mortalityIntroduced] <- 0.5
    
    # Recolonization of vacant sites
    availableSpaces <- Population[, t + 1] == 0.5
    Population[, t + 1][availableSpaces] <- as.numeric(runif(sum(availableSpaces)) < 0.5)
  }
  
  # Return the population matrix showing the state of each site over time
  return(Population)
}

# Set the parameters for the simulation
time_steps <- 1000
population_size <- 2^12

# Run the simulation for the three mortality scenarios
# Compute the mean state of the population for each time step
population_equal_mortality <- apply(neutralModelCR(0.001, 1/300, 0.5, 0.5, time_steps, population_size), 2, mean)
population_higher_mortality_native <- apply(neutralModelCR(0.001, 1/300, 0.7, 0.3, time_steps, population_size), 2, mean)
population_higher_mortality_introduced <- apply(neutralModelCR(0.001, 1/300, 0.3, 0.7, time_steps, population_size), 2, mean)

# Load the ggplot2 library for creating the plot
library(ggplot2)

# Create a data frame for plotting
scenarios <- data.frame(
  TimeStep = rep(1:time_steps, 3),
  Population = c(population_equal_mortality, population_higher_mortality_native, population_higher_mortality_introduced),
  Scenario = factor(rep(c("Equal Mortality", "Higher Mortality for Native", "Higher Mortality for Introduced"), each = time_steps))
)

# Use a colorblind-friendly palette for the plot
cb_palette <- c("#E69F00", "#56B4E9", "#009E73")

# Construct the plot with ggplot2, employing a minimalistic theme
p <- ggplot(scenarios, aes(x = TimeStep, y = Population, color = Scenario)) +
  geom_line(size = 1.5) +  # Increase line thickness for visibility
  scale_color_manual(values = cb_palette) +  # Apply the colorblind-friendly palette
  ylim(0,1) +
  theme_bw() +  # Use a clean background theme
  labs(title = "Species Composition Change Over Time",
       x = "Time Steps",
       y = "Dominance of Introduced Species",
       color = "Scenario") +
  theme(plot.title = element_blank(), #element_text(hjust = 0.5, face = "bold", size = 14),
        legend.title = element_text(face = "bold", size = 11),
        legend.text = element_text(face = "bold", size = 10),
        legend.position = "bottom",  # Position the legend at the bottom
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.ticks = element_line(size = 1.2),
        panel.grid.major = element_blank(),  # Remove grid lines for a clean look
        panel.grid.minor = element_blank())

# Save the plot to a file
ggsave("Neutral_Model_Species_Composition.png", plot = p, width = 8, height = 6, dpi = 300)

# Output the plot to the screen
print(p)
