# Load required packages (if not already installed)
# install.packages("ggplot2")
library(ggplot2)

# Sample data
data <- data.frame(Condition = c("A", "B", "A", "A", "B", "B"),
                   Response = c("Correct", "Incorrect", "Correct", "Incorrect", "Correct", "Incorrect"))

# Create a bar plot
bar_plot <- ggplot(data, aes(x = Condition, fill = Response)) +
  geom_bar(position = "fill") +
  labs(x = "Condition", y = "Proportion", fill = "Response") +
  ggtitle("2AFC Task Results")

# Overlay data points
data_points <- geom_point(data = data, aes(x = Condition, y = 0.5, fill = Response),
                          position = position_jitter(width = 0.2, height = 0),
                          shape = 21, size = 3, color = "black")

# Combine bar plot and data points
combined_plot <- bar_plot + data_points

# Display the combined plot
combined_plot