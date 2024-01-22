library(ddalpha)
library(dplyr)

# Load iris dataset
df <- iris

# Subset the data for setosa, versicolor, and virginica
seto <- subset(x = df, subset = (df$Species == "setosa"))
vers <- subset(x = df, subset = (df$Species == "versicolor"))
virg <- subset(x = df, subset = (df$Species == "virginica"))

# Function to calculate dd plot
calculate_dd_plot <- function(X, Y, title_text) {
  # Convert to matrices
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  
  # Combine X and Y
  Z <- rbind(X, Y)
  
  # Calculate distance using half depth
  Dx <- depth.halfspace(x = Z, data = X)
  Dy <- depth.halfspace(x = Z, data = Y)
  
  # Plot dd plot
  plot(x = Dx, y = Dy, main = title_text,pch = 20)
  abline(a = 0, b = 1,col="blue")
}

# Compare setosa vs versicolor without standardization
X_seto <- as.matrix(seto[, c(1, 2, 3, 4)])
Y_vers <- as.matrix(vers[, c(1, 2, 3, 4)])
calculate_dd_plot(X_seto, Y_vers, "Versicolor vs Setosa (Without Standardization)")

# Compare setosa vs versicolor with standardization
X_seto <- as.data.frame(X_seto) %>% mutate_all(~(scale(.) %>% as.vector))
Y_vers <- as.data.frame(Y_vers) %>% mutate_all(~(scale(.) %>% as.vector))
calculate_dd_plot(X_seto, Y_vers, "Versicolor vs Setosa (With Standardization)")

# Compare setosa vs virginica without standardization
X_seto <- as.matrix(seto[, c(1, 2, 3, 4)])
Y_virg <- as.matrix(virg[, c(1, 2, 3, 4)])
calculate_dd_plot(X_seto, Y_virg, "Virginica vs Setosa (Without Standardization)")

# Compare setosa vs virginica with standardization
X_seto <- as.data.frame(X_seto) %>% mutate_all(~(scale(.) %>% as.vector))
Y_virg <- as.data.frame(Y_virg) %>% mutate_all(~(scale(.) %>% as.vector))
calculate_dd_plot(X_seto, Y_virg, "Virginica vs Setosa (With Standardization)")

# Compare virginica vs versicolor without standardization
X_virg <- as.matrix(virg[, c(1, 2, 3, 4)])
Y_vers <- as.matrix(vers[, c(1, 2, 3, 4)])
calculate_dd_plot(X_virg, Y_vers, "Versicolor vs Virginica (Without Standardization)")

# Compare virginica vs versicolor with standardization
X_virg <- as.data.frame(X_virg) %>% mutate_all(~(scale(.) %>% as.vector))
Y_vers <- as.data.frame(Y_vers) %>% mutate_all(~(scale(.) %>% as.vector))
calculate_dd_plot(X_virg, Y_vers, "Versicolor vs Virginica (With Standardization)")
