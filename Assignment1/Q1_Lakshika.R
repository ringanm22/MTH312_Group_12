library(dplyr)
library(ggplot2)
library(ddalpha)

iris_setosa <- iris[1:50, 0:4]
iris_virginica <- iris[51:100, 0:4]
iris_versicolor <- iris[101:150, 0:4]

#### ggplots before standardization

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                 group = Species, colour = Species)) + 
  geom_point() +
  scale_colour_manual(values = c("red", "purple", "deepskyblue")) + 
  labs(title = "Data Without Standardization") + 
  theme_test()

ggplot(iris, aes(x = Sepal.Length, y = Petal.Length,
                 group = Species, colour = Species)) + 
  geom_point() +
  scale_colour_manual(values = c("red", "purple", "deepskyblue")) + 
  labs(title = "Data Without Standardization") + 
  theme_test()

ggplot(iris, aes(x = Sepal.Width, y = Petal.Width,
                 group = Species, colour = Species)) + 
  geom_point() +
  scale_colour_manual(values = c("red", "purple", "deepskyblue")) + 
  labs(title = "Data Without Standardization") + 
  theme_test()

ggplot(iris, aes(x = Petal.Length, y = Petal.Width,
                 group = Species, colour = Species)) + 
  geom_point() +
  scale_colour_manual(values = c("red", "purple", "deepskyblue")) + 
  labs(title = "Data Without Standardization") + 
  theme_test()

dat <- rbind(iris_setosa, iris_virginica, iris_versicolor)

dsp <- as.data.frame(depth.space.halfspace(dat, c(50, 50, 50)))

#### dd plots without any standardization

dsp$species <- factor(rep(c("Setosa", "Virginica", "Versicolor"), each = 50))

colnames(dsp) <- c("Iris_Setosa", "Iris_Virginica", "Iris_Versicolor", "species")
ggplot(dsp[1:100, ]) + 
  geom_point(aes(x = Iris_Setosa, y = Iris_Virginica, color = species)) + 
  scale_color_manual(values = c("red", "purple")) + 
  labs(title = "DD plot of Virginica vs Setosa", x = "Depth w.r.t. Iris Setosa", y = "Depth w.r.t. Iris Virginica") +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.25) + 
  theme_test()

ggplot(dsp[c(1:50, 101:150), ]) + 
  geom_point(aes(x = Iris_Setosa, y = Iris_Versicolor, color = species)) + 
  scale_color_manual(values = c("red", "deepskyblue")) + 
  labs(title = "DD plot of Versicolor vs Setosa", x = "Depth w.r.t. Iris Setosa", y = "Depth w.r.t. Iris Versicolor") + 
  geom_abline(intercept = 0, slope = 1, linewidth = 0.25) + 
  theme_test()

ggplot(dsp[51:150, ]) + 
  geom_point(aes(x = Iris_Virginica, y = Iris_Versicolor, color = species)) + 
  scale_color_manual(values = c("purple", "deepskyblue")) + 
  labs(title = "DD plot of Versicolor vs Virginica", x = "Depth w.r.t. Iris Virginica", y = "Depth w.r.t. Iris Versicolor") +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.25) + 
  theme_test()


iris_setosa <- iris_setosa %>% mutate_all(~(scale(.) %>% as.vector))
iris_virginica  <- iris_virginica  %>% mutate_all(~(scale(.) %>% as.vector))
iris_versicolor <- iris_versicolor %>% mutate_all(~(scale(.) %>% as.vector))

df <- rbind(iris_setosa, iris_virginica, iris_versicolor)

Species <- iris[,5]
dfs <- cbind(df, Species)

#### ggplots after standardization

ggplot(dfs, aes(x = Sepal.Length, y = Sepal.Width,
                 group = Species, colour = Species)) + 
  geom_point() +
  scale_colour_manual(values = c("red", "purple", "deepskyblue")) + 
  labs(title = "Data With Standardization") + 
  theme_test()

ggplot(dfs, aes(x = Sepal.Length, y = Petal.Length,
                group = Species, colour = Species)) + 
  geom_point() +
  scale_colour_manual(values = c("red", "purple", "deepskyblue")) + 
  labs(title = "Data With Standardization") + 
  theme_test()

ggplot(dfs, aes(x = Sepal.Width, y = Petal.Width,
                group = Species, colour = Species)) + 
  geom_point() +
  scale_colour_manual(values = c("red", "purple", "deepskyblue")) + 
  labs(title = "Data With Standardization") + 
  theme_test()

ggplot(dfs, aes(x = Petal.Length, y = Petal.Width,
                 group = Species, colour = Species)) + 
  geom_point() +
  scale_colour_manual(values = c("red", "purple", "deepskyblue")) + 
  labs(title = "Data With Standardization") + 
  theme_test()


#### dd plots with standardization
dspace <- as.data.frame(depth.space.halfspace(df, c(50, 50, 50)))
dspace$species <- factor(rep(c("Setosa", "Virginica", "Versicolor"), each = 50))

colnames(dspace) <- c("Iris_Setosa", "Iris_Virginica", "Iris_Versicolor", "species")
ggplot(dspace[1:100, ]) + 
  geom_point(aes(x = Iris_Setosa, y = Iris_Virginica, color = species)) + 
  scale_color_manual(values = c("red", "purple")) + 
  labs(title = "DD plot of Virginica vs Setosa", x = "Depth w.r.t. Iris Setosa", y = "Depth w.r.t. Iris Virginica") +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.25) + 
  theme_test()

ggplot(dspace[c(1:50, 101:150), ]) + 
  geom_point(aes(x = Iris_Setosa, y = Iris_Versicolor, color = species)) + 
  scale_color_manual(values = c("red", "deepskyblue")) + 
  labs(title = "DD plot of Versicolor vs Setosa", x = "Depth w.r.t. Iris Setosa", y = "Depth w.r.t. Iris Versicolor") + 
  geom_abline(intercept = 0, slope = 1, linewidth = 0.25) + 
  theme_test()

ggplot(dspace[51:150, ]) + 
  geom_point(aes(x = Iris_Virginica, y = Iris_Versicolor, color = species)) + 
  scale_color_manual(values = c("purple", "deepskyblue")) + 
  labs(title = "DD plot of Versicolor vs Virginica", x = "Depth w.r.t. Iris Virginica", y = "Depth w.r.t. Iris Versicolor") +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.25) + 
  theme_test()


