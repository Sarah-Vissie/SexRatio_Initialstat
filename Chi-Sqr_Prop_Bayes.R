library(pacman)
p_load(tidyverse, pwr, spatstat, sf)


###BB Site 1 - 179(M) 141(F)
###BB Site 2 - 188(M) 146(F)
###Grootbos - honsfieldcamp - 192(M) 117(F)


##Analysis of BB Site 1

n <- 179+141 #BB path above with spatial patterns


observed <- c(179, 141)

observed/n

# Expected counts (for a fair coin)
expected <- c(n / 2, n / 2)

# Perform the Chi-Square Test
chi_square_test <- chisq.test(observed, p = c(0.5, 0.5), simulate.p.value = TRUE, B=100000)

# Print the results
print(chi_square_test)

help(prop.test)
prop.test(x = 179, n = n, alternative = 'two.sided')

# Calculate total count and expected counts under a 1:1 ratio
total_count <- 179+141
expected_male <- total_count / 2
expected_female <- total_count / 2

# Create a contingency table
observed_counts <- c(179, 141)
expected_counts <- c(expected_male, expected_female)
contingency_table <- matrix(c(observed_counts, expected_counts), nrow=2, byrow=TRUE)
rownames(contingency_table) <- c("Observed", "Expected")
colnames(contingency_table) <- c("Male", "Female")

# Perform Fisher's exact test
fisher_result <- fisher.test(contingency_table)

# Output the result
print(fisher_result)

p_load(DescTools)
# Perform the G-test with Yates's correction
g_test_result <- GTest(contingency_table, correct="yates")

# Perform Williams' adjusted G-test
williams_adjusted_g <- GTest(contingency_table, correct="williams")

##Analysis of BB Site 2

# Number of coin flips
n <- 188+146 #BB nature reserve with basal diamter

observed <- c(188, 146)

# Expected counts (for a fair coin)
expected <- c(n / 2, n / 2)

# Perform the Chi-Square Test
chi_square_test <- chisq.test(observed, p = c(0.5, 0.5))
help(chisq.test)
# Print the results
print(chi_square_test)


prop.test(x = 188, n = n, alternative = 'two.sided')


#Two sites/species together
data <- data.frame(
  location = c("A", "B"),   # Locations
  male = c(179, 188),           # Number of males at each location
  female = c(141, 146)          # Number of females at each location
)

data <- data.frame(
  location = c("A", "B", "C", "D"),   # Locations
  male = c(240, 240, 250, 255),           # Number of males at each location
  female = c(15, 25, 25, 30)          # Number of females at each location
)

# Create a contingency table
contingency_table <- as.table(rbind(data$male, data$female))
colnames(contingency_table) <- data$location
rownames(contingency_table) <- c("Male", "Female")

# Perform Fisher's exact test
fisher_result <- fisher.test(contingency_table)

##Grootbos - honsfieldcamp - 192(M) 117(F)

n <- 192+117 #Two populations combine - not correct but just for this example


observed <- c(192, 117)

observed/n

# Expected counts (for a fair coin)
expected <- c(n / 2, n / 2)

# Perform the Chi-Square Test
chi_square_test <- chisq.test(observed, p = c(0.5, 0.5), simulate.p.value = TRUE, B=100000)

# Print the results
print(chi_square_test)

help(prop.test)
prop.test(x = 192, n = n, alternative = 'two.sided')

# Calculate total count and expected counts under a 1:1 ratio
total_count <- 192+117
expected_male <- total_count / 2
expected_female <- total_count / 2

# Create a contingency table
observed_counts <- c(192, 117)
expected_counts <- c(expected_male, expected_female)
contingency_table <- matrix(c(observed_counts, expected_counts), nrow=2, byrow=TRUE)
rownames(contingency_table) <- c("Observed", "Expected")
colnames(contingency_table) <- c("Male", "Female")

# Perform Fisher's exact test
fisher_result <- fisher.test(contingency_table)

# Output the result
print(fisher_result)

p_load(DescTools)
# Perform the G-test with Yates's correction
g_test_result <- GTest(contingency_table, correct="yates")
print(g_test_result)

# Perform Williams' adjusted G-test
williams_adjusted_g <- GTest(contingency_table, correct="williams")
print(williams_adjusted_g)




####Bayesian analysis with flat prior
# Sample data
n <- 334  # total number of plants
x <- 188  # number of males

# Prior parameters for Beta distribution (flat prior)
alpha_prior <- 1
beta_prior <- 1

# Posterior parameters
alpha_posterior <- alpha_prior + x
beta_posterior <- beta_prior + (n - x)

# Create a sequence of values from 0 to 1 to evaluate the Beta distributions
theta <- seq(0, 1, length.out = 1000)

# Posterior mean
posterior_mean <- alpha_posterior / (alpha_posterior + beta_posterior)

# Posterior credible interval (e.g., 95% credible interval)
credible_interval <- qbeta(c(0.025, 0.975), alpha_posterior, beta_posterior)

# Print results
cat("Posterior mean:", posterior_mean, "\n")
cat("95% credible interval:", credible_interval, "\n")

# Calculate the density values for the prior and posterior distributions
prior_density <- dbeta(theta, alpha_prior, beta_prior)
posterior_density <- dbeta(theta, alpha_posterior, beta_posterior)

# Create a data frame for plotting
data <- data.frame(
  theta = rep(theta, 2),
  density = c(prior_density, posterior_density),
  distribution = rep(c("Prior", "Posterior"), each = length(theta))
)

# Plot the prior and posterior distributions using ggplot2
ggplot(data, aes(x = theta, y = density, color = distribution)) +
  geom_line(size = 1) +
  labs(title = "Prior and Posterior Distributions",
       x = "Sex Ratio (Proportion of Males)",
       y = "Density",
       color = "Distribution") +
  theme_minimal() +
  theme(legend.position = "top")

#Bayesian analysis with informative prior i.e. 50:50 ratio

# Prior parameters for Beta distribution (informative prior reflecting belief in 0.5 ratio)
alpha_prior <- 70
beta_prior <- 30

# Posterior parameters
alpha_posterior <- alpha_prior + x
beta_posterior <- beta_prior + (n - x)

# Posterior mean
posterior_mean <- alpha_posterior / (alpha_posterior + beta_posterior)

# Posterior credible interval (e.g., 95% credible interval)
credible_interval <- qbeta(c(0.025, 0.975), alpha_posterior, beta_posterior)

# Print results
cat("Posterior mean:", posterior_mean, "\n")
cat("95% credible interval:", credible_interval, "\n")

# Create a sequence of values from 0 to 1 to evaluate the Beta distributions
theta <- seq(0, 1, length.out = 1000)

# Calculate the density values for the prior and posterior distributions
prior_density <- dbeta(theta, alpha_prior, beta_prior)
posterior_density <- dbeta(theta, alpha_posterior, beta_posterior)

# Create a data frame for plotting
data <- data.frame(
  theta = rep(theta, 2),
  density = c(prior_density, posterior_density),
  distribution = rep(c("Prior", "Posterior"), each = length(theta))
)

# Plot the prior and posterior distributions using ggplot2
ggplot(data, aes(x = theta, y = density, color = distribution)) +
  geom_line(size = 1) +
  labs(title = "Prior and Posterior Distributions",
       x = "Sex Ratio (Proportion of Males)",
       y = "Density",
       color = "Distribution") +
  theme_minimal() +
  theme(legend.position = "top")


#Frequentist power analysis
# Define the effect sizes for the different sex ratios
sex_ratios <- c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8)
p0 <- 0.5  # Null hypothesis proportion

# Calculate the effect sizes (Cohen's h)
effect_sizes <- abs(2 * asin(sqrt(sex_ratios)) - 2 * asin(sqrt(p0)))

# Desired power and significance level
desired_power <- 0.8
alpha <- 0.05

# Calculate the required sample sizes
sample_sizes <- sapply(effect_sizes, function(h) {
  pwr.p.test(h = h, sig.level = alpha, power = desired_power, alternative = "two.sided")$n
})

# Create a data frame to display the results
results <- data.frame(
  Sex_Ratio = sex_ratios,
  Effect_Size = effect_sizes,
  Sample_Size = ceiling(sample_sizes)  # Round up to the next whole number
)

# Print the results
print(results)

#Bayesian power analysis
# Function to simulate data and calculate the posterior
simulate_posterior <- function(n, sex_ratio, alpha_prior, beta_prior) {
  x <- round(n * sex_ratio)  # Number of males
  alpha_posterior <- alpha_prior + x
  beta_posterior <- beta_prior + (n - x)
  credible_interval <- qbeta(c(0.025, 0.975), alpha_posterior, beta_posterior)
  list(
    mean = alpha_posterior / (alpha_posterior + beta_posterior),
    lower = credible_interval[1],
    upper = credible_interval[2]
  )
}

# Function to determine the required sample size
determine_sample_size <- function(sex_ratio, alpha_prior, beta_prior, threshold = 0.5) {
  n <- 1  # Start with a small sample size
  while (TRUE) {
    posterior <- simulate_posterior(n, sex_ratio, alpha_prior, beta_prior)
    if (posterior$lower > threshold || posterior$upper < threshold) {
      return(n)
    }
    n <- n + 1
  }
}

# Prior parameters for Beta distribution (informative prior reflecting belief in 0.5 ratio)
alpha_prior <- 50
beta_prior <- 50

# Different sex ratios to test
sex_ratios <- c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8)

# Determine the required sample size for each sex ratio
required_sample_sizes <- sapply(sex_ratios, function(sex_ratio) {
  determine_sample_size(sex_ratio, alpha_prior, beta_prior)
})

# Create a data frame to display the results
results <- data.frame(
  Sex_Ratio = sex_ratios,
  Sample_Size = required_sample_sizes
)

# Print the results
print(results)


#Let's visualize the Beta distributions with different α and β values to see how they represent prior beliefs

# Sequence of values from 0 to 1
theta <- seq(0, 1, length.out = 1000)

# Prior densities
beta_1_1 <- dbeta(theta, 1, 1)
beta_5_5 <- dbeta(theta, 5, 5)
beta_10_10 <- dbeta(theta, 10, 10)
beta_25_25 <- dbeta(theta, 25, 25)
beta_50_50 <- dbeta(theta, 50, 50)

# Create a data frame for plotting
data <- data.frame(
  theta = rep(theta, 5),
  density = c(beta_1_1, beta_5_5, beta_10_10, beta_25_25, beta_50_50),
  prior = rep(c("Beta(1,1)", "Beta(5,5)","Beta(10,10)","Beta(25,25)","Beta(50,50)"), each = length(theta))
)

# Plot the Beta distributions
ggplot(data, aes(x = theta, y = density, color = prior)) +
  geom_line(size = 1) +
  labs(title = "Beta Distributions",
       x = "Sex Ratio (Proportion of Males)",
       y = "Density",
       color = "Prior") +
  theme_minimal() +
  theme(legend.position = "top")

#Do spatial analysis on transect with GPS points.
transect <- read.csv("/Users/michaelcramer/Library/CloudStorage/Dropbox/Sarah_Visser/BB_SexLocation_07.06.csv")

#Convert NA in Male and NA in Female to 0
transect$Male[is.na(transect$Male)] <- 0
transect$Female[is.na(transect$Female)] <- 0

#Convert to spatial data
transect_sf <- st_as_sf(transect, coords = c("lon", "lat"), crs = 4326)

#Create a spatial_lines object from the unique points in the transect following the order of the Waypoint number column with just one line between each subsequent point
transect_lines <- st_sfc(st_linestring(st_coordinates(transect_sf)))
plot(transect_lines)

#Calculate the distance along the transect between each successive point in meters using st_distance
transect_sf$Distance_m <- c(0, st_distance(transect_sf[-1,],transect_sf[-nrow(transect_sf),], by_element=TRUE))

#Calculate cummalitve distances from Distance_m
transect_sf$Cumulative_distance_m <- cumsum(transect_sf$Distance_m)

transect_long_sf <- pivot_longer(transect_sf, cols = c(Male, Female), values_to = "Count", names_to = "MF")

transect_longer_sf <- transect_long_sf %>% uncount(weights = Count) 

distances <- transect_long_sf$Distance_m
MF <- factor(transect_long_sf$MF)

#Assuming a simple straight line segment from min to max distance
vertices <- ppp(x = distances, y = rep(0, length(distances)), window = owin(xrange=c(min(distances), max(distances)), yrange=c(0,0)))
#Create a matric of two columns with all successive pairs of distances
edges <- cbind(1:(length(distances)-1), 2:length(distances))
L <- linnet(vertices, edges = edges)

#Create a point pattern on the linear network (lpp) object
pp_lpp <- lpp(vertices, L = L)

# Add the number of males or females as marks to the lpp object
marks(pp_lpp) <- MF

plot(pp_lpp, show.vertices=TRUE, use.marks=TRUE, legend=TRUE)

#plot(pp_lpp, show.vertices=TRUE, use.marks=TRUE, legend=TRUE)
help(linearKcross)

#Calculate Ripley's K function for linear networks
K_lpp <- envelope(pp_lpp, linearKcross, i="Male", j = "Female", correction = "none")

# Plot Ripley's K function
print(plot(K_lpp, main=paste0("Ripley's K Function for Linear Network"), xlab="Distance", ylab="K(d)", las=1))

#This plot shows that males and females are spatially clustered along the transect. The observed K(d) values are higher than the expected values, indicating clustering.
