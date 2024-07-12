library(pacman)
p_load(tidyverse, pwr, spatstat, sf, DescTools)

# Load data

cdata <- read.csv("C:/Users/VSSSAR003/OneDrive - University of Cape Town/Documents/HonsProj_SexRatios/R - Stats/Comb_InData.csv")
names(cdata)

cdata$Pop <- cdata$M + cdata$F


#cdata <- cdata %>% group_by(Site, Species, Age) %>% summarise(M = sum(M), F = sum(F), Pop = sum(Pop), Lat = mean(Lat), Lon = mean(Lon))

#cdata$Sub.site <- 1

stat_result_df <- data.frame(Test=character(), Site=character(), Sub_site = character(), Lat = numeric(), Lon = numeric(), Species=character(), Age = character(), stat = numeric(), p_value = numeric(), sex_ratio=numeric())


i<- 2
for (i in 1:nrow(cdata)){
  
n <- cdata$Pop[i]
observed <- c(cdata$M[i], cdata$F[i])

observed/n

# Expected counts (for a fair coin)
expected <- c(n / 2, n / 2)

# Perform the Chi-Square Test
chi_square_test <- chisq.test(observed, p = c(0.5, 0.5), simulate.p.value = TRUE, B=10000)

stat_result_df <- rbind(stat_result_df, data.frame(Test="chi_squared", Site= cdata$Site[i], Sub_site = cdata$Sub.site[i], Lat = cdata$Lat[i], Lon = cdata$Lon[i], Species=cdata$Species[1], Age = cdata$Age[i], stat = chi_square_test$statistic, p_value = chi_square_test$p.value, sex_ratio=cdata$M[i]/n))


prop_test <- prop.test(x = observed[1], n = n, alternative = 'two.sided')

stat_result_df <- rbind(stat_result_df, data.frame(Test="prop", Site= cdata$Site[i], Sub_site = cdata$Sub.site[i], Lat = cdata$Lat[i], Lon = cdata$Lon[i], Species=cdata$Species[1], Age = cdata$Age[i], stat = prop_test$statistic, p_value = prop_test$p.value, sex_ratio=cdata$M[i]/n))


contingency_table <- matrix(c(observed, expected), nrow=2, byrow=TRUE)
rownames(contingency_table) <- c("Observed", "Expected")
colnames(contingency_table) <- c("Male", "Female")

# Perform Fisher's exact test
fisher_result <- fisher.test(contingency_table)

stat_result_df <- rbind(stat_result_df, data.frame(Test="Fisher", Site= cdata$Site[i], Sub_site = cdata$Sub.site[i], Lat = cdata$Lat[i], Lon = cdata$Lon[i], Species=cdata$Species[1], Age = cdata$Age[i], stat = fisher_result$estimate, p_value = fisher_result$p.value, sex_ratio=cdata$M[i]/n))


# Perform Williams' adjusted G-test
williams_adjusted_g <- GTest(contingency_table, correct="williams")

stat_result_df <- rbind(stat_result_df, data.frame(Test="G_test", Site= cdata$Site[i], Sub_site = cdata$Sub.site[i], Lat = cdata$Lat[i], Lon = cdata$Lon[i], Species=cdata$Species[1], Age = cdata$Age[i], stat = williams_adjusted_g$statistic, p_value = williams_adjusted_g$p.value, sex_ratio=cdata$M[i]/n))
                        
                 
}

stat_result_df

