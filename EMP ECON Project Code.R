################################################################################
# EMP ECON Project Code - TEAM 34
################################################################################


rm(list=ls()) # clear workspace
cat("\014")  # clear console
graphics.off() # shut down all open graphics devices 


# Load required libraries
library(readxl)
library(stargazer)
library(ggplot2)

# Load the data
walkability_data <- read_excel("walkability_data.xlsx")
View(walkability_data)
summary(walkability_data)

################################################################################
# DATA CLEANING
################################################################################

# Most of the data cleaning occurred during the data gathering in Excel
# The data was gathered and compiled from four different sources, using city as the reference point:
      # Walkability index data obtained from WalkScore.com, updated in 2020
      # '500 Cities: Local Data for Better Health' obtained from the 2020 release from the U.S. CDC 
      # U.S. city demographics data obtained from the U.S. Census Bureau's 2016 American Community Survey
      # 2020 City Income Data obtained from U.S. IndexMundi  

# One key assumption we are making here is that the city's infrastructure / walkability index 
# did not undergo any major changes over the 4-year period from 2016 - 2020 (from the data compiling)

# factoring the region and state variables
walkability_data$region <- factor(walkability_data$region)
walkability_data$state <- factor(walkability_data$state)

# Changing transist score from character to numeric
walkability_data$transit_score <- as.numeric(walkability_data$transit_score)
# Replace NAs with the average
walkability_data$transit_score[is.na(walkability_data$transit_score)] <- mean(walkability_data$transit_score, na.rm = TRUE)

################################################################################
# EXPLORATORY DATA ANALYSIS
################################################################################



##### Relationships between independent variables and obesity (dependent variable of interest) #####

### Three main data visualizations ###


# Walkability Index vs Obesity plot
ggplot(walkability_data, aes(x=walk_score, y=obesity_perc)) + geom_point(aes(col=region, size=total_pop)) +
  geom_smooth(method="lm", se=FALSE, col="brown") + scale_colour_brewer(palette = "Spectral") +
  labs(title="Walkability vs. Obesity", subtitle="Urban walkability scores plotted against percentage of obese residents",
       y="Obesity", x="Walkability Index", caption="U.S. Metropolitan Statistical Areas") + theme_bw() + labs(color="Region", size="Population")

# Obesity density plot by region
ggplot(walkability_data, aes(x=obesity_perc, color = region, fill = region)) + 
  geom_density(alpha=0.4) + scale_colour_brewer(palette = "Set1") +
  labs(x = "Obesity Percent", y = "Density", title = "Regional Obesity Levels",
       subtitle="The percentage of obese residents by region", caption="U.S. Metropolitan Statistical Areas") +
  theme_bw()

# Median Household Income vs Obesity plot
ggplot(walkability_data, aes(x=median_hh_income, y=obesity_perc)) + geom_point(aes(col=region, size=total_pop)) +
  geom_smooth(method="lm", se=FALSE, col="brown") + scale_colour_brewer(palette = "Spectral") +
  labs(title="Household Income vs. Obesity", subtitle="Median household income plotted against percentage of obese residents",
       y="Obesity", x="Median Household Income", caption="U.S. Metropolitan Statistical Areas") + theme_bw() + labs(color="Region", size="Population")



### extra visualizations that helped us understand the data ###

# Bike Index vs Obesity plot
ggplot(walkability_data, aes(x=bike_score, y=obesity_perc)) + geom_point(aes(col=region, size=total_pop)) +
  geom_smooth(method="lm", se=FALSE, col="brown") + scale_colour_brewer(palette = "Spectral") +
  labs(title="Bike-ability vs. Obesity", subtitle="Urban bike-friendliness scores plotted against percentage of obese residents",
       y="Obesity", x="Bike Index", caption="U.S. Metropolitan Statistical Areas") + theme_bw() + labs(color="Region", size="Population")

# No Self-Reported Exercise vs. Obesity plot
ggplot(walkability_data, aes(x=no_exercise_perc, y=obesity_perc)) + geom_point(aes(col=region, size=total_pop)) +
  geom_smooth(method="lm", se=FALSE, col="brown") + scale_colour_brewer(palette = "Spectral") +
  labs(title="No Self-Reported Exercise vs. Obesity", subtitle="Percentage of residents who say they don't exercise plotted against percentage of obese residents",
       y="Obesity", x="No Self-Reported Exercise", caption="U.S. Metropolitan Statistical Areas") + theme_bw() + labs(color="Region", size="Population")


# Average obesity by region bar graph
west.obesity <- mean(walkability_data$obesity_perc[walkability_data$region == "west"])
southwest.obesity <-  mean(walkability_data$obesity_perc[walkability_data$region == "southwest"])
northeast.obesity <-  mean(walkability_data$obesity_perc[walkability_data$region == "northeast"])
southeast.obesity <-  mean(walkability_data$obesity_perc[walkability_data$region == "southeast"])
midwest.obesity <-  mean(walkability_data$obesity_perc[walkability_data$region == "midwest"])
region.averages <- c(west.obesity, southwest.obesity, northeast.obesity, southeast.obesity, midwest.obesity)
regions <- c("west", "southwest", "northeast", "southeast", "midwest")
region.obesity <- data.frame(region=regions, obesity=region.averages)

ggplot(data=region.obesity, aes(x=region, y=obesity)) + scale_colour_brewer(palette = "Spectral") +
  geom_bar(stat="identity") + labs(title="Obesity by Region", subtitle="Average percentage of residents who are obese by U.S. region",
                                   y="Obesity", x="Region", caption="U.S. Metropolitan Statistical Areas") + theme_minimal()


# White vs. Obesity plot
ggplot(walkability_data, aes(x=white/total_pop, y=obesity_perc)) + geom_point(aes(col=region, size=total_pop)) +
  scale_colour_brewer(palette = "Spectral") +
  labs(title="White vs. Obesity", subtitle="Percentage of white residents plotted against percentage of obese residents",
       y="Obesity", x="White", caption="U.S. Metropolitan Statistical Areas") + theme_bw() + labs(color="Region", size="Population")

# Black vs. Obesity plot
ggplot(walkability_data, aes(x=black/total_pop, y=obesity_perc)) + geom_point(aes(col=region, size=total_pop)) +
  scale_colour_brewer(palette = "Spectral") +
  labs(title="Black vs. Obesity", subtitle="Percentage of black residents plotted against percentage of obese residents",
       y="Obesity", x="Black", caption="U.S. Metropolitan Statistical Areas") + theme_bw() + labs(color="Region", size="Population")

# Hispanic/Latino vs. Obesity plot
ggplot(walkability_data, aes(x=hispanic_latino/total_pop, y=obesity_perc)) + geom_point(aes(col=region, size=total_pop)) +
  scale_colour_brewer(palette = "Spectral") +
  labs(title="Hispanic/Latino vs. Obesity", subtitle="Percentage of Hispanic/Latino residents plotted against percentage of obese residents",
       y="Obesity", x="Hispanic/Latino", caption="U.S. Metropolitan Statistical Areas") + theme_bw() + labs(color="Region", size="Population")

# Asian vs. Obesity plot
ggplot(walkability_data, aes(x=asian/total_pop, y=obesity_perc)) + geom_point(aes(col=region, size=total_pop)) +
  scale_colour_brewer(palette = "Spectral") +
  labs(title="Asian vs. Obesity", subtitle="Percentage of Asian residents plotted against percentage of obese residents",
       y="Obesity", x="Asian", caption="U.S. Metropolitan Statistical Areas") + theme_bw() + labs(color="Region", size="Population")

# Median Age vs. Obesity plot
ggplot(walkability_data, aes(x=median_age, y=obesity_perc)) + geom_point(aes(col=region, size=total_pop)) +
  scale_colour_brewer(palette = "Spectral") +
  labs(title="Median Age vs. Obesity", subtitle="Median age of residents plotted against percentage of obese residents",
       y="Obesity", x="Median Age", caption="U.S. Metropolitan Statistical Areas") + theme_bw() + labs(color="Region", size="Population")

# Female Proportion vs Obesity plot
ggplot(walkability_data, aes(x=female_pop/total_pop, y=obesity_perc)) + geom_point(aes(col=region, size=total_pop)) +
  scale_colour_brewer(palette = "Spectral") + geom_smooth(method="lm", se=FALSE, col="brown") +
  labs(title="Proportion Female vs. Obesity", subtitle="Proportion of female residents plotted against percentage of obese residents",
       y="Obesity", x="Proportion Female", caption="U.S. Metropolitan Statistical Areas") + theme_bw() + labs(color="Region", size="Population")

# Male Proportion vs Obesity plot
ggplot(walkability_data, aes(x=male_pop/total_pop, y=obesity_perc)) + geom_point(aes(col=region, size=total_pop)) +
  scale_colour_brewer(palette = "Spectral") + geom_smooth(method="lm", se=FALSE, col="brown") +
  labs(title="Proportion Male vs. Obesity", subtitle="Proportion of male residents plotted against percentage of obese residents",
       y="Obesity", x="Proportion Male", caption="U.S. Metropolitan Statistical Areas") + theme_bw() + labs(color="Region", size="Population")




##### Relationships among independent variables #####
### (can potentially help to determine relevant interaction terms)
### all of these are also "extra" visualizations to help us understand the data


# Walkability Index vs. Bike Index plot
ggplot(walkability_data, aes(x=walk_score, y=bike_score)) + geom_point(aes(col=region, size=total_pop)) +
  geom_smooth(method="lm", se=FALSE, col="brown") + scale_colour_brewer(palette = "Spectral") +
  labs(title="Walkability vs. Bike-ability", subtitle="Urban walkability scores plotted against bike-friendliness scores",
       y="Bike Score", x="Walk Score", caption="U.S. Metropolitan Statistical Areas") + theme_bw() + labs(color="Region", size="Population")


# Transit Index vs Walkability plot
ggplot(walkability_data, aes(x=transit_score, y=walk_score)) + geom_point(aes(col=region, size=total_pop)) +
  scale_colour_brewer(palette = "Spectral") + stat_smooth(method = "loess", se=FALSE, col="brown") +
  labs(title="Public Transit vs. Walkability", subtitle="Urban public transit scores plotted against walkability index",
       y="Walkability", x="Public Transit Index", caption="U.S. Metropolitan Statistical Areas") + theme_bw() + labs(color="Region", size="Population")

# Median Age vs. Median Income
ggplot(walkability_data, aes(x=median_age, y=median_hh_income)) + geom_point(aes(col=region, size=total_pop)) +
  geom_smooth(method="lm", se=FALSE, col="brown") + scale_colour_brewer(palette = "Spectral") +
  labs(title="Median Age vs. Median Household Income", subtitle="Median age plotted against median household income",
       y="Median Household Income", x="Median Age", caption="U.S. Metropolitan Statistical Areas") + theme_bw() + labs(color="Region", size="Population")

# Median Age vs No Exercise Percent
ggplot(walkability_data, aes(x=median_age, y=no_exercise_perc)) + geom_point(aes(col=region, size=total_pop)) +
  scale_colour_brewer(palette = "Spectral") +
  labs(title="Median Age vs. No Exercise Percent", subtitle="Median age plotted against self-reported no exercise percent",
       y="No Exercise Percent", x="Median Age", caption="U.S. Metropolitan Statistical Areas") + theme_bw() + labs(color="Region", size="Population")

################################################################################
# QUANTITATIVE ANALYSIS
################################################################################


# 
# STEP ONE 
#
### Clustering with respect to treatment (high walk score) 
### Use median to cluster the cities
### Match cities with high walkability and low walkability (but similar demographics) 
### Look at the differences across groups
#

# Separating into 4 groups based on walk and obesity

plot(walkability_data$walk_score, walkability_data$obesity_perc)

median(walkability_data$walk_score)
# 43.2

walkH <- walkability_data[which(walkability_data$walk_score >= 43.2),]
nrow(walkH)

walkL <- walkability_data[which(walkability_data$walk_score < 43.2),]
nrow(walkL)

median(walkH$obesity_perc)
# 30.2
median(walkL$obesity_perc) 
# 31.8

walkH_obesL <- walkH[which(walkH$obesity_perc < 30.2),]
nrow(walkH_obesL)

walkH_obesH <- walkH[which(walkH$obesity_perc >= 30.2),]
nrow(walkH_obesH)

walkL_obesL <- walkL[which(walkL$obesity_perc < 31.8),]
nrow(walkL_obesL)

walkL_obesH <- walkL[which(walkL$obesity_perc >= 31.8),]
nrow(walkL_obesH)

#
# STEP TWO: Pseudo-panel DID analysis 
#
### 2x2 table of high/low walkability vs. high/low obesity 
### Look at the differences across groups to try to elicit a treatment effect 
### Talk about why we can't implement DID with the data (not panel data/ no "over time" observations) 
### Talk about what data we'd ideally like to have and what we could learn from it after DID 
#

Y0l = mean(walkL_obesL$obesity_perc)
Y1l = mean(walkH_obesL$obesity_perc)
Y0h = mean(walkL_obesH$obesity_perc) 
Y1h = mean(walkH_obesH$obesity_perc)

did_matrix = matrix(nrow = 2, ncol = 2)
did = did_matrix
did[1, 1] = round(Y0l, 3)
did[2, 1] = round(Y1l, 3)
did[1, 2] = round(Y0h, 3)
did[2, 2] = round(Y1h, 3)

rownames(did) = c("Low Walk (Control)", "High Walk (Treatment)")
colnames(did) = c("Low Obesity", "High Obesity")

control = Y0h - Y0l
treatment = Y1h - Y1l
treatment_effect = treatment - control


# The treatment group, which is the one with higher walkablilty causes an impact of -2.72% 
# in obesity

# STEP THREE: OLS (regular regressions) 
#
### Identify significant indicators of obesity outside of walk score 
### Including interaction terms (first identify which interaction terms are significant) 
### Cross-sectional data 
### Multicollinearity 
#

#### Incremental OLS

model1 = lm(obesity_perc ~ walk_score + median_hh_income + no_exercise_perc, 
            data = walkability_data)
summary(model1)


model2 = lm(obesity_perc ~ walk_score + median_hh_income + no_exercise_perc
            + region, data = walkability_data)
summary(model2)


model3 = lm(obesity_perc ~ walk_score + median_hh_income + no_exercise_perc
            + region + median_hh_income*median_age,
            data = walkability_data)
summary(model3)

model4 = lm(obesity_perc ~ walk_score + median_hh_income + no_exercise_perc
            + region + median_hh_income*median_age + male_pop*female_pop,
            data = walkability_data)
summary(model4)






# STEP FOUR: Use stargazer to depict regression tables with different covariates 
### Useful to tell a story (slowly involve more variables) as the table progresses
### 
#
stargazer(model1, model2, model3, model4, type = "html", out = "OLS.html",
          title = "OLS Models")
stargazer(model1, model2, model3, model4, type = "text", out = "OLS.txt",
          title = "OLS Models")

################################################################################
# APPENDIX
################################################################################

### OTHER OLS EXPERIMENTS

# Performing OLS with all variables except walkability and city

### OLS for all the variables
model5 = lm(obesity_perc ~ . - walk_score - city, data = ols_data)
summary(model5)

M = model.matrix(obesity_perc ~ . - walk_score - city, data = ols_data)

# Keeping coefficients only with a p-value less than 0.05

pvals1 = summary(model1)$coefficients[,4]
sig1 = as.vector(names(pvals1[pvals1 < 0.05]))

model6 = lm(obesity_perc ~ M[ , sig1], data = ols_data)
summary(model6)

# Removing states and using region instead
nonstate = colnames(M)[grepl("state", colnames(M)) == FALSE]

model7 = lm(obesity_perc ~ M[ , nonstate], data = ols_data)
summary(model7)


# Using just demographic variables

model8 = lm(obesity_perc ~ male_pop + female_pop + veterans + foreign_born + black + white + asian + hispanic_latino, data = walkability_data)
summary(model8)

# Testing OLS with interactions

model9 = lm(obesity_perc ~ state*region, data = ols_data)
summary(model9)


################################################################################
# END OF CODE
################################################################################
