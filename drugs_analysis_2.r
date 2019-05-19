# Install packages if not already installed
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(reshape2)){install.packages("reshape2")}
if(!require(data.table)){install.packages("data.table")}
if(!require(FSA)){install.packages("FSA")}
if(!require(car)){install.packages("car")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(pwr)){install.packages("pwr")}
if(!require(PMCMR)){install.packages("PMCMR")}


library(dplyr)
library(tidyr)
library(data.table)

# Read the preprocessed HRB file using file choose.
DrugsData <- read.csv(file.choose(), header=TRUE, na.strings=c("", " ", "NA"), 
                        stringsAsFactors = FALSE)

###################
# Clean the data  #

# Replace the column names which are too long
colnames(DrugsData) <- c('Year', 'County', 'NoSchool', 'Primary', 'JuniorCert', 
                         'LeavingCert', 'ThirdLevel', 'Unknown', 'Total')
# Make the dataframe a data.table as much easier to work with
setDT(DrugsData)

# Lets have a look at the structure
str(DrugsData)
# As its a data.tabe we can see head and tail 
DrugsData

# We dont need the totals column
DrugsData <- DrugsData[ , -9]

# In the original data set HRB has chosen to replace counts that are less then 5 
# by '~'.  We need to replace these in some way, there is no obvious way to 
# interpolate/deconvolve them
# These are also read in as strings so we convert them to integrers
# From examination of the data many vales appear to be 1 as the totals add this way
DrugsData[NoSchool == '~', NoSchool := '1'][, NoSchool := as.integer(NoSchool)]
DrugsData[Primary == '~', Primary := '1'][, Primary := as.integer(Primary)]
DrugsData[JuniorCert == '~', JuniorCert := '1'][, JuniorCert := as.integer(JuniorCert)]
DrugsData[LeavingCert == '~', LeavingCert := '1'][, LeavingCert := as.integer(LeavingCert)]
DrugsData[ThirdLevel == '~', ThirdLevel := '1'][, ThirdLevel := as.integer(ThirdLevel)]
DrugsData[Unknown == '~', Unknown := '1'][, Unknown := as.integer(Unknown)]
# Check to see if they are all gone
DrugsData[NoSchool == '~', ]

###################################################

# The data is in wide form so we need to rehape it
library("reshape2")
DrugsDatalf <- melt(DrugsData, id=c("Year","County"))
# Rename column from the melt
names(DrugsDatalf)[names(DrugsDatalf) == "variable"] <- "Ed_Level"
names(DrugsDatalf)[names(DrugsDatalf) == "value"] <- "Reported_cases"

# Change the order of levels in Ed_level just in case, although they already in order
DrugsDatalf$Ed_Level <-factor(DrugsDatalf$Ed_Level, 
                                   levels = c("NoSchool","Primary","JuniorCert","LeavingCert",
                                              "ThirdLevel","Unknown"))
DrugsDatalf$Year <- as.factor(DrugsDatalf$Year) # Change Year to factor
# The data is broken down by county, so extract the totals rows into its 
# own data.table 
DrugsDataTots <- DrugsDatalf[DrugsDatalf$County == 'Totals',]
DrugsCounty <- DrugsDatalf[DrugsDatalf$County != 'Totals',]

# Remove the County variable in the totals set as it isnt needed
DrugsDataTots <- DrugsDataTots[ , -2]

# Examine the new data.tables
DrugsCounty
DrugsDataTots

# Lets look at the structure of the resultant dataset
str(DrugsDataTots)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
##### Exploratory analysis #####

library(FSA)

Summarize(Reported_cases ~ Ed_Level,
          data = DrugsDataTots)


# Box plots
# ++++++++++++++++++++
# Plot the reported cases by Ed_level
library(ggpubr)

box_plot <- ggplot(DrugsDataTots, aes(x = Ed_Level, y = Reported_cases )) +
  geom_boxplot() +
  ggtitle("Counts by education")
box_plot

# Mean plots
# ++++++++++++++++++++
# Plot Reported_cases by Ed_level
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
ggline(DrugsDataTots, x = "Ed_Level", y = "Reported_cases", 
       add = c("mean_se", "jitter"), 
       order = c("NoSchool","Primary","JuniorCert","LeavingCert",
                 "ThirdLevel","Unknown"),
       ylab = "Reported Cases", xlab = "Education Level")

# Lets see a Q-Q plot for the data 
ggqqplot(DrugsDataTots$Reported_cases, ylab = "Reported Cases")
# Indicates thatassumeing normality might not be safe

###################################################
# Check to see if the data is normal
library("lattice")
histogram(~Reported_cases | Ed_Level, data = DrugsDataTots)  # Visually

# Formal test of normality
# provided through widely used Shapiro-Wilks test
shapiro.test(DrugsDataTots$Reported_cases)

with(DrugsDataTots, tapply(Reported_cases, Ed_Level, shapiro.test))

# p-value tells us the chances that the sample comes from a normal distribution 
# In this example, some p-values are greater than 0.05 while others
# they are clearly not so we cant reject the hypotheseis that it is normally 
# distributed, but it also not possible to accept it.

# Lets do Q-Q plots for all of them
for (lvl in levels(DrugsDataTots$Ed_Level))
{
    qqnorm(DrugsDataTots$Reported_cases[DrugsDataTots$Ed_Level == lvl], 
         main = paste("QQ Plot", lvl, "Education Attained"))
  qqline(DrugsDataTots$Reported_cases[DrugsDataTots$Ed_Level == lvl])
}
  
with(DrugsDataTots, {
  qqnorm(Reported_cases, main = "QQ Plot Education")
  qqline(Reported_cases)
})


#===============================================================#
##### Build the ANOVA model ####

AOV_model <- aov(Reported_cases ~ Ed_Level, data = DrugsDataTots)
summary(AOV_model)
#### Checking residuals ####

## Distribution (regular ANOVA assumption is Normal)
hist(AOV_model$residuals, probability = TRUE, xlab = "residuals", main = "Residuals")
lines(density(AOV_model$residuals), col = "red", lwd = 1)
qqnorm(AOV_model$residuals)
qqline(AOV_model$residuals)
shapiro.test(x = AOV_model$residuals)
# Histogram and qqplot indicate that there is a departure from normality.
# From Shapiro-Wilks test we reject H0 at 5% significance level. There is an evidence to conclude 
# that residuals are not normally distributed


## Equality of variances
plot(AOV_model$residuals, main = "Residuals vs their index")
abline(h = 0.0, col = "red")
# From the plot above it is hard to tell with certainty if the variances 
# are homogeneous, but it looks like there is some pattern in them. Larger 
# values tend to be below 0. There is a high chance the residuals are 
# heteroscedstic.

plot(x = AOV_model$fitted.values, 
     y = AOV_model$residuals, 
     main = "Residuals vs fitted values",
     xlab = "Fitted values",
     ylab = "Residuals")
# The "Residulas vs fitted values" idicates that the variances might not be 
# homogeneous. We need to further invistigate this with the Levene's test 
# (it is less sensitive to Normality assumptions than Bartlett's)
library(car)
leveneTest(AOV_model)

# Do the bartletts but it is needed but does no harm
bartlett.test(Reported_cases ~ Ed_Level, data=DrugsDataTots) # Also suggets inequality of variances under 5% significance level.
# H0: population variances are equal
# H1: not equal
# Reject null under 5% significance level. There is an evidence to conclude that there is a significant difference between variances.
# Variances are not homogeneous (aka homoscedastic)

#### Hypothesis testing ####
summary(AOV_model)
# H0: mean_1 = mean_2 = ... mean_m
# H1: not all means are equal
#
# The p-value indicates that there is an evidence to conclude that the means are not equal under 5% significance level.
# Conclusion: Education level does have an effect on the "drug use" clinical visits. 
# Since the assumptions of the ANOVA model were not fully satisfied, 
# this conclusion should be taken with care.

## Not let's see which levels are actually different.
comparisons <- TukeyHSD(AOV_model)
comparisons
par(mar=c(5,10,4,2)) # increase left margin
plot(comparisons, las=1)
# From the plot above we can see that there is a significant difference between Primary and  No school
# levels under 5% significance level. Furthermore there were more people with Primary than with No school.
# Same conclusion for JuniorCert and NoSchool, also having more people with JuniorCert than No school.
# Similar analysis should be done for other comparisons..... 

#### Taking a Ln-transform of the data to stabilize the variance
DrugsDataTots$ln_value <- log(DrugsDataTots$Reported_cases)

##### Exploratory analysis #####
par(mar=c(5,5,5,5)) # increase left margin
box_plot_ln_value <- ggplot(DrugsDataTots, aes(x = Ed_Level, y = ln_value)) +
  geom_boxplot() +
  ggtitle("Counts by education")
box_plot_ln_value


##### Build the ANOVA model ####
AOV_model_ln <- aov(ln_value ~ Ed_Level, data = DrugsDataTots)

#### Checking residuals ####

## Distribution (regular ANOVA assumption is Normal)
hist(AOV_model_ln$residuals, probability = TRUE, xlab = "residuals", main = "Residuals")
lines(density(AOV_model_ln$residuals), col = "red", lwd = 1)
qqnorm(AOV_model_ln$residuals)
qqline(AOV_model_ln$residuals)
shapiro.test(x = AOV_model_ln$residuals)
### You can truy to do the same analysis as before but on the ln-transformed variable value and check
### if the ln stabilized the variance.

#===================================================================================#
#### Nonparametric ANOVA ####
kruskal_wallis_fit <- kruskal.test(Reported_cases ~ Ed_Level, data=DrugsDataTots)
kruskal_wallis_fit
# Indicates that at least one group comes from a different population. 
# This might suggest that education have a significant effect on the visists under 5% significance level.
## Not let's see which levels are actually different.
library(PMCMR)
library(stats)

attach(DrugsDataTots)

# Use Tukey as a ditribution
posthoc.kruskal.nemenyi.test(Reported_cases ~ Ed_Level, data = DrugsDataTots, 
                             dist="Tukey")
# Now do it with chisquare
posthoc.kruskal.nemenyi.test(Reported_cases ~ Ed_Level, data = DrugsDataTots, 
                             dist="Chisquare")

#Also run a Wilcoxon Test
pairwise.wilcox.test(Reported_cases, Ed_Level,
                     p.adjust.method = "BH")


# Default method = "kruskal.test" for multiple groups
ggboxplot(DrugsDataTots, x = "Ed_Level", y = "Reported_cases",
          color = "Ed_Level", add="jitter", palette = "jco")+
  stat_compare_means()


detach(DrugsDataTots)


#### Power analysis to get optimal number of samples ####
# We are doing this for ANOVA although it is not recommened in the case 
# where we are using non-parametric data, ie. for Kruskall Wallis

library(pwr)
power_information <- pwr.anova.test(f = 0.8, k=6, n=NULL, 
                                    sig.level = 0.05, 
                                    power = 0.90)
power_information


# For fun and conclusions
## 
acf(x = AOV_model$residuals)
# The autocorrelation plot shows some significant correlations at some lags, suggesting 
# that there is some temporal dependence among residuals. It is somewhat expected, 
# since we have yearly measurements but ignored time in the model.
