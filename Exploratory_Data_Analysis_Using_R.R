################################################################################
######################                 PROG8435                  ###############
################################################################################
####################            Assign01-Exploratory 24W         ###############
#####                               Assignment 1                         #######
################################################################################
                                     
################################################################################
##                            Written by PRABHDEEP sINGH                      ##
##                                  ID: 8946518                            #####
#
################################################################################
###                               Basic Set Up                              ####
################################################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014")

#Set work directory
setwd("F:/CanConLevel2/Courses/DAMaths/Assignments/Assignment1")

################################################################################
###                              Install Libraries                           ##
################################################################################

if(!require(ggplot2)){install.packages("ggplot2")}
library("ggplot2")

################################################################################
################### Read data and do preliminary data checks ###################
################################################################################

data_PS <- read.csv("PROG8435_Assign_Explore_24W.csv")
print(data_PS)

################################################################################
################################### Summarizing Data   #########################
################################################################################



###########################     1 Summary Table          ####################

#1(a)
total_income_by_status_PS <- tapply(data_PS$income, data_PS$m.status, FUN = sum,
                                    na.rm = TRUE)
income_table_PS <- data.frame(Marital_Status = names(total_income_by_status_PS), 
                              Total_Income = total_income_by_status_PS)
print(income_table_PS)

#1(b)
max_income_status_PS <- income_table_PS[which.max(income_table_PS$Total_Income),
                                  "Marital_Status"]
cat("The marital status with the highest total income is:",
    max_income_status_PS, "\n")





############################# 2 Calculate Mean ##############################

#2(a)
mean_age_asia_PS <- mean(data_PS$age[data_PS$nation == "Asia"], na.rm = TRUE)

cat("Mean age of respondents born in Asia:", round(mean_age_asia_PS, 2), "\n")

#2(b)
weighted_mean_age_asia_PS <- weighted.mean(data_PS$age
                                        [data_PS$nation == "Asia"],
                                        w = data$n.child[data$nation == "Asia"]
                                        , na.rm = TRUE)

cat("Weighted mean age of respondents born in Asia 
    (weighted by number of children):", round(weighted_mean_age_asia_PS, 2), "\n")




#############################  3. Table comparison #############################

#3(a)
mean_score_table_PS <- aggregate(score ~ gender, data =
                                   data_PS, FUN = mean, na.rm = TRUE)
print(mean_score_table_PS)


#3(b)
higher_score_gender_PS <- mean_score_table_PS[which.max(mean_score_table_PS
                                                        $score)
                                              , "gender"]
cat("The higher score gender is", higher_score_gender_PS)



############################    4 Table comparison #############################

#4
percentiles_PS <- quantile(data_PS$time1, c(0.34, 0.63), na.rm = TRUE)

cat("34th percentile of time taken on test:", percentiles_PS[1], "\n")
cat("63rd percentile of time taken on test:", percentiles_PS[2], "\n")


################################################################################
################################### Organizing Data   ##########################
################################################################################



#################################    1. Pie Chart     ###########################

#1(a)
political_affiliation_counts_PS <- table(data_PS$political)

# Creating pie chart
pie(political_affiliation_counts_PS, labels = NULL, main = 
      "Political Affiliation Distribution", col = 
      rainbow(length(political_affiliation_counts_PS)))
legend("left", legend = paste(names(political_affiliation_counts_PS),
                                  ": ", political_affiliation_counts_PS),
       cex = 0.8, fill = rainbow(length(political_affiliation_counts_PS)))


#1(b)

most_respondents_affiliation_PS <- names(political_affiliation_counts_PS)[which.max(political_affiliation_counts)]
cat("Political Affiliation with the most respondents:",
    most_respondents_affiliation_PS, "\n")

#1(c)

fewest_respondents_affiliation_PS <- names(political_affiliation_counts_PS)[which.min(political_affiliation_counts)]
cat("Political Affiliation with the fewest respondents:",
    fewest_respondents_affiliation_PS, "\n")

################################# 2. Summary table    ##########################

#2(a)

country_treatment_table_PS <- table(data_PS$nation, data_PS$group)
country_percentages_PS <- prop.table(country_treatment_table, margin = 1) * 100
print(country_percentages_PS)



#2(b)

# Converting the result to a data frame and binding names column
country_percentage_df_PS <- as.data.frame(cbind(Nations = rownames(country_percentages_PS), country_percentages_PS))

# Highest percent nation
highest_percentage_nation_PS <- country_percentage_df_PS[which.max(country_percentage_df_PS$treat), "Nations"]
cat("The region with the highest percentage of people in the Treatment group is :", highest_percentage_nation_PS, "\n")

#2(c)

lowest_percentage_country_PS <- country_percentage_df_PS[which.min(country_percentage_df$treat), "Nations"]
cat("The region with the lowest percentage of people in the Treatment group:", lowest_percentage_country_PS, "\n")


################################## 3. Bar Chart     ##############################

#3(a)

mean_scores_by_region_PS <- aggregate(scr ~ nation,
                                   data = data_PS, FUN = mean, na.rm = TRUE)

# Creating the bar chart
barplot(mean_scores_by_region_PS$scr, names.arg = 
          mean_scores_by_region_PS$nation, 
        main = "Mean Standardized Test Score by Region", xlab = "Region", 
        ylab = "Mean Standardized Test Score")


#3(b)

region_having_lowest_mean_PS <- mean_scores_by_region_PS[which.min(mean_scores_by_region_PS$scr), "nation"]
cat("Region with the lowest mean Standardized Test Score:", region_having_lowest_mean_PS, "\n")


#3(c)

region_having_highest_mean_PS <- mean_scores_by_region_PS[which.max(mean_scores_by_region_PS$scr), "nation"]
cat("Region with the highest mean Standardized Test Score:", region_having_highest_mean_PS, "\n")



################################## 4. Histogram    #############################

#4(a)

hist(data_PS$food, breaks = 5, main = "Distribution of Percentage of Household Income Going to Food", xlab = "Percentage", col = "lightblue", border = "black")


#4(b)

hist_data_PS <- hist(data_PS$food, breaks = 5, plot = FALSE)
max_freq_range_PS <- hist_data_PS$mids[which.max(hist_data_PS$counts)]

cat("Range with the highest frequency:", max_freq_range_PS, "\n")


################################## 5. Box Plots     ##############################


#5(a)

boxplot(income ~ m.status, data = data_PS, main = "Distribution of Income by Marital Status",
        xlab = "Marital Status", ylab = "Income", col = "lightblue",
        border = "black")

#5(b)

average_income_by_marital_status_PS <- aggregate(income ~ m.status, data = data_PS, FUN = median, na.rm = TRUE)

highest_average_income_marital_status_PS <- average_income_by_marital_status_PS[which.max(average_income_by_marital_status_PS$income), "m.status"]

cat("Marital status with the highest median income:", highest_average_income_marital_status_PS, "\n")


#5(c)

highest_average_income_marital_status_PS <- average_income_by_marital_status_PS[which.min(average_income_by_marital_status_PS$income), "m.status"]

cat("Marital status with the lowest median income:", highest_average_income_marital_status_PS, "\n")


#5(d)

sd_income_by_marital_status_PS <- aggregate(income ~ m.status, data = data_PS, FUN = sd, na.rm = TRUE)

greatest_variability_marital_status <- sd_income_by_marital_status_PS[which.max(sd_income_by_marital_status_PS$income), "m.status"]

cat("Marital status with the greatest variability in income:", greatest_variability_marital_status, "\n")



################################## 6. Scatter Plots     ##############################

#6(a)

hist(data_PS$income, main = "Distribution of Income", xlab = "income", col = "lightblue", border = "black")


#6(b)

hist(data_PS$scr, main = "Distribution of Std. Score", xlab = "Std. Score", col = "blue", border = "black")


#6(c)

plot(data_PS$income, data_PS$scr, main = "Scatter Plot: Income vs Standardized Score", xlab = "Income", ylab = "Standardized Score", col = "red", pch = 16)


#6(e)

correlation_coefficient_PS <- cor(data_PS$income, data_PS$scr)
cat("The correlation coefficient between the two params is ",correlation_coefficient_PS)

