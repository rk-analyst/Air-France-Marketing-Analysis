# Importing all required libraries
library(readxl)
# Importing DoubleClick data source
double_click_data <- read_excel("C:/Users/hanzn/OneDrive/Documents/MBAN/VISUALIZING DATA WITH R/CASE STUDY/Air France Case Spreadsheet Supplement.xls",
sheet = "DoubleClick")
colSums(is.na(double_click_data))
#####################
## DATA MASSAGING ###
#####################
### CLEANING THE DATA ###
# Fixing variable names
colnames(double_click_data) <- gsub("\\.", "", colnames(double_click_data))
colnames(double_click_data) <- gsub("%", "", colnames(double_click_data))
colnames(double_click_data) <- gsub("/", "_per", colnames(double_click_data))
colnames(double_click_data) <- gsub(" ", "_", colnames(double_click_data))
### INFORMATION LOSS ###
### INFORMATION GAIN ###
# Key Metrics for SEM: CPC, Click thru Rate, CVR, and ROAS
# Information Gain: Creating ROAS variable (in percent)
double_click_data$ROAS <- (double_click_data$Amount / double_click_data$Total_Cost) * 100
# Information Gain: Creating Returns variable
double_click_data$Returns <- (double_click_data$Amount - double_click_data$Total_Cost)
# Good campaigns - ABOVE INSDUSTRY AVERAGES
# CPC lower than 1.53(INDUSTRY BENCHMARK)
# CTR greater than 4.68%(INDUSTRY BENCHMARK)
# CVR greater than 3.55%(INDUSTRY BENCHMARK)
# ROAS greater than 200%
good_campaigns <- double_click_data[ which(double_click_data$Avg_Cost_per_Click <= 1.53
& double_click_data$ROAS >= 100
& double_click_data$Engine_Click_Thru_ >= 4.68
& double_click_data$Trans_Conv_ >= 3.55), ]
library(ggplot2)
ggplot(data = good_campaigns, aes(x = Total_Cost,y = Amount)) +
geom_point(aes(size = Avg_Cost_per_Click))
##########################
## DESCRIPTIVE ANALYSIS ##
##########################
# Checking the frequency of selected variables
# Publisher Name
sort(table(double_click_data$Publisher_Name), decreasing = TRUE)
# Match Type
sort(table(double_click_data$Match_Type), decreasing = TRUE)
# Status
sort(table(double_click_data$Status), decreasing = TRUE)
# Keyword Group
sort(table(double_click_data$Keyword_Group), decreasing = TRUE)
# Summary of whole dataset
summary(double_click_data)
# Summary of good campaigns
summary(good_campaigns)
library(dplyr)
Publisher_data_sum <- double_click_data %>%
group_by(Publisher_Name) %>%
summarize(sum_total_cost = sum(Total_Cost),
sum_amount = sum(Amount),
sum_returns = sum(Returns),
sum_bookings = sum(Total_Volume_of_Bookings),
avg_returns_per_booking = sum(Returns) / sum(Total_Volume_of_Bookings),
cost_per_booking = sum(Total_Cost) / sum(Total_Volume_of_Bookings),
number_of_campaigns = n())
ggplot(data = Publisher_data_sum, aes(x = cost_per_booking,y = avg_returns_per_booking, color = Publisher_Name)) +
geom_point(aes(size = sum_bookings)) +
scale_size(range = c(1, 10)) +
geom_text(aes(label = Publisher_Name), nudge_x = 30
, nudge_y = 50)
KeywordGroup_data_sum <- double_click_data %>%
group_by(Keyword_Group) %>%
summarize(sum_total_cost = sum(Total_Cost),
sum_amount = sum(Amount),
sum_returns = sum(Returns),
sum_bookings = sum(Total_Volume_of_Bookings),
avg_returns_per_booking = sum(Returns) / sum(Total_Volume_of_Bookings),
cost_per_booking = sum(Total_Cost) / sum(Total_Volume_of_Bookings),
number_of_campaigns = n()) %>%
arrange(desc(sum_returns))
KeywordGroup_top5 <- KeywordGroup_data_sum[1:5,]
ggplot(KeywordGroup_top5, aes(x = Keyword_Group, y = sum_returns, fill = Keyword_Group)) +
geom_col() +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
ggtitle("Top 5 Keyword Groups") +
labs(x = "Keyword Group", y = "Totla Returns")
good_campaigns1 <- good_campaigns[ , c("Publisher_Name", "Keyword_Group","Avg_Cost_per_Click",
"Engine_Click_Thru_", "Trans_Conv_", "ROAS")]