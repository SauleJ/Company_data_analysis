# Install and load necessary packages
install.packages("readr")
library(readr)

install.packages("corrplot")
library(corrplot)

install.packages("dplyr")
library(dplyr)

install.packages("Hmisc")
library(Hmisc)

install.packages("ggplot2")
library(ggplot2)

install.packages("hrbrthemes")
library(hrbrthemes)

install.packages("waffle")
library(waffle)

install.packages("ggthemes")
library(ggthemes)

# Read data
setwd(getwd())
setwd("Path to Future-500-2.csv ")
fin <- read.csv("Future-500-2.csv")

# Show the structure of the data
str(fin)

# Convert symbols to numeric values
fin$Profit <- as.numeric(as.character(fin$Profit))

# Provide summary statistics of the data
summary(fin)

# Data preparation: removing unnecessary symbols
fin$Expenses <- gsub(" Dollars", "", fin$Expenses)
fin$Expenses <- gsub(",", "", fin$Expenses)
fin$Expenses <- as.numeric(as.character(fin$Expenses))

fin$Revenue <- gsub("\\$", "", fin$Revenue)
fin$Revenue <- gsub(",", "", fin$Revenue)
fin$Revenue <- as.numeric(as.character(fin$Revenue))

fin$Growth <- gsub("\\%", "", fin$Growth)
fin$Growth <- as.numeric(as.character(fin$Growth))
head(fin, 20)

# Excel string error correction
fin$Expenses <- as.numeric(as.character(fin$Expenses))

#----------------------------------------------------------------------------
# Initial data variance calculation
#----------------------------------------------------------------------------
disp_var <- var(fin$Employees)
cat(disp_var, "\n")

disp_var <- var(fin$Revenue)
cat(disp_var, "\n")

disp_var <- var(fin$Expenses)
cat(disp_var, "\n")

disp_var <- var(fin$Profit)
cat(disp_var, "\n")

disp_var <- var(fin$Growth)
cat(disp_var, "\n")

#----------------------------------------------------------------------------
# Data cleaning
#----------------------------------------------------------------------------

# Factual filling
fin[is.na(fin$State) | fin$State == "" | fin$City == "New York", "State"] <- "NY"
fin[is.na(fin$State) | fin$State == "" | fin$City == "San Francisco", "State"] <- "CA"

# Fill missing values with median
fin <- fin %>%
  group_by(Industry) %>%
  mutate(Employees = ifelse(is.na(Employees), median(Employees, na.rm = TRUE), Employees))

fin <- fin %>%
  group_by(Industry) %>%
  mutate(Revenue = ifelse(is.na(Revenue), median(Revenue, na.rm = TRUE), Revenue))

fin <- fin %>%
  group_by(Industry) %>%
  mutate(Growth = ifelse(is.na(Growth), median(Growth, na.rm = TRUE), Growth))

# Fill missing values with derived values

fin[is.na(fin$Profit), "Profit"] <- fin[is.na(fin$Profit), "Revenue"] - 
  fin[is.na(fin$Profit), "Expenses"]

fin[is.na(fin$Expenses), "Expenses"] <- fin[is.na(fin$Expenses), "Revenue"] - 
  fin[is.na(fin$Expenses), "Profit"]

# Remove rows with NA or null values

fin <- subset(fin, !is.na(fin$Expenses) & fin$Expenses != "")
fin <- subset(fin, !is.na(fin$Profit) & fin$Profit != "")
fin <- subset(fin, !is.na(fin$Growth) & fin$Growth != "")
fin <- subset(fin, !is.na(fin$Industry) & fin$Industry != "")
fin <- subset(fin, !is.na(fin$Inception) & fin$Inception != "")

summary(fin)
head(fin, 20)

#----------------------------------------------------------------------------
# Outliers
#----------------------------------------------------------------------------

boxplot(fin$Inception, fin$Employees, fin$Revenue, fin$Expenses, fin$Profit, fin$Growth, names = c("Inception Year", "Number of Employees", "Revenue", "Expenses", "Profit", "Growth"))
boxplot(fin$Revenue, fin$Profit, names = c("Revenue", "Profit"))

outlier_columns <- c('Inception', 'Employees', 'Revenue', 'Profit')
threshold <- 1.5
data_clean <- fin
for (col in outlier_columns) {
  data_clean <- data_clean %>%
    filter(!(!!sym(col) < quantile(!!sym(col), 0.25) - threshold * IQR(!!sym(col)) | !!sym(col) > quantile(!!sym(col), 0.75) + threshold * IQR(!!sym(col))))
}
boxplot(data_clean$Inception, data_clean$Employees, data_clean$Revenue, data_clean$Expenses, data_clean$Profit, data_clean$Growth, names = c("Inception Year", "Number of Employees", "Revenue", "Expenses", "Profit", "Growth"))

boxplot(data_clean$Inception, names = c("Inception Year"))
boxplot(data_clean$Employees, names = c("Number of Employees"))
summary(data_clean)

#----------------------------------------------------------------------------
# Dispersion by industry
#----------------------------------------------------------------------------

# Employees dispersion by industry
variance_by_industry <- aggregate(Employees ~ Industry, data = data_clean, FUN = var, na.rm = TRUE)
print(variance_by_industry)

# Revenue dispersion by industry

variance_by_industry <- aggregate(Revenue ~ Industry, data = data_clean, FUN = var, na.rm = TRUE)
print(variance_by_industry)

# Expenses dispersion by industry

variance_by_industry <- aggregate(Expenses ~ Industry, data = data_clean, FUN = var, na.rm = TRUE)
print(variance_by_industry)

# Profit dispersion by industry

variance_by_industry <- aggregate(Profit ~ Industry, data = data_clean, FUN = var, na.rm = TRUE)
print(variance_by_industry)

# Growth dispersion by industry

variance_by_industry <- aggregate(Growth ~ Industry, data = data_clean, FUN = var, na.rm = TRUE)
print(variance_by_industry)

#----------------------------------------------------------------------------
# Find the two highest values by profit
#----------------------------------------------------------------------------

# Find the two highest values and their corresponding companies
max_profit_data <- fin %>%
  arrange(desc(Profit)) %>%
  head(2)  # Take the two highest values

# Print the results
print(max_profit_data[, c("Name", "Profit")])

#----------------------------------------------------------------------------
# Find the two lowest values by revenue
#----------------------------------------------------------------------------

# Find the two lowest values and their corresponding companies
min_Revenue_data <- fin %>%
  arrange(Revenue) %>%
  head(2)  # Take the two lowest values

# Print the results
print(min_Revenue_data[, c("Name", "Revenue")])

#----------------------------------------------------------------------------
# Summary by industry
#----------------------------------------------------------------------------

summary_by_industry <- by(data_clean, data_clean$Industry, summary)
summary_by_industry$"Government Services"

#----------------------------------------------------------------------------
# Correlations
#----------------------------------------------------------------------------

corrData <- data_clean[, c("Inception", "Employees", "Revenue", "Expenses", "Profit", "Growth")]
cor(corrData)

# Correlation statistical significance

rcorr(as.matrix(corrData))

#----------------------------------------------------------------------------
# Normalization by min-max and mean/variance
#----------------------------------------------------------------------------

data_backup <- data_clean
data_clean <- data_backup

x <- data_clean$Revenue
normalized_data1 <- (x - mean(x)) / sqrt(var(x))

ggplot(data_clean, aes(x = Industry, y = normalized_data1)) +
  geom_bar(stat = "identity", fill = "#0073e6") +
  labs(x = "Industry", y = "Revenue") +
  ggtitle("Column Chart: Revenue by Sector") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(panel.background = element_blank())

# Second method: normalization of data by min - max

x <- data_clean$Profit
normalized_data2 <- (x - min(x)) / (max(x) - min(x))

ggplot(data_clean, aes(x = Industry, y = normalized_data2)) +
  geom_bar(stat = "identity", fill = "#0073e6") +
  labs(x = "Industry", y = "Revenue") +
  ggtitle("Column Chart: Revenue by Sector") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(panel.background = element_blank())

#----------------------------------------------------------------------------
# Visualization
#----------------------------------------------------------------------------

# Industry sector renaming:
industry_mapping <- c(
  "Construction" = "Construction",
  "Financial Services" = "Financial Services",
  "Government Services" = "Government Services",
  "Health" = "Health",
  "IT Services" = "IT Services",
  "Retail" = "Retail",
  "Software" = "Software"
)

# 1. Number of established companies by year of establishment
hist(data_clean$Inception, main = "Number of New Companies by Year of Establishment", xlab = "Year of Establishment", ylab = "Frequency")

company_count_by_year <- data_clean %>%
  group_by(Inception) %>%
  summarise(Count = n())
print(company_count_by_year)

# 2. Number of established companies by industry sector
# Table of industry sector frequencies
industry_counts <- table(data_clean$Industry)

# Diagram
barplot(industry_counts,
        main = "Number of Companies by Industry Sector",
        xlab = "Industry Sector",
        ylim = c(0, 130),
        col = "#942911",
        border = "black",
        names.arg = industry_mapping)

# Numbers above the bars
text(x = barplot(industry_counts, plot = FALSE),
     y = industry_counts,
     labels = industry_counts,
     pos = 3,
     col = "black")

# 3. Number of established companies by industry sector, as a percentage
uic <- table(data_clean$Industry) # UIC - Unique Industry Count
ui_names <- dimnames(uic)[[1]] # Unique Industry Names
uic <- as.vector(uic)
val_names <- sprintf("%s (%s)", ui_names, scales::percent(round(uic/sum(uic), 2)))
names(uic) <- val_names

waffle::waffle(uic)

# 4. Distribution of profit by industry sector
ggplot(data_clean, aes(x = Industry, y = Profit, fill = Industry)) +
  geom_violin() +
  ggtitle("Profit by Industry Sector (Split Violin Plot)") +
  xlab("Industry Sector") +
  ylab("Profit") +
  scale_x_discrete(labels = industry_mapping) +
  theme(legend.position = "none")

# 5. Number of companies by state
ggplot(data_clean, aes(x = State)) +
  geom_bar(fill = "#942911") +
  labs(title = "Number of Companies by State", x = "State", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 50, by = 5)) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5)

# 6. Growth by industry sector diagram
st <- ggplot(data=data_clean, aes(x=Industry, y=Growth,
                                  colour = Industry)) + geom_boxplot(size=1)

st + labs(x = "Industry Sector", y = "Growth") +
  scale_x_discrete(labels = industry_mapping) +
  scale_colour_discrete(name = "Industry Sector", labels = industry_mapping) +
  ggtitle("Company Growth by Industry Sector")

# 7. Expenses by industry sector

ggplot(data_clean, aes(x = Industry, y = Expenses, fill = Industry)) +
  geom_violin() +
  ggtitle("Expenses by Industry Sector (Split Violin Plot)") +
  xlab("Industry Sector") +
  ylab("Expenses") +
  scale_x_discrete(labels = industry_mapping) +
  theme(legend.position = "none")

# 8. Number of employees by industry sector
sum_employees_by_industry <- aggregate(Employees ~ Industry, data = data_clean, FUN = sum, na.rm = TRUE)

ggplot(data = sum_employees_by_industry, aes(x = c("Construction", "Financial Services", "Government Services", "Health", "IT Services", "Retail", "Software"), y = Employees)) +
  geom_bar(stat = "identity", fill = "#942911") +
  geom_text(aes(label = Employees), vjust = -0.5, size = 4, color = "black") +
  labs(title = "Number of Employees by Industry Sector", x = "Industry Sector", y = "Number of Employees") +
  theme_minimal()

# 9. Scatter plot between two variables (Growth and Profit)

ggplot(data = data_clean, aes(x = Growth, y = Profit)) +
  geom_point() +
  labs(title = "Growth vs. Profit", x = "Growth", y = "Profit") + geom_smooth(method = "loess", formula = y ~ x)

# Calculate median profit when growth is 15%
print(mean(data_clean$Profit[data_clean$Growth == 15], na.rm = TRUE))

# 10. Number of employees by state

sum_employees_by_state <- aggregate(Employees ~ State, data = data_clean, FUN = sum, na.rm = TRUE)

ggplot(data = sum_employees_by_state, aes(x = State, y = Employees)) +
  geom_bar(stat = "identity", fill = "#942911") +
  geom_text(aes(label = Employees), vjust = -0.5, size = 4, color = "black") +
  labs(title = "Number of Employees by State", x = "State", y = "Number of Employees") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

