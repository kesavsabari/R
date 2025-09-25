install.packages("readxl")
library(readxl)
library(dplyr)

#Reading data and cleaning
data <- read_excel("~/Downloads/online_retail_II.xlsx")
data$InvoiceDate <- as.Date(data$InvoiceDate)
data_0910 <- filter(data, InvoiceDate >= "2009-01-01" & InvoiceDate <= "2010-12-31")
set.seed(123)
data_sample <- sample_n(data_0910, 10000)
data_sample <- data_sample %>%
  +     na.omit() %>%
  +     filter(Quantity > 0, Price > 0)
dim(data_sample)
head(data_sample)


data_sample <- data_sample %>%
  +     mutate(TotalPrice = Quantity * Price)
head(data_sample %>% select(Quantity, Price, TotalPrice))
summary(data_sample$TotalPrice)
summary(data_sample[, c("Quantity", "Price", "TotalPrice")])


#Visualizing
library(ggplot2)
ggplot(data_sample, aes(x = TotalPrice)) +
  +     geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  +     ggtitle("Distribution of TotalPrice") +
  +     xlab("TotalPrice") +
  +     ylab("Count") +
  +     theme_minimal()
ggplot(data_sample, aes(x = Quantity)) +
  +     geom_histogram(bins = 50, fill = "lightgreen", color = "black") +
  +     ggtitle("Distribution of Quantity") +
  +     xlab("Quantity") +
  +     ylab("Count") +
  +     theme_minimal()
country_spending <- data_sample %>%
  +     group_by(Country) %>%
  +     summarise(TotalSpent = sum(TotalPrice)) %>%
  +     arrange(desc(TotalSpent))
top_countries <- country_spending %>% top_n(10, TotalSpent)
ggplot(top_countries, aes(x = reorder(Country, TotalSpent), y = TotalSpent, fill = Country)) +
  +     geom_bar(stat = "identity") +
  +     coord_flip() +  # Horizontal bars for readability
  +     labs(title = "Total Spending by Country",
             +          x = "Country",
             +          y = "Total Spending") +
  +     theme_minimal() +
  +     theme(legend.position = "none")


#t-test
uk <- filter(data_sample, Country == "United Kingdom")$TotalPrice
non_uk <- filter(data_sample, Country != "United Kingdom")$TotalPrice
t_test_result <- t.test(uk, non_uk)
t_test_result


##Correlation test
correlation <- cor(data_sample$Quantity, data_sample$TotalPrice)
correlation


##ANOVA
top_countries <- data_sample %>%
  +     group_by(Country) %>%
  +     summarise(TotalSpent = sum(TotalPrice)) %>%
  +     arrange(desc(TotalSpent)) %>%
  +     head(3)
anova_data <- filter(data_sample, Country %in% top_countries$Country)
anova_data$Country <- as.factor(anova_data$Country)
anova_data$Country <- droplevels(anova_data$Country)
anova_result <- aov(TotalPrice ~ Country, data = anova_data)
anova_result
summary(anova_result)


#Regression model
top_countries <- data_sample %>%
  +     group_by(Country) %>%
  +     summarise(TotalSpent = sum(TotalPrice)) %>%
  +     arrange(desc(TotalSpent)) %>%
  +     head(5)
model_data <- filter(data_sample, Country %in% top_countries$Country)
model_data$Country <- as.factor(model_data$Country)
model_data$Country <- droplevels(model_data$Country)
model <- lm(TotalPrice ~ Quantity + Price + Country, data = model_data)
summary(model)

