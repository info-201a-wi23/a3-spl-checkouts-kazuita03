library(dplyr)
library(ggplot2)
library(tidyverse)

df <- read.csv("/Users/kazita/Desktop/2017-2023-10-Checkouts-SPL-Data.csv")


first_df <- df %>% 
  select(CheckoutYear, UsageClass) %>% 
  filter(UsageClass == "Physical")
first_y <- c(sum(first_df$CheckoutYear == 2017), 
             sum(first_df$CheckoutYear == 2018), 
             sum(first_df$CheckoutYear == 2019),
             sum(first_df$CheckoutYear == 2020),
             sum(first_df$CheckoutYear == 2021),
             sum(first_df$CheckoutYear == 2022),
             sum(first_df$CheckoutYear == 2023))
first_x <- c(unique(first_df$CheckoutYear))
first_y1 <- df %>% 
  select(CheckoutYear, UsageClass) %>% 
  filter(UsageClass == "Digital")
first_y1 <- c(sum(first_y1$CheckoutYear == 2017), 
              sum(first_y1$CheckoutYear == 2018), 
              sum(first_y1$CheckoutYear == 2019),
              sum(first_y1$CheckoutYear == 2020),
              sum(first_y1$CheckoutYear == 2021),
              sum(first_y1$CheckoutYear == 2022),
              sum(first_y1$CheckoutYear == 2023))

graph1 <- ggplot(data.frame(x = first_x, y1 = first_y, y2 = first_y1), 
                 aes(x = first_x))+
  geom_point(aes(y = first_y, color = "Physical"))+
  geom_point(aes(y = first_y1, color = "Digital"))+
  scale_color_manual(values = c("Physical" = "red", "Digital" = "blue")) +
  labs(title = "Number of Physical and Digital Copies Per Year", x = "Year", y = "Number of Copies", colors = "Sets")




second_df <- df %>% 
  select(CheckoutYear)
second_x <- first_x
second_y <- c(sum(second_df$CheckoutYear == 2017), 
              sum(second_df$CheckoutYear == 2018), 
              sum(second_df$CheckoutYear == 2019),
              sum(second_df$CheckoutYear == 2020),
              sum(second_df$CheckoutYear == 2021),
              sum(second_df$CheckoutYear == 2022),
              sum(second_df$CheckoutYear == 2023))
graph2 <- ggplot(data.frame(x = second_x, y = second_y), aes(x = second_x, y = second_y))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Number of Checkouts Per Year", x = "Year", y = "Number of Checkouts")



third_df <- df %>% 
  select(MaterialType)
third_x <- c(unique(third_df$MaterialType))
third_x
third_y <- table(third_df$MaterialType)[third_x]
third_y
third_y <- data.frame(third_y)
graph3 <- ggplot(data = third_y, aes(x = Var1, y = Freq)) +
  geom_col() +
  labs(title = "Amount per Material Type", x = "Material Type", y = "Amount")


fourth_df <- df %>% 
  select(CheckoutMonth, Checkouts) %>% 
  group_by(CheckoutMonth) %>% 
  summarize(summary = sum(Checkouts))

graph4 <- ggplot(fourth_df, aes(x = CheckoutMonth, y = summary))+
  geom_col() +
  labs(title = "Number of Checkouts Per Month", x = "Month", y = "Number of Checkouts")


