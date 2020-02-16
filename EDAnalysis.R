library(ggplot2)
library(dplyr)
library(treemap)
library(tidyverse)
library(magrittr)
library(jsonlite)
library(knitr)
library(forcats)
library(stringr)
library(tibble)
library(plyr)
library(Hmisc)
library(readr)
library(arules)
library(arulesViz)
library(data.table)
library(methods)
library(RColorBrewer)

number_ticks <- function(n) {function(limits) pretty(limits, n)}

dir <- "../Desktop/IntroooProject/"

aislesDF <- read.csv(paste(dir, "aisles.csv", sep = ""), header = T)
departmentDF <- read.csv(paste(dir, "departments.csv", sep = ""), header = T)
Order_Produtcs_Prior_DF <- read.csv(paste(dir, "order_products__prior.csv", sep = ""), header = T)
Order_Produtcs_Train_DF <- read.csv(paste(dir, "order_products__train.csv", sep = ""), header = T)
OrdersDF <- read.csv(paste(dir, "orders.csv", sep = ""), header = T)
ProductsDF <- read.csv(paste(dir, "products.csv", sep = ""), header = T)



Order_Product_Name_Prior <- merge(Order_Produtcs_Prior_DF, ProductsDF, by = "product_id", all.x = T)

Order_Product_Name_Train <- merge(Order_Produtcs_Train_DF, ProductsDF, by = "product_id", all.x = T)

Prior_User_Order_Product <- merge(Order_Product_Name_Prior, OrdersDF, by = "order_id", all.x = T)

Order_Products <- rbind(Order_Produtcs_Prior_DF, Order_Produtcs_Train_DF)
Order_Products <- Order_Products %>%
  left_join(OrdersDF %>% select(order_id, user_id, order_number), by = "order_id")

ggplot(OrdersDF, aes(x = days_since_prior_order)) +
  geom_histogram(stat = "count", fill = "skyblue2") +
  scale_x_continuous(breaks = c(0:30)) +
  labs(title = "When do they order again?") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

prod <- ProductsDF %>% 
  group_by(department_id, aisle_id) %>%
  summarise(n = n())

prod <- prod %>%
  left_join(departmentDF, by = "department_id")

prod <- prod %>% 
  left_join(aislesDF, by = "aisle_id")

prod2 <- Order_Produtcs_Train_DF %>%
  group_by(product_id) %>%
  summarise(count = n()) %>%
  left_join(ProductsDF, by = "product_id") %>%
  ungroup() %>%
  group_by(department_id, aisle_id) %>%
  summarise(count_sum = sum(count)) %>%
  left_join(prod, by = c("department_id", "aisle_id")) %>%
  mutate(onesize = 1)


treemap(prod[prod$department != "missing" & prod$aisle != "missing",],index=c("department","aisle"),vSize="n",title="Unique Products are Offered in Each Department/Aisle",palette="Set3",border.col="#FFFFFF", type = "index")


treemap(prod2[prod2$department != "misisng" & prod2$aisle != "missing",],index=c("department","aisle"),vSize="count_sum",title="Number of Sales from the Department/Aisle",palette="Set3",border.col="#FFFFFF")


Top_Products_2k <- Order_Products %>%
  group_by(product_id) %>%
  tally() %>%
  arrange(desc(n)) %>%
  slice(1:2000)


Top_Distributions <- Order_Products %>%
  inner_join(Top_Products_2k %>% select(product_id), by = "product_id") %>%
  left_join(OrdersDF %>%
              transmute(order_id,
                        order_hour_of_day = as.numeric(order_hour_of_day)), by = "order_id") %>%
  group_by(product_id, order_hour_of_day) %>%
  tally() %>%
  group_by(product_id) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup %>%
  left_join(ProductsDF %>% select(product_id, product_name), by = "product_id")


Mean_Hour <- Top_Distributions %>%
  group_by(product_id, product_name) %>%
  summarize(mean_hour = sum(order_hour_of_day * n) / sum(n)) %>%
  ungroup


products_per_group <- 20


Morning <- Mean_Hour %>% arrange(mean_hour) %>% slice(1:products_per_group)
Evening <- Mean_Hour %>% arrange(desc(mean_hour)) %>% slice(1:products_per_group)
Morning_Names <- Morning %$% paste(product_name, collapse = '\n')
Evening_Names <- Evening %$% paste(product_name, collapse = '\n')


plt0 <- Top_Distributions %>%
  filter(product_id %in% c(Morning$product_id, Evening$product_id)) %>%
  mutate(hour_group = ifelse(product_id %in% Morning$product_id, "Morning", "Evening")) %>%
  ggplot(aes(order_hour_of_day, percent, group = product_name, colour = hour_group)) +
  annotate("segment", x = 12, y = 0, xend = 12, yend = 15, alpha = .6, linetype = 2) +
  geom_line(alpha = .8) +
  labs(x = "Hour of Day Ordered", y = "Percent of Orders by Product", title = "Popular Products Purchased in the Morning VS in Evening") +
  theme(legend.position = 'none') +
  scale_colour_manual(values = c("lightskyblue", "orange")) +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0,24))

plt1 <- plt0 +
  annotate("text", x = 25, y = 9, label = Evening_Names, adj = 1, size = 4, colour = "lightskyblue") +
  annotate("text", x = 1, y = 9, label = Morning_Names, adj = 0, size = 4, colour = "orange")

plt1




# which day of the week the company received most OrdersDF?

OrdersDF$day_week_name <- 
  ifelse(OrdersDF$order_dow == 0,
         'Sunday',
         ifelse(OrdersDF$order_dow == 1,
                'Monday',
                ifelse(OrdersDF$order_dow == 2,
                       'Tuesday',
                       ifelse(OrdersDF$order_dow == 3,
                              'Wednesday',
                              ifelse(OrdersDF$order_dow == 4,
                                     'Thursday',
                                     ifelse(OrdersDF$order_dow == 5,
                                            'Friday',
                                            ifelse(OrdersDF$order_dow == 6,
                                                   'Saturday',"")))))))

OrdersDF$day_ordered <- factor(OrdersDF$day_week_name,levels = c("Sunday",
                                                                 "Monday",
                                                                 "Tuesday",
                                                                 "Wednesday",
                                                                 "Thursday",
                                                                 "Friday",
                                                                 "Saturday"))


# visualization of OrdersDF placed by different days of the week

dow_graph <- barplot(
  table(OrdersDF$day_ordered),
  main = "Total OrdersDF by Day",
  xlab = 'Days',
  ylab = 'Number of OrdersDF',
  col = 'blue')

text(
  x = dow_graph,
  y = table(OrdersDF$day_ordered),
  labels = table(OrdersDF$day_ordered),
  pos = 1,
  cex = 1.0,
  col = 'white'
)
