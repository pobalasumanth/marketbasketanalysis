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

num_ticks <- function(n) {function(limits) pretty(limits, n)}

dir <- "../Desktop/IntroooProject/"

aisle_df <- read.csv(paste(dir, "aisles.csv", sep = ""), header = T)
dept_df <- read.csv(paste(dir, "departments.csv", sep = ""), header = T)
Order_Produtcs_Prior_DF <- read.csv(paste(dir, "prod_orders__prior.csv", sep = ""), header = T)
Order_Produtcs_Train_DF <- read.csv(paste(dir, "prod_orders__train.csv", sep = ""), header = T)
OrdersDF <- read.csv(paste(dir, "orders.csv", sep = ""), header = T)
ProductsDF <- read.csv(paste(dir, "products.csv", sep = ""), header = T)



prodname_order_prior <- merge(Order_Produtcs_Prior_DF, ProductsDF, by = "product_id", all.x = T)

prodname_order_train <- merge(Order_Produtcs_Train_DF, ProductsDF, by = "product_id", all.x = T)

Prior_User_Order_Product <- merge(prodname_order_prior, OrdersDF, by = "order_id", all.x = T)

prod_orders <- rbind(Order_Produtcs_Prior_DF, Order_Produtcs_Train_DF)
prod_orders <- prod_orders %>%
  left_join(OrdersDF %>% select(order_id, user_id, order_number), by = "order_id")

ggplot(OrdersDF, aes(x = days_since_prior_order)) +
  geom_histogram(stat = "count", fill = "skyblue2") +
  scale_x_continuous(breaks = c(0:30)) +
  labs(title = "When do they order again?") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

product <- ProductsDF %>% 
  group_by(department_id, aisle_id) %>%
  summarise(n = n())

product <- product %>%
  left_join(dept_df, by = "department_id")

product <- product %>% 
  left_join(aisle_df, by = "aisle_id")

product2 <- Order_Produtcs_Train_DF %>%
  group_by(product_id) %>%
  summarise(count = n()) %>%
  left_join(ProductsDF, by = "product_id") %>%
  ungroup() %>%
  group_by(department_id, aisle_id) %>%
  summarise(count_sum = sum(count)) %>%
  left_join(product, by = c("department_id", "aisle_id")) %>%
  mutate(onesize = 1)


treemap(product[product$department != "missing" & product$aisle != "missing",],index=c("department","aisle"),vSize="n",title="Unique Products are Offered in Each Department/Aisle",palette="Set3",border.col="#FFFFFF", type = "index")


treemap(product2[product2$department != "misisng" & product2$aisle != "missing",],index=c("department","aisle"),vSize="count_sum",title="Number of Sales from the Department/Aisle",palette="Set3",border.col="#FFFFFF")


Top_Products_2k <- prod_orders %>%
  group_by(product_id) %>%
  tally() %>%
  arrange(desc(n)) %>%
  slice(1:2000)


top_distributes <- prod_orders %>%
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


meanhour <- top_distributes %>%
  group_by(product_id, product_name) %>%
  summarize(meanhour = sum(order_hour_of_day * n) / sum(n)) %>%
  ungroup


prod_pergroup <- 20


mrng <- meanhour %>% arrange(meanhour) %>% slice(1:prod_pergroup)
evng <- meanhour %>% arrange(desc(meanhour)) %>% slice(1:prod_pergroup)
mrng_names <- mrng %$% paste(product_name, collapse = '\n')
evng_names <- evng %$% paste(product_name, collapse = '\n')


plot0 <- top_distributes %>%
  filter(product_id %in% c(mrng$product_id, evng$product_id)) %>%
  mutate(hour_group = ifelse(product_id %in% mrng$product_id, "mrng", "evng")) %>%
  ggplot(aes(order_hour_of_day, percent, group = product_name, colour = hour_group)) +
  annotate("segment", x = 12, y = 0, xend = 12, yend = 15, alpha = .6, linetype = 2) +
  geom_line(alpha = .8) +
  labs(x = "Hour of Day Ordered", y = "Percent of Orders by Product", title = "Popular Products Purchased in the mrng VS in evng") +
  theme(legend.position = 'none') +
  scale_colour_manual(values = c("lightskyblue", "orange")) +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0,24))

plot1 <- plot0 +
  annotate("text", x = 25, y = 9, label = evng_names, adj = 1, size = 4, colour = "lightskyblue") +
  annotate("text", x = 1, y = 9, label = mrng_names, adj = 0, size = 4, colour = "orange")

plot1




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

prod_orders <- fread('../Desktop/Introooproject/prod_orders__train.csv')
head(prod_orders)

prod_orders %>% 
  group_by(order_id) %>% 
  summarize(n_items = last(add_to_cart_order)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(stat="count",fill="skyblue") + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))

temp <-prod_orders %>% 
  group_by(product_id) %>% 
  left_join(products,by="product_id")


write.csv(temp, file = "transactions.csv")
transactions<-read.transactions("transactions.csv", format = "single", sep = ",",cols = c(2,6))

summary(transactions)



inspect(transactions[1:3])

#visualiza the most frequent item sets in this dataset
itemFrequency(transactions,type='relative')
itemFrequencyPlot(transactions,topN=25,type='relative')
itemFrequencyPlot(transactions,topN=25,type='absolute')

grocery_rules <- apriori(transactions, parameter = list(support =
                                                         0.00005, confidence = 0.6))

# Association rules
summary(grocery_rules)

inspect(grocery_rules[1:10])

inspect(head(sort(grocery_rules,by='lift'),6))
