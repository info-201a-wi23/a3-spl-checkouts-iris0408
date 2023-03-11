# Chart 3
Physical_total <-
  data %>% filter(UsageClass == "Physical") %>% count(UsageClass)
Digital_Total <-
  data %>% filter(UsageClass == "Digital") %>% count(UsageClass)

ggplot() + geom_col(Physical_total, mapping = aes(
                                    x = UsageClass, 
                                    y = n, 
                                    fill = "Physical")) +
  scale_y_continuous(limits = c(0, 470000)) +
  labs(title = "Total Checkouts of Physical and Digital Classes from 2017-2023",
       fill = "Usage Class Type",
       x = "Usage Class",
       y = "count") +
  geom_col(Digital_Total, mapping = aes(
                    x = UsageClass,
                    y = n,
                    fill = "Digital"))