#Chart 2 
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

data <-
  read.csv("2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = F)

author_df <- data %>%
  filter(str_detect(Creator, "Colleen")) %>%
  filter(str_detect(Creator, "Hoover"))

hoover_books <- author_df %>% filter(MaterialType == "BOOK")

it_ends_with_us <-
  hoover_books %>% filter(str_detect(Title, "It ends with us"))

it_ends_with_us_2021 <-
  it_ends_with_us %>% filter(CheckoutYear == 2021)
it_ends_with_us_2022 <-
  it_ends_with_us %>% filter(CheckoutYear == 2022)

layla_book <- hoover_books %>% filter(str_detect(Title, "Layla"))
layla_2021 <- layla_book %>% filter(CheckoutYear == 2021)
layla_2022 <- layla_book %>% filter(CheckoutYear == 2022)

ggplot() +
  geom_col(
    data = it_ends_with_us_2021,
    mapping = aes(x = CheckoutYear,
                  y = Checkouts,
                  color = "it ends with us 2021")
  ) +
  labs(title = "Checkouts of It Ends With Us and Layla in 2021 and 2022") +
  geom_col(
    data = it_ends_with_us_2022,
    mapping = aes(x = CheckoutYear,
                  y = Checkouts,
                  color = "it ends with us 2022")
  ) +
  labs(title = "Checkouts of It Ends With Us and Layla in 2021 and 2022",
       y = "Checkouts per Year",
       color = "Specific Book") +
  geom_col(
    data = layla_2021,
    mapping = aes(x = CheckoutYear,
                  y = Checkouts,
                  color = "layla 2021")
  ) +
  labs(title = "Checkouts of It Ends With Us and Layla in 2021 and 2022") +
  geom_col(
    data = layla_2022,
    mapping = aes(x = CheckoutYear,
                  y = Checkouts,
                  color = "layla 2022")
  ) +
  labs(title = "Checkouts of It Ends With Us and Layla in 2021 and 2022")