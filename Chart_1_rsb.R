# Chart one Code
library(dplyr)
library(ggplot2)
library(knitr)

data <-
  read.csv("2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = F)

books <-
  data %>% filter(MaterialType == "BOOK") %>% count(CheckoutYear)
music <-
  data %>% filter(MaterialType == "MUSIC") %>% count(CheckoutYear)
audiobooks <-
  data %>% filter(MaterialType == "AUDIOBOOK") %>% count(CheckoutYear)
video_disc <-
  data %>% filter(MaterialType == "VIDEODISC") %>% count(CheckoutYear)
sound_disc  <-
  data %>% filter(MaterialType == "SOUNDDISC") %>% count(CheckoutYear)
ebook <-
  data %>% filter(MaterialType == "EBOOK") %>% count(CheckoutYear)
magazine <-
  data %>% filter(MaterialType == "MAGAZINE") %>% count(CheckoutYear)

ggplot() + geom_line(data = books,
                     mapping = aes(x = CheckoutYear,
                                   y = n,
                                   color = "books")) +
  scale_x_continuous(limits = c(2017, 2023)) +
  labs(title = "Material Types Checkouts Over the Years",
       x = "Checkout Year",
       y = "Total Checkouts",
       color= "Materials") +
  geom_line(data = audiobooks,
            mapping = aes(x = CheckoutYear,
                          y = n,
                          color = "audiobooks")) +
  scale_x_continuous(limits = c(2017, 2023)) +
  labs(title = "Material Types Checkouts Over the Years",
       x = "Checkout Year",
       y = "Total Checkouts") +
  geom_line(data = video_disc,
            mapping = aes(x = CheckoutYear,
                          y = n,
                          color = "video disc")) +
  scale_x_continuous(limits = c(2017, 2023)) +
  labs(title = "Material Types Checkouts Over the Years",
       x = "Checkout Year",
       y = "Total Checkouts") +
  geom_line(data = sound_disc,
            mapping = aes(x = CheckoutYear,
                          y = n,
                          color = "sound disc")) +
  scale_x_continuous(limits = c(2017, 2023)) +
  labs(title = "Material Types Checkouts Over the Years",
       x = "Checkout Year",
       y = "Total Checkouts") +
  geom_line(data = ebook,
            mapping = aes(x = CheckoutYear,
                          y = n,
                          color = "ebook")) +
  scale_x_continuous(limits = c(2017, 2023)) +
  labs(title = "Material Types Checkouts Over the Years",
       x = "Checkout Year",
       y = "Total Checkouts") +
  geom_line(data = magazine,
            mapping = aes(x = CheckoutYear, y = n, color = "magazine")) +
  scale_x_continuous(limits = c(2017, 2023)) +
  labs(title = "Material Types Checkouts Over the Years",
       x = "Checkout Year",
       y = "Total Checkouts")