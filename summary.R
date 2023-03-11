
library(dplyr)
library(ggplot2)
library(knitr)
data <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = F)

#Introduction

#Summary Information and first chart data

Total_books_2017 <- data %>% filter(CheckoutYear== 2017) %>% filter(MaterialType== "BOOK") 
total_books_2018 <- data %>% filter(CheckoutYear == 2018) %>% filter(MaterialType == "BOOK")
total_books_2019 <- data %>% filter(CheckoutYear == 2019) %>% filter(MaterialType == "BOOK")
total_books_2020 <- data %>% filter(CheckoutYear == 2020) %>% filter(MaterialType == "BOOK")
total_books_2021 <- data %>% filter(CheckoutYear == 2021) %>% filter(MaterialType == "BOOK")
total_books_2022<- data %>% filter(CheckoutYear == 2022) %>% filter(MaterialType == "BOOK")
total_books_2023 <- data %>% filter(CheckoutYear == 2023) %>% filter(MaterialType == "BOOK") 

books <- data %>% filter(MaterialType == "BOOK") %>% count(CheckoutYear)
risk <- maternaldf <- filter(RiskLevel == c("high risk", "mid risk", "low risk")) %>% count(RiskLevel)

book_checkouts_all <- books %>% summarize(total_books = sum(n)) %>% pull(total_books)

music <- data %>% filter(MaterialType == "MUSIC") %>% count(CheckoutYear)

music_checkouts_all <- music %>% summarize(total_music = sum(n)) %>% pull(total_music)

audiobooks <- data %>% filter(MaterialType == "AUDIOBOOK") %>% count(CheckoutYear)

audiobooks_checkouts_all <- audiobooks %>% summarize(total_audiobooks = sum(n)) %>% pull(total_audiobooks)

video_disc <- data %>% filter(MaterialType == "VIDEODISC") %>% count(CheckoutYear)

video_disc_checkouts_all <- video_disc %>% summarize(total_video_disc = sum(n)) %>% pull(total_video_disc)

sound_disc  <- data %>% filter(MaterialType == "SOUNDDISC") %>% count(CheckoutYear)

sound_disc_checkouts_all <- sound_disc %>% summarize(total_sound_disc = sum(n)) %>% pull(total_sound_disc)

ebook <- data %>% filter(MaterialType == "EBOOK") %>% count(CheckoutYear)
ebook_checkouts_all <- ebook %>% summarize(total_ebook = sum(n)) %>% pull(total_ebook)

magazine <-data %>% filter(MaterialType == "MAGAZINE") %>% count(CheckoutYear)

magazine_checkouts_all <- magazine %>% summarize(total_magazine = sum(n)) %>% pull(total_magazine)

#Line plot of books, audiobooks, video discs, sound discs,ebooks, and magazines every year from 2017- 2023
 ggplot()+
  geom_line(data= books, mapping = aes(x = CheckoutYear, y = n, color= "books"))+
  scale_x_continuous(limits = c(2017, 2023))+
  labs(title = "Material Types Checkouts Over the Years",
       x = "Checkout Year",
       y = "Total Checkouts")+
  geom_line(data= audiobooks, mapping =aes(x= CheckoutYear, y= n, color = "audiobooks"))+
    scale_x_continuous(limits = c(2017, 2023))+ 
    labs(title = "Material Types Checkouts Over the Years",
         x = "Checkout Year",
         y = "Total Checkouts")+
  geom_line(data= video_disc, mapping =aes(x= CheckoutYear, y= n, color = "video disc"))+
    scale_x_continuous(limits = c(2017, 2023))+
    labs(title = "Material Types Checkouts Over the Years",
         x = "Checkout Year",
         y = "Total Checkouts")+
  geom_line(data= sound_disc, mapping =aes(x= CheckoutYear, y= n, color = "sound disc"))+
    scale_x_continuous(limits = c(2017, 2023))+
    labs(title = "Material Types Checkouts Over the Years",
         x = "Checkout Year",
         y = "Total Checkouts")+
  geom_line(data= ebook, mapping =aes(x= CheckoutYear, y= n, color = "ebook"))+
    scale_x_continuous(limits = c(2017, 2023))+
    labs(title = "Material Types Checkouts Over the Years",
         x = "Checkout Year",
         y = "Total Checkouts")+
  geom_line(data= magazine, mapping =aes(x= CheckoutYear, y= n, color = "magazine"))+
    scale_x_continuous(limits = c(2017, 2023))+
    labs(title = "Material Types Checkouts Over the Years",
         x = "Checkout Year",
         y = "Total Checkouts")


#Chart: 2 It Ends With Us vs Layla checkouts in 2021 and 2022 
author_df <- data %>% 
  filter(str_detect(Creator, "Colleen")) %>% 
  filter(str_detect(Creator, "Hoover"))

hoover_books <- author_df %>% filter(MaterialType == "BOOK")

it_ends_with_us <-hoover_books %>% filter(str_detect(Title, "It ends with us")) 

it_ends_with_us_2021 <- it_ends_with_us %>% filter(CheckoutYear== 2021)

it_ends_with_us_2022 <- it_ends_with_us %>% filter(CheckoutYear== 2022)

layla_book <- hoover_books %>% filter(str_detect(Title, "Layla"))

layla_2021 <- layla_book %>% filter(CheckoutYear == 2021)

layla_2022 <- layla_book %>% filter(CheckoutYear == 2022)

ggplot()+
  geom_col(data= it_ends_with_us_2021, mapping= aes(x= CheckoutYear, y = Checkouts, color= "it ends with us 2021"))+ 
  labs(title="Checkouts of It ends with Us and Layla in 2021 and 2022")+
  geom_col(data=it_ends_with_us_2022, mapping= aes(x=CheckoutYear, y= Checkouts, color = "it ends with us 2022"))+
    labs(title="Checkouts of It ends with Us and Layla in 2021 and 2022")+
  geom_col(data= layla_2021, mapping= aes(x=CheckoutYear, y= Checkouts, color="layla 2021"))+
    labs(title="Checkouts of It ends with Us and Layla in 2021 and 2022")+
  geom_col(data = layla_2022, mapping = aes(x= CheckoutYear, y= Checkouts, color = "layla 2022"))+
    labs(title="Checkouts of It ends with Us and Layla in 2021 and 2022")


#Chart 3: Physical vs digital total checkouts in the years 2017-2023

physical <- data %>% filter(UsageClass == "Physical")


Digital <- data %>% filter(UsageClass == "Digital")


Physical_total <- data %>% filter(UsageClass== "Physical") %>% count(UsageClass)
Digital_Total <- data %>% filter(UsageClass == "Digital") %>% count(UsageClass)

ggplot()+geom_col(Physical_total, mapping = aes(x=UsageClass, y= n, fill= "Physical" ))+ 
  scale_y_continuous(limits= c(0, 470000))+
  labs(title= "Total Checkouts of Physical and Digital Classes from 2017- 2023",
       fill = "Legend",
       x= "UsageClass",
       y= "count")+
  geom_col(Digital_Total, mapping = aes(x = UsageClass, y = n, fill = "Digital"))

