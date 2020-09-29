library(tidyverse)
library(readr)
library(stringr)
library(ggrepel)
library(ggthemes)
library(scales)

santiago = 230  
rest = 120 

muni = data.frame(area = c('Santiago', 'Rest'), liters = c(230, 120))



df1 <- data.frame(name = c("George", "Stan", "Carly"),  
                  age  = c(75,15,31),  
                  retired = c(T,F,F)) 
