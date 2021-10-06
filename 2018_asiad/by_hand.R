# Source:
# http://www.ocagames.com/orb/files/4/104/AG2018_OfficialResultBook_Sport%20Climbing_v1.0.pdf

library(tidyverse)
library(pdftools)
txt <- pdf_text("data/asiad_2018/climbing_asiad_2018.pdf")
# each index is a page

# 


# quick look at correlations
# men's qualification

rank <- 1:23

speed <- c(6, 10, 9, 17, 5, 3,
           1, 2, 8, 4, 11, 13, 15, 12, 19,
           7, 16, 14, 18, 20, 22, 21, 23)

bould <- c(1, 2, 3, 6, 8, 13,
           19, 14, 4, 9, 7, 12, 5, 10, 11,
           16, 17, 18, 15, 20, 22, 22, 22)

lead <- c(1, 4, 3, 2, 7, 8,
          18, 14, 13, 16, 10, 5, 11, 9, 6,
          15, 12, 17, 21, 22, 19, 20, 23)

cor(cbind(rank, speed, bould, lead), method = "kendall")

# women's qualification

rank <- 1:20
speed <- c(8, 6, 11, 1, 5, 9, 12, 3, 2, 4,
           10, 16, 7, 15, 14, 13, 17, 20, 18, 19)
bould <- c(2, 1, 3.5, 11, 6, 7, 3.5, 9, 15, 14, 10,
           5, 13, 8, 12, 16, 17, 18, 19.5, 19.5)
lead <- c(1, 3, 2, 16, 8, 4, 6, 11, 12, 7, 5,
          9, 10, 13, 14, 15, 18, 17, 19, 20)

cor(cbind(rank, speed, bould, lead), method = "kendall")

