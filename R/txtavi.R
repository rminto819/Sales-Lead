
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)

### import data ###
base <- read.csv("~/Textron Aviation/base.csv", header = TRUE, 
                stringsAsFactors = FALSE)   




# convert to numeric
for(i in 26:40) {
  base[ ,i] <- as.numeric(base[ ,i])                   
}   


### create purchase date
base <- mutate(base, pdate = paste("1", paste(month_purchased, year_purchased)))
base$pdate <- dmy(base$pdate)


base <- data.frame(cbind(base[ ,1], base$pdate, base[ ,2:ncol(base)]))   ### put id with pdate

names(base) <- c("id", "pdate", names(base)[3:ncol(base)])




####  filter only ids with two or more obs  ###
a <- base %>% 
  group_by(id) %>%
  summarize(l = length(id)) %>%
  filter(l >= 2)

b <- filter(base, id %in% a$id)

base <- b





#### calculate days between purchases ######

base <- mutate(base, last_purchase = NA)
base <- arrange(base, id, pdate)

base$last_purchase[2:nrow(base)] <- as.character(days(base$pdate[2:nrow(base)] - base$pdate[1:nrow(base) - 1]))

###  clean up and convert to numeric  ###
base$last_purchase <- word(base$last_purchase, 1)
base$last_purchase <- str_replace_all(base$last_purchase, "d", "")
base$last_purchase <- as.numeric(base$last_purchase)
base$last_purchase <- base$last_purchase / 86400
base$next_purchase <- base$next_purchase / 86400


###  next purchase  ###
base <- mutate(base, next_purchase = NA)
base$next_purchase[1:nrow(base) - 1] <- base$last_purchase[2:nrow(base)]






# calculate change in econ vars since last purchase
base <- mutate(base, rgdp_lp = NA, unrate_lp = NA, sp500_lp = NA, cpi_lp = NA)

base$rgdp_lp[2:nrow(base)] <- base$rgdp[2:nrow(base)] / base$rgdp[1:nrow(base)-1]

base$unrate_lp[2:nrow(base)] <- base$unrate[2:nrow(base)] / base$unrate[1:nrow(base)-1]
base$sp500_lp[2:nrow(base)] <- base$sp500[2:nrow(base)] / base$sp500[1:nrow(base)-1]



#####  last id/observation indicator  #####

base <- mutate(base, last_id = NA)
base <- arrange(base, id, pdate)


for(i in 1:nrow(base)) {
  base$last_id[i] <- ifelse(base$id[i] == base$id[i + 1], 0, 1)
}



base2 <- base[complete.cases(base), ]



###  first id indicator  ###
base2 <- mutate(base2, first_id = NA)
base2 <- arrange(base2, id, pdate)

for (i in 2:nrow(base2)) {
  base2$first_id[i] <- ifelse(base2$id[i] == base2$id[i - 1], 0, 1)
}



###  filter out first ids  ####

base3 <- filter(base2, first_id == 0)


#############  trailing average/median  ##########

is.even <- function(x) {
  x %% 2 == 0
}

base3 <- mutate(base3, trail_index = NA)    


### create index for the trailing average to reference
base3$trail_index[1] <- 1

for (i in 2:nrow(base3)) {
  base3$trail_index[i] <- ifelse(base3$id[i] == base3$id[i - 1], 
                                base3$trail_index[i-1] + 1, 1) 
}






base3 <- mutate(base3, trail_avg = NA)
base3 <- mutate(base3, trail_med = NA)


### create the trailing average

for(i in 2:nrow(base3)) {
  j <- i - base3$trail_index[i] + 1
  k <- i
  
  base3$trail_avg[i] <- rollmean(    
    base3$last_purchase[j:k], base3$trail_index[i], align = 'right'
    )
  
  base3$trail_med[i] <- rollmedian(base3$last_purchase[j:k], 
                                   ifelse(is.even(base3$trail_index[i]), 
                                          base3$trail_index[i] - 1, 
                                          base3$trail_index[i]), align = 'right')
  
}



base.train <- filter(base3, last_id == 0)


base.last <- filter(base3, last_id == 1)
base.last$next_purchase <- NA



####### time over median purchase ########
base.last <- mutate(base.last, now = word(now(), 1), time = NA)

base.last$now <- ymd(base.last$now)

base.last$time <- as.character(days(base.last$now - base.last$pdate))
base.last$time <- word(base.last$time, 1)
base.last$time <- str_replace_all(base.last$time, "d", "")
base.last$time <- as.numeric(base.last$time)

base.last <- mutate(base.last, time_over = time - trail_med)


base.hot <- filter(base.last, time_over >= -150, time_over <= 250)




####  minimum variance  ########

mv <- base3 %>%
  group_by(id) %>% 
  summarize(v = var(trail_avg), l = length(id)) %>%
  filter(l >= 3) %>%
  arrange(v)


hot <- filter(base.hot, id %in% mv$id[1:2000])


