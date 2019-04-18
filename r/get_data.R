

library(tidyverse)
library(RODER)


## query backup in case data needs to be updated
# 
# system_plan_raw <-  get_system_plan(query_start = "2018-01-01 00:00", query_end = "2019-01-01 00:00")
# 
# nps_prices_raw <- get_NPS_price(query_start = "2018-01-01 00:00", query_end = "2019-01-01 00:00")
# 
## save query results to RDS to get easy access 
# saveRDS(system_plan_raw,"data/2018/2018_sys_plan.RDS")
# saveRDS(nps_prices_raw,"data/2018/2018_nps_prices.RDS")


  



## read in 2018 data
system_plan <- readRDS("data/2018/2018_sys_plan.RDS")
nps_price <- readRDS("data/2018/2018_nps_prices.RDS")
real_losses <- readRDS("data/2018/losses.RDS")

## combine 2018 data for the basis of a model
total_data <- system_plan %>% 
  left_join(nps_price, by = "datetime") %>% 
  mutate(month = lubridate::month(datetime),
         week = lubridate::week(datetime),
         day = lubridate::day(datetime),
         weekday = lubridate::wday(datetime),
         hour = lubridate::hour(datetime)) %>% 
  select(-plan.frequency)

## Check planned losses vs AMR losses

plan_vs_amr <- total_data %>% 
  select(datetime, plan.losses, plan.consumption) %>% 
  left_join(real_losses[,c("datetime","AMR")], by = c("datetime")) %>% 
  mutate(difference = AMR-plan.losses,
         percentage = AMR/plan.consumption) %>% 
  filter(percentage < 1)

ggplot(plan_vs_amr)+
  geom_line(aes(x = datetime,y = AMR/plan.consumption))+
  geom_smooth(aes(x = datetime,y = AMR/plan.consumption))


## explore the data

ggplot(total_data) +
  geom_line(aes(x = hour, y = plan.losses, color = as.factor(day)))+
  facet_wrap(~month)+
  theme(legend.position = "none")


cor(total_data$plan.consumption,total_data$plan.losses)


ggplot(total_data)+
  geom_line(aes(x = datetime, y = plan.losses/plan.consumption))+
  scale_y_continuous(limits = c(0,0.1))
