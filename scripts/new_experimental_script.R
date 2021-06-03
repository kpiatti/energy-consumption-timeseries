hms_yrs07_09 <- hms_yrs07_09 %>% 
  mutate(min = as.numeric(min)) 

glimpse(hms_yrs07_09)

hist(hms_yrs07_09$min)
# more obs. with min value = 0 and fewer obs. with min = 59
count(hms_yrs07_09)
#1,569,894

hms_yrs07_09 %>% count(min == c(0, 10, 20, 30, 40, 50))
# obs. with those min values = 23,108


# each min 0-59 is in about 26,165 obs
hms_yrs07_09 %>% count(min)

#this method worked to select the right number of obs
hms_yrs07_09 %>% 
  filter(min %in% c(0, 10, 20, 30, 40, 50))



#this did not work, have to define minute object first
yrs2007_2009 %>% 
  filter(minute(yrs2007_2009$date_time %in% c(0, 10, 20, 30, 40, 50)))




################### BINGO ###################
minute <- minute(TSyrs_long$date_time)

TSyrs_long %>% 
  filter(minute %in% c(0, 10, 20, 30, 40, 50))


yrs_tsb <- yrs2007_2009 %>% 
  pivot_longer(names_to = "submeter",
               values_to = "reading",
               submeter1:submeter3) %>%
  as_tsibble(key = submeter, index = date_time)



############# filling gaps in tsibble
yrs_tsb_min %>% 
  fill_gaps()

test <- yrs_tsb_min %>% 
  select(-date, -time) %>% 
  fill_gaps()

unique(test$reading)

test %>% gg_season(reading)


yrs_tsb %>% 
  ggplot(aes(x = date_time, y = reading, color = submeter)) +
  geom_line() +
  facet_wrap(~ Length, scales = "free_y", nrow = 2)

yrs_tsb_min %>% 
  as_tibble(
    key = submeter,
    index = date_time,
    regular = FALSE)
  


################# FILTERING OUT DATES/TIMES########
yrs_tsb %>% 
  filter_index("2007" )

wkdays <- weekdays(yrs_tsb$date_time)
week <- week(yrs_tsb$date_time)

# these are some lines of code i found on stack over
# colnames(Calendar30mn) <- "DateTime"
# Calendar30mn$Time <- strftime(Calendar30mn$DateTime, format="%H:%M:%S")
# Calendar30mn$DayWk <- weekdays(Calendar30mn$DateTime)
# Calendar30mn$WkNumber <- week(Calendar30mn$DateTime)


test <- yrs2007_2009 


test$wday <- weekdays(test$date_time)
test <- test %>% 
  separate("date", c("year", "month", "day"))

test <- test %>% 
  separate("time", c("hour", "min", "sec"))
           
glimpse(test)


################## FITER DATETIME WITH LUBRIDATE OPERATORS

yrs_tsb %>%
  filter_index("2007")

yrs_tsb %>%
  filter(day(date_time) %in% 07)

yrs_tsb %>% 
  filter(weekdays(date_time) %in% "Tuesday")

yrs_tsb %>% 
  filter(minute(date_time) %in% c(0, 10, 20, 30, 40))







################### trying to fill gaps without introducing gaps in index in tsibble

yrs_tsb2 <- yrs2007_2009 %>% 
  pivot_longer(names_to = "submeter",
               values_to = "reading",
               kitchen:AC) %>%
  as_tsibble(key = submeter,
                index = date_time,
                validate = TRUE) %>% 
  fill_gaps(reading = mean(reading))

# this allowed me to aggregate to 1-hour intervals
yrs_tsb3 <- yrs_tsb2 %>% 
  group_by_key() %>% 
  index_by(datetime4 = ~lubridate::floor_date(., "1 hour")) %>% 
  summarise(avg_read = mean(reading))

# this allowed me to aggregate to 1 day intervals
yrs_tsb4 <- yrs_tsb2 %>% 
  group_by_key() %>% 
  index_by(day = ~lubridate::as_date(.)) %>% 
  summarise(avg_read = mean(reading))

yrs_tsb1 <- yrs_tsb %>% 
  group_by_key() %>%
  index_by(date_time) %>% 
  fill_gaps() %>%
  tidyr::fill(reading, .direction = "down")
# above code filled the gaps, but when i tried to create a gg_seanson plot, it ran for 20 mins without producing

yrs_tsb3 <- yrs_tsb %>% 
  index_by(date_time) %>% 
  fill_gaps(reading = 0)


# this worked and when i compared the daily average seasonality graph for 2008 with and without gaps filled with 0, they were the same 
yrs_tsb4 <- yrs_tsb %>% 
  fill_gaps(reading = 0)



# # gaps as default `NA`
# fill_gaps(harvest, .full = TRUE)
# fill_gaps(harvest, .full = start())
# fill_gaps(harvest, .full = end())
# full_harvest <- fill_gaps(harvest, .full = FALSE)
# full_harvest

y

s######################## REDUCE GRANULARITY
# create obj representing minute element of date_time var
minute <- minute(yrs_tsb$date_time)

yrs_tsb_min <- yrs_tsb %>% 
  filter(minute %in% c(0, 10, 20, 30, 40, 50))




################ fill_gaps ####################
# i was just trying to figure out how the results of these commands differ. i still don't know the answer

test <- yrs_tsb %>% fill_gaps()

test2 <- yrs_tsb %>% fill_gaps(.full = TRUE)





################# hms( ) ################
# this command works differently than i expected/wanted. instead of preseving the 00:00:00 format, it turns the times into a single integer, representing number of second (e.g. 00:01:00 becomes 60s)
df$times <-  hms(df$time)



############################ Multiple time series
# experiment to see what happens if i don't pivot_longer when creating tsibble-- i could not get visualizations or summary stats to work right when i left the table in wide format. the reason i was trying to do this is that i read in the time series book that tsibbles can represent multiple time series objects...and i thought leaving it as a wide df might make it run faster. while working on the energy project r studio kept crashing and all my visualizations and models were taking FOREVER 
df_tsbl <- df %>% 
  select(-date, -time) %>% 
  as_tsibble(key = c('times', 'kitchen', 'laundry', 'AC'), index = date_time)






################### period()########################
#here i was trying to figure out how the period( ) function from lubridate works and seeing if i coud use it to filter time periods for vizualizing and forcasting time series data...i couldn't get it to work right and decided i needed to move on for now. 
yrs_tsb %>% 
  filter(period())


#plot that worked but i decided to cut out of my markdown b/c it didn't show anything useful. 
yrs_tsb %>% 
  filter(year(date_time) == "2009",
         day(date_time) %in% c(01, 05, 10, 15, 20, 25, 30),
         minute(date_time) %in% c(0, 10, 20, 30, 40, 50)) %>% 
  ggplot(aes(x = date_time, y = reading, color = submeter)) +
  geom_line()



#################### FORECASTING

# fit an exponential smoothing model (ETS)
fbl_sm3 <- submeter3_ac %>% model(ETS(reading))
# above model worked, but took several minutes to run

#get information on the fbl_sm3 model
components(fbl_sm3)

new_yrstsb <- yrs2007_2009 %>% 
  select(-date, -time) %>%  
  pivot_longer(names_to = "submeter",
               values_to = "reading",
               kitchen:AC) %>%
  mutate(sub_daily = ymd_hms(date_time)) %>% 
  as_tsibble(key = submeter, index = sub_daily)


# can i use the same the same var name in mutate
new_yrs <- yrs2007_2009 %>% 
  select(-date, -time) %>%  
  pivot_longer(names_to = "submeter",
               values_to = "reading",
               kitchen:AC) %>%
  mutate(date_time = ymd_hms(date_time)) %>% 
  as_tsibble(key = submeter, index = date_time)
# that does work and doesn't have the Group[3] in the tsibble, which i don't know what it for or means

#remove gaps from new_yrs
new_yrs <- new_yrs %>%
  group_by_key() %>%
  fill_gaps() %>%
  tidyr::fill(reading, .direction = "down")

#testing plots with new_yrs
new_yrs %>% 
  group_by_key() %>% 
  index_by(yearweek = yearweek(date_time)) %>% 
  summarize(avg_reading = mean(reading)) %>% 
  autoplot(avg_reading) +
  ggtitle("Weekly Average Reading")+
  labs(x = " ", y = "Submeter Reading (watt-hour)")


new_yrs %>% 
  filter(minute(date_time) %in% c(0, 10, 20, 30, 40, 50)) %>% 
  filter_index("2008-01-09") %>% 
  autoplot(reading)+
  labs(title = "Submeter Readings for January 9, 2008",
       x = "",
       y = "Reading (Watt/Hour)")


new_yrs %>% 
  filter(weekdays(date_time) %in% "Monday") %>% 
  filter(hour(date_time) %in% 20) %>%
  filter(minute(date_time) %in% 01) %>% 
  filter(submeter == "AC") %>% 
  autoplot(reading) +
  labs(title = 'AC Submeter Readings for Mondays at 8PM',
       x = "", y = 'Reading (watt-hours)')

nsm3_mon8pm <- yrs_tsb %>%
  filter_index(weekdays(date_time) %in% "Monday",
               hour(date_time) %in% 20,
               minute(date_time) %in% 01) %>% 
  filter(submeter =="AC")



############# convert to tibble time
tbbltime <- yrs2007_2009 %>% 
  select(-date, -time) %>%  
  pivot_longer(names_to = "submeter",
               values_to = "reading",
               kitchen:AC)

tbbltime <- as_tbl_time(tbbltime, index = date_time)

as_period(tbbltime, "hourly")
 

yrwk <- yrs_tsb %>%
  group_by_key() %>% 
  index_by(year_week = yearweek(date_time)) %>% 
  summarize(avg_reading = mean(reading))

new_yrs_tsb <- yrs2007_2009 %>% 
  mutate(date = as_date(date),
         time = hms(time)) %>%  
  pivot_longer(names_to = "submeter",
               values_to = "reading",
               kitchen:AC) %>%
  as_tsibble(key = submeter, index = date_time)






###################### ROLLING AVERAGE ######################
# during my research for the submeter plots, i can across some code to plot rolling daily covid 19 avg. and tried it out. i got it to work pretty easily. the code is below

#create tsibble of rolly daily avg readings
roll_day_avg <- daily_avg_07 %>% 
  group_by_key() %>%
  mutate(roll_avg = slide_index_dbl(.i = day,
                                    .x = day_avg,
                                    .f = mean,
                                    .before = 6))


roll_day_avg %>% ggplot(aes(x = day,
                            y = roll_avg,
                            colour = submeter)) + 
  geom_line()



## Create New Tsibbles



# create tsibble of daily avgs for 2007
daily_avg_07 <- yrs_tsb %>% 
  filter(year(date_time) == '2007') %>%
  group_by_key() %>% 
  index_by(day = ~lubridate::as_date(.)) %>% 
  summarise(day_avg = mean(reading))



# create tsibble of daily avgs for 2008
daily_avg_08 <- yrs_tsb %>% 
  filter(year(date_time) == '2008') %>%
  group_by_key() %>% 
  index_by(day = ~lubridate::as_date(.)) %>% 
  summarise(day_avg = mean(reading))



# create tsibble of daily avgs for 2009
daily_avg_09 <- yrs_tsb %>% 
  filter(year(date_time) == '2009') %>%
  group_by_key() %>% 
  index_by(day = ~lubridate::as_date(.)) %>% 
  summarise(day_avg = mean(reading))


s3_1x_wk01 <-  yrs2007_2009 %>% 
  filter(weekdays(date_time) %in% "Monday") %>% 
  filter(hour(date_time) %in% 20) %>%
  filter(minute(date_time) %in% 01) %>% 
  pivot_longer(names_to = "submeter",
               values_to = "reading",
               kitchen:AC) %>% 
  select(date, submeter, reading) %>% 
  filter(submeter == "AC") %>% 
  mutate(date = as_date(date)) %>% 
  as_tsibble(key = submeter, index = date, validate = TRUE)
