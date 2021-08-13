# 4 Data Wrangle: dates / times (lubridate & hms)

rm(list = ls())
graphics.off()

# Install packages and load package
install.packages("lubridate")
install.packages("hms")  
install.packages("nycflights13") # nycflights - stored as separate tables

library(tidyverse)
library(lubridate)
library(hms)
library(nycflights13)



# 4.2 Create dates / times

# Basic objects
d <- as_date(18992) # date
d

t <- as_hms(120) # time
t

dt <- as_datetime(1640952000) # datetime
dt


# Parsing date/time: string or number converson...

ymd_hms("2021-12-31 12:00:00") # %>% class()
ymd_hm("2021-12-31 12:00")
ymd_h("2021-12-31 12")

ydm_hms("2021-31-12 11:30:00")
ydm_hm("2021-31-12 11:30")
ydm_h("2021-31-12 11")

mdy_hms("12/31/2021 3:05:05")

dmy_hms("31 Dec 2021 22/15:00")

ymd("20211231")

mdy("December 13st 2021")

dmy("31st of December 21")

yq("2021: Q4")

hms::hms(seconds = 5, minutes = 1, hours = 0)
lubridate::hms("00:01:05")


# date_decimal() - Parse date stored as decimal number
d <- seq(2021,2022,0.25)
d
date_decimal(d)


# fast_strptime() - Parse datetime
fast_strptime(x = "2021-12-31 12:00:00", format = "%Y-%m-%d %H:%M:%S")


# parse_date_time() - Easier parse datetime
parse_date_time("2021-12-31 12:00:00", "ymd HMS")


# Create date/time from individual components

flights

#   Create datetime and date column using other components
flights %>% 
  select(year, month, day, hour, minute) %>%  # components
  mutate(datetime = make_datetime(year, month, day, hour, minute), # datetime create
         date = make_date(year, month, day))  # date create


# Create date/time from existing objects

#   Current timestamp and todays date
now()
today()

#   Convert between datetime and date
as_date(now())
as_datetime(today())


# Different codes for datetimes components:

# Code  Value
#-------------------------------------------#
# %d    Day of the month (decimal number)
# %a    Abbreviated weekday
# %m    Month (decimal number)
# %A    Full weekday
# %b    Month (abbreviated)
# %I    Decimal hour (12 hour)
# %B    Month (full name)
# %j    Decimal day of the year
# %y    Year (2 digits)
# %w    Decimal Weekday (0=Sunday)
# %Y    Year (4 digits)
# %W    Decimal week of the year (starting on Monday)
# %H    Decimal hour (24 hour)
# %p    Locale-specific AM/PM
# %M    Decimal minute
# %x    Locale-specific Date
# %S    Decimal second
# %X    Locale-specific Time



# 4.3 Components

# Extract different components of current time stamp

dt <- now()
dt

#   Extract each piece of datetime
year(dt)
month(dt)
day(dt)
hour(dt)
minute(dt)
second(dt)

#   Some additional components
isoyear(dt)
epiyear(dt)
wday(dt)
qday(dt)
week(dt)
isoweek(dt)
epiweek(dt)
quarter(dt)
semester(dt)

#   Logicals
am(dt)
pm(dt)
dst(dt)
leap_year(dt)

#   Store value of a component into column
flights %>% 
  select(year, month, day, hour, minute) %>%  # components
  mutate(datetime = make_datetime(year, month, day, hour, minute)) %>%  # datetime create
  # extract week day, week and quarter
  mutate(wday = wday(datetime),
         week = week(datetime),
         Q = quarter(datetime))



# 4.4 Rounding values & setting component

# Rounding dates per month level
d <- today()
d

floor_date(d, unit = "month")   # round down to previous month
ceiling_date(d, unit = "month") # round up to next month
round_date(d, unit = "month")   # mathematical rules for rounding
rollback(d) # rollback to last day of previous month

# It also works for other units
floor_date(d, unit = "year")
ceiling_date(dt, unit = "day")
round_date(dt, unit = "minute")


# Updating components

#   Update each component  by assigning new values
dt
year(dt) <- 2022
month(dt) <- 12
day(dt) <- 31
hour(dt) <- 23
minute(dt) <- 59
second(dt) <- 59

#   Update all components in one take with update()
update(dt, year = 2022, month = 12, day = 31, hour = 23, minute = 59, second = 59)

#   Too great values rollback!
update(dt, month = 13) # 12 + 1 months = 1 year + 1 month
update(dt, hour = 25)  # 24 + 1 hour = 1 day  + 1 hour



# 4.5 Date-times arithmetics and durations


# Some basic date/time arithmetics

today <- Sys.Date()
today
today + 1 # tomorrow
today - 1 # yesterday

now <- Sys.time()
now
now + 3600 # after 1 hour
now - 3600 # before 1 hour

#   How old are you?
birth_date <- ymd("1987-05-28")
age <- today - birth_date
age


# Durations 

#   Convert age to duration

as.duration(age) # in seconds


# Durations constructor functions

x <- 1 # number of seconds

dyears(x)
dmonths(x)
dweeks(x)
ddays(x)
dhours(x)
dminutes(x)
dpicoseconds(x)

is.duration(age)
is.duration(as.duration(age))


# Durations - arithmetics

dseconds(10) + dminutes(1) # addition
dyears(1) - dweeks(27)     # subtraction
10 * dmonths(1)            # multiplication


# Inconsistent timeline behaviour (durations)

#   Daylight Savings Time
dt <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")
dt + ddays(1)

#   Leap year
dt <- ymd_hms("2019-02-28 23:00:00")
dt +  dyears(1)



# 4.6 Periods

#   Age as period
as.period(age)

#   Constructor functions
seconds(3600)
minutes(60)
hours(1)
days(1)
months(12)
weeks(54)
years(1)

period_to_seconds(years(1))
seconds_to_period(3600)

period(3600, units = "second")


# Periods - arithmetics

seconds(10) + minutes(1) # addition
years(1) - weeks(27)     # subtraction
10 * months(1)           # multiplication


# Inconsistent timeline behaviour (periods)

#   Daylight Savings Time
dt <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")
dt + days(1)

#   Leap year
dt <- ymd_hms("2019-02-28 23:00:00")
dt +  years(1)



# 4.7 Intervals

# Create an interval
d1 <- ymd("2021-12-30")
d2 <- ymd("2021-12-31")

i1 <- interval(d1, d2)
i2 <- d2 %--% d1
i1
i2

# Extract boundaries
int_start(i1)
int_end(i1)

# Is time point within given interval
ymd_hms("2021-12-30 01:00:00") %within% i1
ymd_hms("2021-12-29 23:00:00") %within% i1

# Do intervals overlap
int_overlaps(i1, i2)
int_overlaps(i1, ymd("2021-12-28") %--% ymd("2021-12-29"))

# Create intervals form vector of dates
dates <- now() + days(1:365) # one year of dates
int_diff(dates) # daily intervals

# Length of an interval / flip interval
i1
int_flip(i1)
int_length(i1)



# 4.8 Time zones

# What R sees as your time zone?
Sys.timezone()

# Different time zones
OlsonNames()

#   How many different time zones
OlsonNames() %>% length()

#   "US" ~ "Europe" included in TZ
OlsonNames() %>% str_subset(pattern = "US")
OlsonNames() %>% str_subset(pattern = "Europe")
