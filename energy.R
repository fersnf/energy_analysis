####libraries####
library("dplyr")
library("tidyr")
library("lubridate")
library("readr")
library("ggplot2")
library("ggthemes")
library("reshape2")

####opening the file####
consumption <- read_delim(
  "household_power_consumption.txt", ";",
  escape_double = FALSE,
  col_types = cols(
    Date = col_character(),
    Global_active_power = col_number(),
    Global_intensity = col_number(),
    Global_reactive_power = col_number(),
    Sub_metering_1 = col_number(),
    Sub_metering_2 = col_number(),
    Sub_metering_3 = col_number(),
    Time = col_character(),
    Voltage = col_number()
  ),
  trim_ws = TRUE
)

#converting to data frame
consumption <- as.data.frame(consumption)

####date and time####
#creating DateTime column
consumption <- cbind(consumption, paste(consumption$Date, consumption$Time))
colnames(consumption)[10] <- "DateTime"
consumption <- consumption[, c(ncol(consumption), 1:(ncol(consumption) - 1))]
# head(consumption)

#converting date and time format
consumption$DateTime <- strptime(consumption$DateTime, "%d/%m/%Y %H:%M:%S")

#changing the str to a time variable
consumption$DateTime <- as.POSIXct(consumption$DateTime, tz = "GMT")
consumption$Date <- as.Date(consumption$Date, "%d/%m/%Y", tz = "GMT")

####renaming columns####
names(consumption)
names(consumption)[4:5] <- c("ActivePower", "ReactivePower")
names(consumption)[7] <- "Intensity"
names(consumption)[8:10] <- c("Kitchen", "Laundry", "HVAC")

####active energy####
#active energy consumed per minute (watt hour) in the household
#by electrical equipment not measured in the sub-meterings 1, 2 and 3
consumption$Others <- (consumption$ActivePower * 1000 / 60) -
  (consumption$Kitchen + consumption$Laundry + consumption$HVAC)

# #converting kilo watts in to watt hours
consumption$ActiveConvert <- consumption$ActivePower * 1000 / 60

####mutating####
#changing the language of weekdays to english
Sys.setlocale("LC_TIME", "English")

#separating month, year, date, etc
consumption <- consumption %>%
  mutate(
    MonthYear = format(DateTime, "%Y/%m"),
    Year = format(DateTime, "%Y"),
    Month = format(DateTime, "%m"),
    Day = format(DateTime, "%d"),
    Hour = format(DateTime, "%H:%M:%S"),
    Weekday = weekdays(DateTime, abbreviate = T),
    Week = week(DateTime),
    WeekYear = paste(format(DateTime, "%Y"), format(week(DateTime))),
    wdayMonth = paste(format(DateTime, "%m"), format(wday(DateTime))),
    Quarter = paste(format(DateTime, "%Y"), quarter(DateTime))
  )

#ordering month
consumption$Month <- factor(consumption$Month)
levels(consumption$Month) = c("Jan", "Fev", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

#ordering weekdays
consumption$Weekday <- factor(consumption$Weekday)
consumption$Weekday <- ordered(consumption$Weekday,
                               levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat",  "Sun"))

####removing columns####
consumption$Time <- NULL
consumption$ActivePower <- NULL

#renaming the column back
names(consumption)[which(names(consumption) == "ActiveConvert")] <- "ActivePower"

#removing 2006 (because it's only the last 15 days of the year / holiday time)
consumption_clean <- consumption %>% dplyr::filter(year(DateTime) != 2006)

####seasons####
#creating a function to add seasons to the df
seasons <- function(x){
  if(x %in% 3:5) return("Spring")
  if(x %in% 6:8) return("Summer")
  if(x %in% 9:11) return("Fall")
  if(x %in% c(12, 1, 2)) return("Winter")
}

#applying the function
consumption_clean$Season = sapply(month(
  consumption_clean$DateTime), seasons)

#creating season/year column
consumption_clean <- consumption_clean %>%
  mutate(SeasonYear = paste(
    format(consumption_clean$DateTime, "%Y"),
    consumption_clean$Season
  ))

####> aggregating####
#global active power (average) + in watt hours
####by year####
consYear <- aggregate(
  consumption_clean$ActivePower,
  by = list(consumption_clean$Year),
  FUN = function(x)
    mean(x, na.rm = T)
)

names(consYear) <- c("Year", "ActivePower")
head(consYear)

####by month####
consMonth <- aggregate(
  consumption_clean$ActivePower,
  by = list(consumption_clean$Month),
  FUN = function(x)
    mean(x, na.rm = T)
)

names(consMonth) <- c("Month", "ActivePower")
head(consMonth)

####by month/year####
consYM <- aggregate(
  consumption_clean$ActivePower,
  by = list(consumption_clean$MonthYear),
  FUN = function(x)
    mean(x, na.rm = T)
)

names(consYM) <- c("YearMonth", "ActivePower")
head(consYM)

####by weekday####
consWeekday <- aggregate(
  consumption_clean$ActivePower,
  by = list(consumption_clean$Weekday),
  FUN = function(x)
    mean(x, na.rm = T)
)

names(consWeekday) <- c("Weekday", "ActivePower")
head(consWeekday)

####by season####
consSeason <- aggregate(
  consumption_clean$ActivePower,
  by = list(consumption_clean$Season),
  FUN = function(x)
    mean(x, na.rm = T)
)

names(consSeason) <- c("Season", "ActivePower")
head(consSeason)

####> grouping by####
#sum of energy consumption in kWh
####by hour####
consHourly <- consumption_clean %>%
  group_by(Hour = hour(DateTime),
           Date,
           Year,
           Month,
           Day,
           Weekday,
           Week,
           WeekYear,
           MonthYear,
           wdayMonth,
           Season,
           SeasonYear,
           Quarter) %>%
  summarise(
    Kitchen = sum(Kitchen, na.rm = TRUE)/1000,
    Laundry = sum(Laundry, na.rm = TRUE)/1000,
    HVAC = sum(HVAC, na.rm = TRUE)/1000,
    Others = sum(Others, na.rm = TRUE)/1000,
    ActivePower = sum(ActivePower, na.rm = TRUE)/1000
  )

names(consHourly)
class(consHourly)
consHourly <- as.data.frame(consHourly)
head(consHourly)

####by weekday/month####
#in kw
consWM <- consHourly %>%
  group_by(wdayMonth) %>%
  summarise(
    Kitchen = mean(Kitchen, na.rm = TRUE)/1000,
    Laundry = mean(Laundry, na.rm = TRUE)/1000,
    HVAC = mean(HVAC, na.rm = TRUE)/1000,
    Others = mean(Others, na.rm = TRUE)/1000,
    ActivePower = mean(ActivePower, na.rm = TRUE)/1000
  )

####by day####
consDays <- consumption_clean %>%
  group_by(Year, Month, Season, Weekday) %>%
  summarise(
    Kitchen = sum(Kitchen, na.rm = TRUE)/1000,
    Laundry = sum(Laundry, na.rm = TRUE)/1000,
    HVAC = sum(HVAC, na.rm = TRUE)/1000,
    Others = sum(Others, na.rm = TRUE)/1000,
    ActivePower = sum(ActivePower, na.rm = TRUE)/1000
  )

####by year####
consRoomYear <- consumption_clean %>%
  group_by(Year) %>%
  summarise(
    Kitchen = sum(Kitchen, na.rm = TRUE)/1000,
    Laundry = sum(Laundry, na.rm = TRUE)/1000,
    HVAC = sum(HVAC, na.rm = TRUE)/1000,
    Others = sum(Others, na.rm = TRUE)/1000,
    ActivePower = sum(ActivePower, na.rm = TRUE)/1000
  )