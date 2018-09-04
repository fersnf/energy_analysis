####> scatter plots####
#global active power (all years)
####by year####
ggplot(data = consYear,
       aes(x = Year, y = ActivePower, group = 1)) +
  geom_point(color = "navy",
             shape = 20,
             size = 3) +
  ylab("Global Active Power (kWh)") + xlab("Year") +
  ggtitle("Avg. Global Active Power (kWh) per Year") +
  theme(plot.title = element_text(hjust = 0.5))

####by month####
ggplot(data = consMonth,
       aes(x = Month, y = ActivePower, group = 1)) +
  geom_point(color = "navy",
             shape = 20,
             size = 3) +
  ylab("Global Active Power (kWh)") + xlab("Month") +
  ggtitle("Avg. Global Active Power (kWh) per Month") +
  theme(plot.title = element_text(hjust = 0.5))

####by weekday####
ggplot(data = consWeekday,
       aes(x = Weekday, y = ActivePower, group = 1)) +
  geom_point(color = "navy",
             shape = 20,
             size = 3) +
  ylab("Global Active Power (kWh)") + xlab("Weekday") +
  ggtitle("Avg. Global Active Power (kWh) per Weekday") +
  theme(plot.title = element_text(hjust = 0.5))

####by year/month####
ggplot(data = consYM,
       aes(x = YearMonth, y = ActivePower, group = 1)) +
  geom_line(color = "gray") +
  geom_point(color = "navy") +
  ylab("Global Active Power (kWh)") + xlab("Year/Month") +
  ggtitle("Avg. Global Active Power (kWh) per Year/Month") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )

####by hour####
#January/2007
ggplot(
  data = consHourly %>% group_by(Month, Year, Hour) %>%
    summarise(mean_ActivePower = mean(ActivePower)) %>%
    filter(Month == "Jan", Year == 2007),
  aes(x = factor(Hour), y = mean_ActivePower, group = 1)
)  + geom_line(color = "gray") +
  geom_point(color = "navy") +
  ylab("Global Active Power (kWh)") + xlab("Hour") +
  ggtitle("Avg. Global Active Power (kWh) per Hour - Jan/2007") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text())

####by season####
ggplot(data = consSeason,
       aes(x = Season, y = ActivePower, group = 1)) +
  geom_point(color = "navy",
             shape = 20,
             size = 3) +
  ylab("Global Active Power (kW)") + xlab("Season") +
  ggtitle("Global Active Power per Season") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text())

####> line plots####
####by month/year####
ggplot(consYM, aes(
  x = factor(YearMonth),
  y = ActivePower,
  group = 1
)) +
  geom_line() +
  ylab("Global Active Power (kWh)") + xlab("Month/Year") +
  ggtitle("Global Active Power (kWh) per month/year") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )

####by week/year####
#from time series
ggplot(ActiveWY, aes(
  x = factor(WeekYear),
  y = ActivePower,
  group = 1
)) +
  geom_line() +
  ylab("Global Active Power (kWh)") + xlab("Week/Year") +
  ggtitle("Global Active Power (kWh) per week/year") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )

####by weekday/month####
ggplot() + geom_line(data = consWM, aes(
  x = wdayMonth,
  y = Kitchen,
  group = 1,
  colour = "Kitchen"
)) +
  geom_line(data = consWM, aes(
    x = wdayMonth,
    y = Laundry,
    group = 1,
    colour = "Laundry"
  )) +
  geom_line(data = consWM, aes(
    x = wdayMonth,
    y = HVAC,
    group = 1,
    colour = "HVAC"
  )) +
  geom_line(data = consWM, aes(
    x = wdayMonth,
    y = ActivePower,
    group = 1,
    colour = "Active Power"
  )) +
  geom_line(data = consWM, aes(
    x = wdayMonth,
    y = Others,
    group = 1,
    colour = "Others"
  )) +
  ylab("Consumption (kWh)") + xlab("Weekday/Month") +
  ggtitle("Avg. Energy Consumption (kWh) per weekday/month") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  ) +
  scale_color_manual(
    name = "Legend",
    values = c(
      "Kitchen" = "blue",
      "Laundry" = "red",
      "HVAC" = "#008744",
      "Active Power" = "#ffa700",
      "Others" = "#800080"
    )
  )

####by days/season####
daySeason <-
  consumption_clean[which(
    as.character(consumption_clean$Date) %in%
      c("2008-01-15", "2008-03-11", "2008-06-17", "2008-09-16")),
    c("DateTime", "ActivePower", "Kitchen", "Laundry", "HVAC", "Others")]

consDaySeason <- daySeason %>%
  group_by(hour(DateTime),
           as.Date(DateTime)) %>%
  summarise(
    Kitchen = sum(Kitchen, na.rm = TRUE)/1000,
    Laundry = sum(Laundry, na.rm = TRUE)/1000,
    HVAC = sum(HVAC, na.rm = TRUE)/1000,
    Others = sum(Others, na.rm = TRUE)/1000,
    ActivePower = sum(ActivePower, na.rm = TRUE)/1000
  )

names(consDaySeason)[1:2] <- c("Hour", "Date")
# mean(consDaySeason$ActivePower)*24*365

consDaySeason <- as.data.frame(consDaySeason)

head(consDaySeason)

ggplot() + geom_line(data = consDaySeason, aes(
  x = factor(Hour),
  y = Kitchen,
  group = 1,
  colour = "Kitchen"
)) +
  geom_line(data = consDaySeason, aes(
    x = factor(Hour),
    y = Laundry,
    group = 1,
    colour = "Laundry"
  )) +
  geom_line(data = consDaySeason, aes(
    x = factor(Hour),
    y = HVAC,
    group = 1,
    colour = "HVAC"
  )) +
  geom_line(data = consDaySeason,
            aes(
              x = factor(Hour),
              y = ActivePower,
              group = 1,
              colour = "ActivePower"
            )) +
  geom_line(data = consDaySeason, aes(
    x = factor(Hour),
    y = Others,
    group = 1,
    colour = "Others"
  )) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(
     vjust = - 0.5,
      hjust = 1
    )
  ) +
  scale_color_manual(
    name = "Legend",
    values = c(
      "Kitchen" = "blue",
      "Laundry" = "red",
      "HVAC" = "#008744",
      "ActivePower" = "#ffa700",
      "Others" = "#800080"
    )
  ) +
  facet_wrap( ~ Date) +
  ylab("Consumption") + xlab("Hour") +
  ggtitle("Energy Consumption per Hour (on Tuesdays)") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(
      angle = 0,
      vjust = 0.5,
      hjust = 1
    )
  )


####> bar plot####
####by weekday####
ggplot(data = consHourly, aes(x = factor(Weekday), y = ActivePower)) +
  geom_bar(stat = "identity") +
  ylab("Global Active Power (kWh)") + xlab("Weekday") +
  ggtitle("Avg. Global Active Power (kWh) per Weekday/Year") + facet_wrap( ~ Year) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(
      angle = 45,
      vjust = 0.5,
      hjust = 1
    )
  )

####by year####
ggplot(data = consHourly,
       aes(x = factor(Season), y = ActivePower)) +
  geom_bar(stat = "identity") +
  ylab("Global Active Power (kWh)") + xlab("Season") +
  ggtitle("Avg. Global Active Power (kWh) per Season/Year") + facet_wrap( ~ Year) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(
      angle = 45,
      vjust = 0.5,
      hjust = 1
    )
  )

####> stacked bar####
#kitchen, laundry and HVAC
####by season/year####
names(consDays)

#selecting year, season, kitchen to active power
selSeason <- consDays[c(1, 3,5:8)] #selecting the columns I want
plotSeason <- melt(selSeason, id.vars = c("Season", "Year"))

ggplot(plotSeason, aes(x = factor(Season), y = value, fill = variable)) +
  geom_bar(stat= "identity") + facet_wrap(~Year) + 
  ylab("Consumption (kWh)") + xlab("Season") +
  ggtitle("Avg. Energy Consumption (kWh) per Season/Year") +
  labs(fill = "Legend:") +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

####by season####
selSeason2 <- consDays[c(3,5:8)] #selecting the columns I want
plotSeason2 <- melt(selSeason, id.vars = c("Season"))

ggplot(plotSeason, aes(x = Season, y = value, fill = variable)) +
  geom_bar(stat= "identity")+ 
  ylab("Consumption (kWh)") + xlab("Season") +
  ggtitle("Avg. Energy Consumption (kWh) per Season") +
  labs(fill = "Legend:") +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

####by weekday####
selDay <- consDays[c(1,4,5:8)]
plotDay <- melt(selDay, id.vars = c("Weekday","Year"))

ggplot(plotDay, aes(x = Weekday, y = value, fill = variable)) +
  geom_bar(stat= "identity") + #facet_wrap(~Year) + 
  ylab("Consumption (kWh)") + xlab("Weekday") +
  ggtitle("Avg. Energy Consumption per Weekday/Year") +
  labs(fill = "Legend:") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

####by year####
selYear <- consDays[c(1,5:8)]
plotYear <- melt(selYear, id.vars = "Year")

ggplot(plotYear, aes(x = Year, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  ylab("Consumption (kWh) ") + xlab("Year") +
  ggtitle("Avg. Energy Consumption (kWh) per Year") +
  labs(fill = "Legend:") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

####> grouped bar####
####by season/year####
ggplot(plotSeason, aes(x = factor(Season), y = value, fill = variable)) +
  geom_bar(stat= "identity", position="dodge") + 
  ylab("Consumption (kWh)") + xlab("Season") + facet_wrap(~Year) +
  ggtitle("Avg. Energy Consumption (kWh) per Season/Year") +
  labs(fill = "Legend:") +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

####by season####
ggplot(plotSeason, aes(x = Season, y = value, fill = variable)) +
  geom_bar(stat= "identity", position="dodge") +
  ylab("Consumption (kWh)") + xlab("Season") +
  ggtitle("Avg. Energy Consumption (kWh) per Season") +
  labs(fill = "Legend:") +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

####by weekday/year####
ggplot(plotDay, aes(x = Weekday, y = value, fill = variable)) +
  geom_bar(stat= "identity", position = "dodge") + #facet_wrap(~Year) + 
  ylab("Consumption (kWh)") + xlab("Weekday") +
  ggtitle("Avg. Energy Consumption per Weekday") +
  labs(fill = "Legend:") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

####by year####
ggplot(plotYear, aes(x = Year, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Consumption (kWh) ") + xlab("Year") +
  ggtitle("Avg. Energy Consumption (kWh) per Year") +
  labs(fill = "Legend:") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

####> pie chart####
selRooms <- consDays_sum[c(5:8)]

rooms <- selRooms %>% summarise(
  Kitchen = sum(Kitchen),
  Laundry = sum(Laundry),
  HVAC = sum(HVAC),
  Others = sum(Others)
)

ggplot(melt(rooms) %>% mutate(percentages = round(value / sum(value) * 100, 1)),
       aes(x = "", y = value, fill = factor(variable)
       )) +
  geom_bar(width = 1, stat = "identity") + coord_polar(theta = "y", start = 0) +
  theme_void() +
  theme(axis.line = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Legend:", x = NULL, y = NULL,
       title = "Distribution of Power Consumption") +
  geom_text(aes(label = paste(percentages, "%")),
            position = position_stack(vjust = 0.5),
            size = 3.5)