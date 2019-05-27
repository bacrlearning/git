#28/5 test bacrlearning
#27/5 thu file thu 2 commit len github
##Test github xem the nao

### Sua them chut nua - ngay 18/5

#hom nay 21/5


##Chapter 1
library(dplyr)
library(hflights)
library(data.table)
library(RMySQL)

-hflights
head(hflights)
glimpse(hflights)

hflights <- tbl_df(hflights)
hflights

#Changing labels of hflights, part 1 of 2
lut <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental", 
         "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways", 
         "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier", 
         "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")

hflights$Carrier = lut[hflights$UniqueCarrier]

glimpse(hflights)

#Changing labels of hflights, part 2 of 2
lut <- c("A" = "carrier", "B" = "weather", "C" = "FFA", "D" = "security", "E" = "not cancelled")

hflights$Code <- lut[hflights$CancellationCode]

glimpse(hflights)

summarize(hflights,
          n_obs = n(hflights),
          n_carrier = n_distinct(hflights$UniqueCarrier),
          n_dest = n_distinct(hflights$Dest))

hflights %>%
  mutate(diff = TaxiIn - TaxiOut) %>%
  filter(!is.na(diff))%>%
  summarize(avg = mean(diff))


hflights %>%
  mutate(RealTime = ActualElapsedTime + 100,mph = 60*(Distance/RealTime))%>%
  filter(!is.na(mph),mph<70)%>%
  summarize(n_less = n(),
            n_dest = n_distinct(Dest),
            min_dist = min(Distance),
            max_dist = max(Distance))

hflights %>%
  mutate(
    RealTime = ActualElapsedTime + 100, 
    mph = 60 * Distance / RealTime) %>%
  filter(mph < 105|Cancelled==1|Diverted ==1)%>%
  summarize(n_non = n(),
            n_dest = n_distinct(Dest),
            min_dist = min(Distance),
            max_dist = max(Distance))

hflights %>%
  filter(!is.na(DepTime),!is.na(ArrTime),DepTime>ArrTime)%>%
  summarize(num = n())

hflights %>%
  group_by(UniqueCarrier)%>%
  summarize(p_canc = mean(Cancelled==1)*100,
            avg_delay = mean(ArrDelay,na.rm = TRUE)) %>%
  arrange(avg_delay,p_canc)

hflights %>%
  filter(!is.na(ArrDelay),ArrDelay>0)%>%
  group_by(UniqueCarrier)%>%
  summarize(avg = mean(ArrDelay))%>%
  mutate(rank = rank(avg))%>%
  arrange(rank)

hflights %>%
  filter(!is.na(ArrDelay))%>%
  summarize(avg = mean(ArrDelay))%>%
  mutate(rank = rank(avg))%>%
  arrange(rank)

hflights

hflights %>%
  group_by(UniqueCarrier)%>%
  summarize(nplanes = n_distinct(Dest))%>%
  filter(nplanes == 1)%>%
  summarize(nplanes = n())

hflights %>%
  group_by(UniqueCarrier,Dest)%>%
  summarize(n = n())%>%
  mutate(rank = rank(desc(n)))%>%
  filter(rank == 1)

hflights2 = as.data.table(hflights)
hflights2 %>%
  summarize(n_carrier = n_distinct(UniqueCarrier))

my_db <- src_mysql(dbname = "dplyr", 
                   host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                   port = 3306, 
                   user = "student",
                   password = "datacamp")

nycflights <- tbl(my_db, "dplyr")
glimpse(nycflights)

nycflights%>%
  group_by(carrier)%>%
  summarize(n_flights = n(),avg_delay = mean(arr_delay))%>%
  arrange(avg_delay)
  
