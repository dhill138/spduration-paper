#  Replication script for JSS manuscript examples


library(spduration)
#data(bscoup)
bscoup <- read.dta("data/BelkinSchoferTable4.dta")
bscoup$coup <- ifelse(bscoup$coup=="yes", 1, 0)
bscoup <- add_duration(bscoup, "coup", unitID="countryid", tID="year",
                       freq="year")

