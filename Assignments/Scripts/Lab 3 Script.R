today = Sys.Date()
library(lubridate)
format(today, format = "%B")
format(today, format = "%y")
format(today, format = "%M")
str_today = "2021-February-03"
class(str_today)
date = ymd(str_today)
date
lubridate::origin
