DatePeriod <- function(Period, Origin = Sys.Date()) {
	require(lubridate)
	PeriodStartDate <- case_when(
		Period == "LastWeek" ~ Origin - days(7),
		Period == "ShortTerm" ~ Origin - days(14),
		Period == "MidTerm" ~ as.Date(ifelse(is.na(Origin - months(1)), Origin - days(30), Origin - months(1))),
		Period == "UpperMidTerm" ~ as.Date(ifelse(is.na(Origin - months(2)), Origin - days(60), Origin - months(2))),
		Period == "LongTerm" ~ as.Date(ifelse(is.na(Origin - months(6)), Origin - days(180), Origin - months(6))))
	return(as.Date(PeriodStartDate))
}

