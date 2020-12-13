getRange=function(dfd, StartDate="", EndDate="") { # Use format year-month-day
  # Format all dates, then grab first & last dates
  DatesFmtd = as.Date(names(dfd[2:length(names(dfd))]), format="X%m.%d.%y")
  FirstDate = DatesFmtd[1]
  LastDate  = DatesFmtd[length(DatesFmtd)]

  # Grab starting and ending date
  if (StartDate!="") StartDate = as.Date(StartDate) else StartDate = FirstDate
  if (EndDate  !="") EndDate   = as.Date(EndDate)   else EndDate   = LastDate

  # Determine columns for the starting and ending states + error handling
  # All columns in DatesFmtd are off by 1, because column 1 is reserved for State
  StartCol = which(DatesFmtd==StartDate) + 1
  if (length(StartCol)==0) StartCol=2                  # check if error, default to start column
  EndCol   = which(DatesFmtd==EndDate  ) + 1
  if (length(EndCol  )==0) EndCol  = length(DatesFmtd) # check if error, default to end column

  # Return a data frame with just the starting and ending dates
  dft=data.frame(State=dfd$State,dfd[,StartCol:EndCol])
}
