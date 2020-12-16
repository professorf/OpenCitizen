#
# Create a data frame of daily differences (dfd) based on df's daily running totals
#
createGlobalDiffs=function(df) {
  NumCols = length(colnames(df))   # This is 1 + #dates
  NumRows = length(df$Country)     # This is the number of countries
  Deltas  = sapply(1:NumRows, function (row) {                               # for each state
    Delta   = sapply(3:NumCols, function (col) {df[row,col]-df[row,(col-1)]}) # Create vector of daily differences
  }) # Note: start at column 3 because column 2 is the first date, no delta for 1st date
  Deltas=t(Deltas) # Always have to transpose due to sapply returning a column vector
  colnames(Deltas)=colnames(df)[3:NumCols]
  Countries  = unique(df$Country)
  dfd=data.frame(Country=Countries,Deltas)   # dfd: data frame of daily differences
  dfd
}
