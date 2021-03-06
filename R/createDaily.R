#
# Create a data frame of daily differences (dfd) based on df's daily running totals
#
createDaily=function(df, StartCol=3) {
  NumCols = length(colnames(df)) # This is 1 + #dates
  NumRows = length(df$State)     # This is the number of states
  Deltas  = sapply(1:NumRows, function (row) {                               # for each state
    Delta   = sapply(StartCol:NumCols, function (col) {df[row,col]-df[row,(col-1)]}) # Create vector of daily differences
  }) # Note: start at column 3 because column 2 is the first date, no delta for 1st date
  Deltas=t(Deltas) # Always have to transpose due to sapply returning a column vector
  colnames(Deltas)=colnames(df)[StartCol:NumCols]
  States  = unique(df$State)
  dfd=data.frame(State=States,Deltas)   # dfd: data frame of daily differences
  dfd                                   # Return a data frame of daily (dfd) values
}
