cleanGlobalData=function(dfOrig) {
  #
  # Get a list of unique countries
  #
  Countries=unique(dfOrig$Country.Region)

  #
  # For each State's rows, sum all columns
  #
  CountryTotals=sapply(Countries, function (x) {
    # Extract the county rows for a given state x
    rows=which(dfOrig$Country.Region==x)
    # Extract just the date cols for a county
    cols=grep("^X", colnames(dfOrig)) # All date columns begin with x
    # Results in a rectangular matrix, where rows are counties, and cols are dates
    rect=dfOrig[rows,cols]
    # Sum all columns to get the totals for a given state
    totals=colSums(rect)
  })


  # Transpose and add row & column names
  CountryTotals=t(CountryTotals)
  cols=grep("^X", colnames(dfOrig))
  colnames(CountryTotals)=names(dfOrig)[cols]
  rownames(CountryTotals)=Countries

  #
  # Create a dataframe (df) of the collapsed state values. This data frame contains daily running totals.
  #
  df=data.frame(Country=Countries,CountryTotals)
  df
}
