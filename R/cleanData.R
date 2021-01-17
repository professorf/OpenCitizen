cleanData=function(dfOrig, Region="US", State="") { # dfType: US or Global
  if (tolower(Region)=="global") ColName="Country.Region" else ColName="Province_State"

  if (Region=="US" && State!="") { # Special Case for counties, extract all the Counties for a State
      rows=which(dfOrig[ColName]==State)
      cols=which(grepl("^X", names(dfOrig)))
      matx=dfOrig[rows, cols[1]:cols[length(cols)]]
      df=data.frame(State=dfOrig$Admin2[rows], matx)
  } else {                         # Typical case for states and countries
    #
    # Get a list of unique states
    #
    States=unique(dfOrig[,ColName])
    #
    # For each State's rows, sum all columns
    #
    StateTotals=sapply(States, function (x) {
      # Extract the county rows for a given state x
      rows=which(dfOrig[ColName]==x) # & (grepl("^Out of", dfOrig$County)==F) & (grepl("^Unassigned", dfOrig$County)==F))
      # Extract just the date cols for a county
      cols=grep("^X", colnames(dfOrig)) # All date columns begin with x
      # Results in a rectangular matrix, where rows are counties, and cols are dates
      rect=dfOrig[rows,cols]
      # Sum all columns to get the totals for a given state
      totals=colSums(rect)
    })
    # Transpose and add row & column names
    StateTotals=t(StateTotals)
    cols=grep("^X", colnames(dfOrig))
    colnames(StateTotals)=names(dfOrig)[cols]
    rownames(StateTotals)=States

    #
    # Create a dataframe (df) of the collapsed state values. This data frame contains daily running totals.
    # Instead of a country column, both countries and US states are now called state
    df=data.frame(State=States,StateTotals)
    df
  }
}
