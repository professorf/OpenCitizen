summarizeCountry = function (dfd, Country) {
  Row=which (dfd$Country==Country)
  #
  # Determine date columns
  #
  Cols=grep("^X", colnames(dfd)) # dates have "X" as a column-label prefix)
  #
  # Extract the time series data and clean-up labels
  #
  CountryDailyVals=dfd[Row,Cols]                       # a row (1xN matrix)
  CountryVals=unlist(CountryDailyVals)                         # convert to vector
  names(CountryVals)=gsub("X","", names(CountryVals))     # remove "X" in column names
  names(CountryVals)=gsub("[.]", "/", names(CountryVals)) # change "." to "/"

  #
  # 7-day moving average
  #
  Avg7=sapply(7:length(CountryVals), function(i) { mean(CountryVals[(i-6):i])})
  # 4-day centered moving average
  #Avg4=sapply(4:(length(CountryVals)-3), function(i) { mean(CountryVals[(i-3):(i+3)])})

  LastVal=CountryVals[length(CountryVals)]
  LastDate=names(CountryVals)[length(CountryVals)]
  iMaxDay=which(CountryVals==max(CountryVals))[1] # [1] in case duplicate max days, take 1st
  MaxDayVal=CountryVals[iMaxDay]
  MaxDate=names(CountryVals)[iMaxDay]
  CountryPopulation=WorldPopulation$Population[which(WorldPopulation$Country==Country)]
  CountryRow=which(WorldArea$Country==Country)
  if (identical(CountryRow, integer(0))==T) {
    CountryArea=1
  } else {
    CountryArea = WorldArea$areasqm[CountryRow] # WorldArea only has areasqm
  }
  PopulationDensity=CountryPopulation/CountryArea
  PopulationDensityFmt=formatC(PopulationDensity, format="f", big.mark=",", digits=2)
  Total=sum(CountryVals)
  TotalFmt=formatC(Total, format="f", big.mark=",", digits=0)
  OverallPerMillion=Total/CountryPopulation*1000000
  OverallPerMillionFmt=formatC(OverallPerMillion, format="f", big.mark=",", digits=2)
  LastValPerMillion=LastVal/CountryPopulation*1000000

  ret=list(PopulationDensity=PopulationDensity,
           Total=Total,
           LastVal=LastVal,
           LastDate=LastDate,
           LastValPerMillion=LastValPerMillion,
           OverallPerMillion=OverallPerMillion,
           CountryArea=CountryArea,
           CountryPopulation=CountryPopulation,
           CountryVals=CountryVals,
           MaxDayVal=MaxDayVal,
           MaxDate=MaxDate,
           Avg7=Avg7
  )
  ret
}
