summarizeState = function (dfd, State, Region="US") {
  Row=which (dfd$State==State)
  #
  # Determine date columns
  #
  Cols=grep("^X", colnames(dfd)) # dates have "X" as a column-label prefix)
  #
  # Extract the time series data and clean-up labels
  #
  StateDailyVals=dfd[Row,Cols]                       # a row (1xN matrix)
  StateVals=unlist(StateDailyVals)                         # convert to vector
  names(StateVals)=gsub("X","", names(StateVals))     # remove "X" in column names
  names(StateVals)=gsub("[.]", "/", names(StateVals)) # change "." to "/"

  #
  # 7-day moving average
  #
  Avg7=sapply(7:length(StateVals), function(i) { mean(StateVals[(i-6):i])})
  # 4-day centered moving average
  #Avg4=sapply(4:(length(StateVals)-3), function(i) { mean(StateVals[(i-3):(i+3)])})

  LastVal=StateVals[length(StateVals)]
  LastDate=names(StateVals)[length(StateVals)]
  iMaxDay=which(StateVals==max(StateVals))[1] # [1] in case duplicate max days, take 1st
  MaxDayVal=StateVals[iMaxDay]
  MaxDate=names(StateVals)[iMaxDay]
  if (tolower(Region)=="global") {
    StatePopulation=WorldPopulation$Population[which(WorldPopulation$Country==State)]
    StateRow=which(WorldArea$Country==State)
    if (identical(StateRow, integer(0))==T) StateArea=1 else StateArea = WorldArea$areasqm[StateRow]
  } else {
    StatePopulation=USPopulation$Population[which(USPopulation$State==State)]
    StateRow=which(USArea$State==State)
    if (identical(StateRow, integer(0))==T) StateArea=1 else StateArea = USArea$landsqm[StateRow]
  }
  PopulationDensity=StatePopulation/StateArea
  PopulationDensityFmt=formatC(PopulationDensity, format="f", big.mark=",", digits=2)
  Total=sum(StateVals)
  TotalFmt=formatC(Total, format="f", big.mark=",", digits=0)
  OverallPerMillion=Total/StatePopulation*1000000
  OverallPerMillionFmt=formatC(OverallPerMillion, format="f", big.mark=",", digits=2)
  LastValPerMillion=LastVal/StatePopulation*1000000

  ret=list(PopulationDensity=PopulationDensity,
           Total=Total,
           LastVal=LastVal,
           LastDate=LastDate,
           LastValPerMillion=LastValPerMillion,
           OverallPerMillion=OverallPerMillion,
           StateArea=StateArea,
           StatePopulation=StatePopulation,
           StateVals=StateVals,
           MaxDayVal=MaxDayVal,
           MaxDate=MaxDate,
           Avg7=Avg7
  )
  ret
}
