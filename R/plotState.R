plotState = function (dfd, State, DataType) {
  StateInfo=summarizeState(dfd, State)
  XBarPlot=barplot(StateInfo$StateVals,ylim=c(0, StateInfo$MaxDayVal), col="#CCCCCC", bor="white", xlab="Date", ylab=sprintf("# %s",DataType))
  Colors=RColorBrewer::brewer.pal(8, "Dark2")
  #
  # 7-day moving average
  #
  Avg7=sapply(7:length(StateInfo$StateVals), function(i) { mean(StateInfo$StateVals[(i-6):i])})
  lines(XBarPlot[7:length(StateInfo$StateVals)], Avg7, col=Colors[1])
  # 4-day centered moving average
  #Avg4=sapply(4:(length(StateVals)-3), function(i) { mean(StateVals[(i-3):(i+3)])})
  #lines(XBarPlot[4:(length(StateVals)-3)], Avg4, col=Colors[1])

  #
  # Title graph
  #
  LastVal=StateInfo$LastVal
  LastDate=StateInfo$LastDate
  MaxDayVal=StateInfo$MaxDayVal
  MaxDate=StateInfo$MaxDate
  LastValFmt=formatC(LastVal, format="f", digits=0, big.mark=",")
  MaxDayValFmt=formatC(MaxDayVal, format="f", digits=0, big.mark=",")
  StatePopulation=StateInfo$StatePopulation
  StatePopulationFmt=formatC(StatePopulation, format="f", big.mark = ",", digits=0)
  StateRow=which(USArea$State==State)
  StateArea = StateInfo$StateArea
  StateAreaFmt=formatC(StateArea, format="f", big.mark=",", digits=0)
  PopulationDensity=StateInfo$PopulationDensity
  PopulationDensityFmt=formatC(PopulationDensity, format="f", big.mark=",", digits=2)
  Total=StateInfo$Total
  TotalFmt=formatC(Total, format="f", big.mark=",", digits=0)
  OverallPerMillion=StateInfo$OverallPerMillion
  OverallPerMillionFmt=formatC(OverallPerMillion, format="f", big.mark=",", digits=2)
  LastValPerMillion=StateInfo$LastValPerMillion
  title(sprintf("%s - COVID-19 DAILY %s (Total): %s on %s (%s)\nPop: %s; Area: %s sq-miles; Peak: %s on %s\nPopulation Density: %s; Total per Million: %s", State, toupper(DataType), LastValFmt, LastDate, TotalFmt, StatePopulationFmt, StateAreaFmt, MaxDayValFmt, MaxDate, PopulationDensityFmt, OverallPerMillionFmt))

  StateInfo
}
