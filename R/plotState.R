plotState = function (dfd, State, DataType, dfa=NULL) {
  #
  # TWO CASES. Plot 1 state & Plot multiple states
  #

  # CASE: Plot 1 State
  if (length(State)==1) {
    StateInfo=summarizeState(dfd, State)
    XBarPlot=barplot(StateInfo$StateVals,ylim=c(0, StateInfo$MaxDayVal), col="#CCCCCC", bor="white", xlab="Date", ylab=sprintf("# %s",DataType))
    StateInfo[["XBarPlot"]]=XBarPlot # Add xvalues to list of return values
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
    # Do any annotations (TO DO: Fix Crash if Dates out of range)
    #
    if (!is.null(dfa)) {
      Dates=as.Date(names(dfd[2:length(names(dfd))]), format="X%m.%d.%y")
      AnnotateDate  = dfa$AnnotateDate
      AnnotateLabel = dfa$AnnotateLabel
      YVal=StateInfo$MaxDayVal
      XRow=sapply(AnnotateDate, function(x) { which(Dates==x) })
      XVal=StateInfo$XBarPlot[XRow]
      for (i in 1:length(XVal)) {
        lines(c(XVal[i], XVal[i]), c(0,YVal), col="pink", lty=3)
        text(XVal[i], YVal, AnnotateLabel[i], pos=1, col="gray") # 1 is below
      }
    }
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
  } else {
  # CASE: Plot Multiple States (State contains a vector of states)
    MultipleStates=State   # Redefine variable for readability
    StateRows=sapply(MultipleStates, function (x) {which(dfd$State==x)})
    dfm = dfd[StateRows, ] # Multiple State data frame (dfm)

    #
    # Put in a matrix
    #
    ValueMatrix=dfm[1:length(dfm$State), 2:length(dfm)] # Was matx
    DateLabels=gsub("[.]","/", gsub("X", "", names(dfm)[2:length(dfm)]))
    #
    # Get Populations & plot
    #
    Palette=brewer.pal(8, "Dark2")
    MaxValue=max(ValueMatrix)
    Day1=gsub("[.]","/",gsub("X","", colnames(ValueMatrix)[1]))
    DayN=gsub("[.]","/",gsub("X","", colnames(ValueMatrix)[length(colnames(ValueMatrix))]))
    for (i in 1:nrow(ValueMatrix)) {
      Color = Palette[((i-1)%%8)+1]
      # Make a light version of the color
      iColor = strtoi(gsub("#", "0x", Color))
      Lighten=64
      R=bitwShiftR(iColor,16)                  ; R=R+Lighten; R=ifelse(R>255, 255, R)
      G=bitwShiftR(bitwAnd(iColor,0x00ff00), 8); G=G+Lighten; G=ifelse(G>255, 255, G)
      B=bitwAnd(iColor, 0x0000ff)              ; B=B+Lighten; B=ifelse(B>255, 255, B)
      LiteColor=sprintf("#%x%x%x", R, G, B)
      if (i==1) {
        plot(as.numeric(ValueMatrix[i,]) , typ="p", col=LiteColor, xaxt="n", main=sprintf("COVID-19 Daily %s: %s", toupper(DataType), paste(MultipleStates, collapse="; ")), xlab=sprintf("Days (0 = %s; Current Day:%s = %s)", Day1, length(colnames(ValueMatrix)), DayN), ylab=DataType, ylim=c(0,MaxValue),lwd=3)
        axis(1, at=1:length(ValueMatrix), las=2, labels=DateLabels, cex.axis=0.5)
      } else
        lines(as.numeric(ValueMatrix[i,]), typ="p", col=LiteColor, lwd=3)
      Avg7=sapply(7:length(ValueMatrix[i,]), function(x) { mean(as.numeric(ValueMatrix[i, (x-6):x]))})
      lines(7:length(ValueMatrix[i,]), Avg7, lwd=3, col=Color)
    }
    legend(0, MaxValue-1, legend=MultipleStates, col=Palette[1:nrow(ValueMatrix)], lty=1, lwd=3)
    #
    # Do any annotations (TO DO: Fix Crash if Dates out of range)
    #
    if (!is.null(dfa)) {
      Dates=as.Date(names(dfd[2:length(names(dfd))]), format="X%m.%d.%y")
      AnnotateDate  = dfa$AnnotateDate
      AnnotateLabel = dfa$AnnotateLabel
      YVal=MaxValue
      XVal=sapply(AnnotateDate, function(x) { which(Dates==x) })
      for (i in 1:length(XVal)) {
        lines(c(XVal[i], XVal[i]), c(0,YVal), col="pink", lty=3)
        text(XVal[i], YVal, AnnotateLabel[i], pos=1, col="gray") # 1 is below
      }
    }

    # PER MILLION CODE — STILL TO BE REFACTORED
    #popu=sapply(rownames(ValueMatrix), function(state) {dfp$Population[which(dfp$State==state)]})
    #maty=ValueMatrix/popu*1000000
    #maxy=max(maty)
    #Day1=gsub("[.]","/",gsub("X","", colnames(maty)[1]))
    #DayN=gsub("[.]","/",gsub("X","", colnames(maty)[length(colnames(maty))]))
    #for (i in 1:nrow(maty)) {
    #  if (i==1)
    #    plot(maty[i,],typ="l", col=Color, main=sprintf("COVID-19 Daily %s: %s", toupper(DataType), paste(MultipleStates, collapse="; ")), xlab=sprintf("Days (0 = %s; Current Day:%s = %s)", Day1, length(colnames(maty)), DayN), ylab=sprintf("%s (per million)",DataType), ylim=c(0,maxy),lwd=3)
    #  else
    #    lines(maty[i,], col=Color, lwd=3)
    #
    #}
    #legend(0, maxy-1, legend=MultipleStates, col=Palette[1:nrow(maty)], lty=1, lwd=3)
  }
}
