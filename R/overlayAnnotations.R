# Improvement: vector of colors
overlayAnnotations=function(dfAnnotation, Dates, MaxValue, LineColor="pink", TextColor="gray") {
  AnnotateDate  = dfAnnotation$AnnotateDate
  AnnotateLabel = dfAnnotation$AnnotateLabel
  YVal=MaxValue
  XVal=sapply(AnnotateDate, function(x) { which(Dates==x) })
  for (i in 1:length(XVal)) {
    if (length(XVal[i])!=0) { # If annotation date falls in plot range
      lines(c(XVal[i], XVal[i]), c(0,YVal), col=LineColor, lty=3)
      text(XVal[i], YVal, AnnotateLabel[i], pos=1, col=TextColor) # 1 is below
    }
  }
}
