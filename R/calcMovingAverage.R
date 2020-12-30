calcMovingAverage=function(Values, N=7) {
  AverageN=sapply(N:length(Values), function(x) { mean(Values[(x-(N-1)):x])})
}
