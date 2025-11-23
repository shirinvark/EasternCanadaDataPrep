


library(SpaDES)

pathsList <- list(
  modulePath = "E:/EasternCanadaProject/modules",
  inputPath  = "E:/EasternCanadaProject/inputs",
  outputPath = "E:/EasternCanadaProject/output",
  scratchPath = "E:/EasternCanadaProject/scratch"
)

sim <- simInit(
  times = list(start = 0, end = 1),
  modules = "EasternCanadaDataPrep",
  paths = pathsList
)

sim@paths

