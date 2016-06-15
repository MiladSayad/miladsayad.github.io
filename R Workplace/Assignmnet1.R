pathFinder <- function(x, directory = 'specdata') {
  if (x < 10) {
    paste(directory, '/00', x, '.csv', sep = "")
  } else if (x < 100) { 
    paste(directory, '/0', x, '.csv', sep = "")
  } else if (x < 333) {
    paste(directory, '/', x, '.csv', sep = "")
  } else {
    print("Error: The given x is out of range.")
  }
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
  data<- NULL
  for (i in id ) {
    path <- pathFinder(x = i, directory = directory)
    new <- read.csv(path,header = TRUE)
    data <- rbind (data, new)
  }
  target <- paste('data', '$', pollutant, sep = '')
  print(target)
  round(mean(eval(parse(text = target)), na.rm = TRUE),3)
}

getwd()
pollutantmean("specdata", "sulfate", 1:10)

pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

data<- NULL
for (i in 1:10 ) {
  path <- pathFinder(x = i, directory = "specdata")
  new <- read.csv(path,header = TRUE)
  data <- rbind (data, new)
}
data <- rbind (new, new)
path
mean(data$sulfate, na.rm = TRUE)
