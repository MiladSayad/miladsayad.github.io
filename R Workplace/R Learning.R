#Press cmd+enter to run the line

# Comment

rm(Var1,Var2,Var3, ...)
rm(list=ls())

ls() # List of Variables

#Coding Style
BrwoseUrl("https://google.github.io/styleguide/Rguide.xml")  
#Task View (Packages Category)
BrwoseUrl("https://cran.r-project.org/web/views/")  
#Packages
BrwoseUrl("https://cran.r-project.org/web/packages/available_packages_by_name.html")
#Packages(other source)
BrwoseUrl("http://crantastic.org/packages")

#Rstudio Keywords Shortcut
browseURL("https://support.rstudio.com/hc/en-us/articles/200711853")


# Packages
install.packages("ggplot2") # Downloading a package
library("ggplot2") # Activating a package
require("ggplot2") #same with Library
library (help="ggplot2")
search() #check active packages

remove.packages("ggplot2") #unload package
detach("package:ggplot2", unload = TRUE) #remove package

vignette(package="ggplot2")
browseVignettes(package = "ggplot2")
vignette()
# Improting Data
?read.csv

Name.csv <- read.csv("cds", header = TRUE)
str(Name.csv)
View (Name.csv)
#Pick Color Code
browseURL("http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.pdf")


# VISULAISATION
summary()

#Load data
?UCBAdmissions
str(UCBAdmissions)

admit.dept <- margin.table(UCBAdmissions, 3)
str(admit.dept)
?table
admit.dept
prop.table(admit.dept) 

?margin.table(UCBAdmissions)

?...
?paste
