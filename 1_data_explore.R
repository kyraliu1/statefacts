## 1. getting and processing

# setting working directory
wd <- "~/Documents/hackdavis2023" # kyra pop wd
setwd(wd)

# loading packages
if (!require(remotes)) install.packages("remotes")
if (!require(terra))remotes::install_github("rspatial/terra")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages('ggplot2')
if(!require(tidyr)) install.packages("tidyr")
if(!require(dplyr)) install.packages("dplyr")
# getting data 
fname <- "./statesdata.csv"
if (!file.exists(fname)){
download.file("https://www.thatnickpowersguy.com/_files/ugd/079347_533f9a5c6af14fe48327dc4fe5329cbe.csv", fname)
}
states <- read.csv(fname)

# subsetting for desired columns
desired <- c(1, 4, 5, 6, 7, 8, 10:12, 15, 22:27, 30:33, 35, 36, 38, 39, 46:53,
              58, 61, 98,99,101,105:108, 119, 120, 122:125, 128,
             167, 187:188, 195, 199, 200, 201,202,203,206)

states <- states[,desired ]

# categorizing variables
cats <- data.frame(names(states),desired,NA,NA)
names(cats) <- c("name","num","category","label")
cats$category[cats$num == 1] <- "name"
cats$category[cats$num %in%c(4,5,6)] <- "general"
cats$category[cats$num %in% c(7,8,10,11,12,15,31,32,38,39)] <- "income"
cats$category[cats$num %in% c(22,23,24,25,26,27,199,200)] <- "housing"
cats$category[cats$num %in% c(30,33,35,36)] <- "living.costs"
cats$category[cats$num %in% c(47,48,49,50,51,52,53,58,61)] <- "law.enforcement"
cats$category[cats$num %in% c(98,99,101,105,106,107,108)] <- "health"
cats$category[cats$num %in% c(46,119,120,122,123,124,125,128,167,187,188,195)] <- "politics"
cats$category[cats$num %in% c(201,202,203,206)] <- "climate"

names(states)[1] <- "State"
#rounding to 2 decimal places
nu <- sapply(states, is.numeric)  
states[nu] <- lapply(states[nu], function(x) round(x, 2))  

stchar <- states %>%
  mutate_all(as.character)
stchar$Minimum.Wage <- paste0("$",stchar$Minimum.Wage,"/hr")
stchar$Car.Registration <- paste0("$", stchar$Car.Registration)
stchar$Median.Wage.Month <- paste0("$",stchar$Median.Wage.Month)
stchar$Median.Income.Yr <- paste0("$",stchar$Median.Income.Yr)
stchar$Median.Rent <- paste0("$",stchar$Median.Rent, "/month")
stchar$MedianWage.HR <- paste0("$",stchar$MedianWage.HR)
stchar$Median.House.Cost <- paste0("$",stchar$Median.House.Cost)
stchar$Average.Electric.Bill <- paste0("$",stchar$Average.Electric.Bill,"/month")
stchar$Groceries.Montly <- paste0("$",stchar$Groceries.Montly)
stchar$Average.Gas.Price <- paste0('$',stchar$Average.Gas.Price)
stchar$Average.Temperature <- paste0(stchar$Average.Temperature, "\u00B0", "F")
#states[51,] <- c(t(cats$category))
# adding labels 
cats$label <- c("state","population","population change between 2010 and 2020",
                 "population density", "number of billionaires", "minimum wage",
"median monthly wage","median annual income", "median hourly wage","gender pay gap",
"median rent", "median house cost", "minimum wage hours needed for house cost", 
"minimum wage hours needed for median rent","median wage hours needed for median rent",
"median income years needed for house cost",
"average electric bill", "minimum wage in number of big macs", "median wage in number of big macs",
"monthly groceries","average gas price", "car registration", "top earner's income tax rate", 
"effective tax rate", "firearm ownership", "violent crime per 100k in 2020", "prisons per 100k",
"total prison population", "number of police officers","police per 100k", "police shootings",
"police shootings per 100k", "2017 hate crimes per 100k", "death penalty", 
"uninsured males", "uninsured females", "uninsured", "suicide rate in 2020", 
"suicide deaths in 2020","infant mortality rate in 2019", "maternal mortality per 100k",
"abortion legality","abortion after roe and casey", "cultural diversity ranking (1-50)", 
"trans safety ranking (1-50)","LGBTQ safety ranking (1-50)","gender affirming medicaid",
"firearm registration", "recreational marijuana", "food insecure adults", "food insecure children",
"poverty rate", "home vacancy rate", "corporate ownership vs. vacancy rate", "air quality",
"water quality", 'average humidity',"average temperature")


# getting state boundaries
f2 <- "./stateboundaries"
if (!file.exists(f2)){

download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_20m.zip", f2)
dir.create("./states")
unzip(f2,exdir="./states")
}

usa <- vect(list.files(path = "./states",full.names = T,pattern = "shp$"))
# project
usa <- project(usa, "+proj=longlat +datum=WGS84")

# take out PR
usa <- usa[usa$NAME!= "Puerto Rico",]

names(usa)[6] <- "State"
# merge data and boundaries
states <- merge(usa, stchar, "State")
# converting to sf for shiny
states <- sf::st_as_sf(states)




