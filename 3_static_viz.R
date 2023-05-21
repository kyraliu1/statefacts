wd <- "~/Documents/hackdavis2023" # kyra pop wd
setwd(wd)
library(terra)
library(wesanderson)
library(maptiles)
fname <- "./statesdata.csv"
if (!file.exists(fname)){
  download.file("https://www.thatnickpowersguy.com/_files/ugd/079347_533f9a5c6af14fe48327dc4fe5329cbe.csv", fname)
}
sta <- read.csv(fname)

# subsetting for desired columns
desired <- c(1, 4, 5, 6, 7, 8, 10:12, 15, 22:27, 30:33, 35, 36, 38, 39, 46:53,
             58, 61, 98,99,101,105:108, 119, 120, 122:125, 128,
             167, 182:188, 195, 199, 200, 206)

st<- sta[,desired ]


# getting state boundaries
f2 <- "./stateboundaries"
if (!file.exists(f2)){
  
  download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_20m.zip", f2)
  dir.create("./states")
  unzip(f2,exdir="./states")
}

usa <- vect(list.files(path = "./states",full.names = T,pattern = "shp$"))
# projecting
aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m"

us <- project(usa, aea)

#AK
ak_aea <- "+proj=aea +lat_1=50 +lat_2=70 +lat_0=60 +lon_0=-160 +x_0=0 +y_0=0 +datum=WGS84 +units=m"
ak <- project(usa[usa$NAME == "Alaska", ], ak_aea)
#HI
hi_aea <- "+proj=aea +lat_1=19 +lat_2=22 +lat_0=20.5 +lon_0=-158 +x_0=0 +y_0=0 +datum=WGS84" 
hi <- project(usa[usa$NAME == "Hawaii", ], hi_aea)
out <- us$NAME %in% c("Alaska", "Hawaii", "Puerto Rico")
us <- us[!out, ] #no pr, will put ak & hi back

#shifting & rescaling
ak2 <- rescale(ak, .25)
ak2 <- shift(ak2, -1900000, -1300000)
hi2 <- shift(hi, -1150000,  -1100000)
usm <- rbind(us, hi2, ak2)
boxes <- rbind(
  as.polygons(ext(ak2) + 10000),
  as.polygons(ext(hi2) + 10000)
)

names(usm)[6] <- "State"
# merge data and boundaries
st <- merge(usm, st, "State")
st$Gender.Pay.Gap
## gender pay gap
pal = colorRampPalette(c("plum1","snow","royalblue2"))(5)
st$Gender.Pay.Gap<-st$Gender.Pay.Gap*100
plot(st,"Gender.Pay.Gap", axes = F,
     main = "percent difference in average income between men and women", 
     col = pal, breaks = 100*c(.1,.15,.2,.25,.3,.35))



## minimum wage hours needed to pay median monthly rent
p2 <- colorRampPalette(c("khaki1","white","salmon"))(100)
plot(st,"MinWageHr.Rent",main = "minimum wage hours needed to pay median monthly rent",
      col =p2, breaks = c(69,90,110,130,150,170), axes = F)

## number of big macs you can buy with median wage
burger <- colorRampPalette(c("olivedrab","khaki","tomato"))(100)
plot(st,"X.of.Big.Macs.MedWage.Wk", main = "median wage week measured in big macs",
     col = burger,axes=F, breaks = c(230,260,290,320,350,380))

cp <- wes_palette("GrandBudapest2",7,"continuous")
 barplot(st$Prison.Per.100K, names.arg = st$State,las = 1, horiz = T,cex.names = 0.3, col =cp)
plot(st$Median.Wage.Month,st$Prison.Per.100K)

re <- sta[,c("MinWageHr.Rent","Prison.Per.100K","Police.per.100k.people","Gender.Pay.Gap",
         "Median.Wage.Month","Poverty.Rate","Groceries.Montly","Population",
         "Population.Density","Average.Electric.Bill","Suicide.Rate.2020","Police.Shootings.Per.100k",
         "Total.Uninsured..","Firearm.Ownership", "Effective.Tax.Rate", "Average.Gas.Price")
         ]

cm <- cor(re)

cm <- round(cm, digits = 2)

# Create a heatmap of the correlation matrix
par(mar = c(6,6,6,6))
heatmap(cm, col = colorRampPalette(c("blue", "white", "red"))(100),
        cexCol = 0.7, cexRow = 0.7, keep.dendro = F)


# making graphic for front
plot(bmap)
plot(us, col = cp,add = T,alpha = 0.5)
bmap <- get_tiles(st, crop = TRUE,provider = "OpenStreetMap",verbose=T,zoom=6)

