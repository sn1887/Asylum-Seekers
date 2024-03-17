#Import Data
d3 = read.csv("asylum_seekers_refugee_status.csv", skip = 3, header = T)


# Changing variables from character to numerical variables:

for (i in names(d3[-c(2,3,4)])){
  new_i <- c()
  for (j in d3[i]){
    new_i <- c(new_i, as.numeric(j))
  }
  d3[i] <- new_i
}
d3 = na.omit(d3)

# find the country list

country <- data.frame('country' = sort(unique(d3[[3]])), 'log'=NA, 'lat'= NA, "variable"=NA)
map<- read.csv('https://raw.githubusercontent.com/albertyw/avenews/master/old/data/average-latitude-longitude-countries.csv')
for (row in 1:nrow(country)){
  i <- country[row,1]
  if (i %in% map[[2]]){
    m <- subset(map, Country==i)
    country[row,c(2,3)] <- m[c(4,3)]
  }
}
keep <- as.data.frame(rbind(c('Central African Rep.',20.9394,6.6111,NA),
                            c('China, Macao SAR',113.5439,22.1987,NA),
                            c('Iran (Islamic Rep. of)',53.6880,32.4279,NA),
                            c('China, Hong Kong SAR',114.1694,22.3193,NA),
                            c('Rep. of Korea',127.7669,35.9078,NA),
                            c('Rep. of Moldova',28.3699,47.4116,NA),
                            c('South Sudan',31.3070,6.8770,NA),
                            c('Viet Nam',108.2772,14.0583,NA)))
names(keep) <- names(country)
country <- as.data.frame(rbind(country,keep))
country[4] <- 1
country <- na.omit(country)

country <- country[order(country$country),]

country <- data.frame(Country=country$country, log=country$log,lat=country$lat, variable=NA)

countrylist <- sort(country[[1]])







log_data = d3

log_data[, sapply(d3, is.numeric)] = log(d3[, sapply(d3, is.numeric)])
log_data[, sapply(d3, is.numeric)] = sapply(log_data[, sapply(d3, is.numeric)], 
                                            function (x) ifelse(is.finite(x) == F, NA, x) )

log_data = na.omit(log_data)

sample_data = sample_n(d3, 5000)
