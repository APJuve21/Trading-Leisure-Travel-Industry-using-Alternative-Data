#https://towardsdatascience.com/scraping-live-traffic-data-in-3-lines-of-code-step-by-step-9b2cc7ddf31f
library(jsonlite)
library(randomcoloR)

cities = c('new-york', 'los-angeles', 'san-francisco', 'austin', 'miami', 'phoenix', 'atlanta', 'boston', 'chicago')
colors = randomColor(length(cities))

############################## Weekly Data ##############################

weekly_data <- data.frame(week_start = '')

for (i in 1:length(cities)) { 
  temp <- fromJSON(paste("https://api.midway.tomtom.com/ranking/weeklyStats/USA_", cities[i], sep = ""))  
  if (i == 1) {
    counter = nrow(temp)
    for (n in 1:counter) {
      weekly_data[n,1] = temp[n,2]
    }
  }
  for (m in 1:counter) {
    weekly_data[m,1+i] = temp[m,4]  
  }
}

names(weekly_data)[2:(length(cities)+1)] = cities

for (p in 1:ncol(weekly_data)){
  if (p == 1) {
    weekly_data[,p] <- as.Date(weekly_data[,p])  
  } else {
    weekly_data[,p] = weekly_data[,p] * 100
  }
}


for (i in 2:ncol(weekly_data)) {
  if (i == 2){
    plot(x = weekly_data[,1], y = weekly_data[,2], xlab = 'Date', ylab = '% Change', type = 'b', col = colors[i-1], lwd = 2, main = 'US Major Cities % Changes in Congestion Lvl 2021 v.s. 2019', ylim = c(-70,10))    
  } else {
    lines(x = weekly_data[,1], y = weekly_data[,i], type ='b', col = colors[i-1], lwd = 2)
  }
}
legend("bottomright", 
       legend = cities, 
       col = colors, 
       pch = 20, 
       bty = "o", 
       pt.cex = 1, 
       cex = 0.5, 
       text.col = colors, 
       inset = c(0.05, 0.05))

############################## Daily Data ##############################

daily_data <- data.frame(which_date = '')

for (i in 1:length(cities)) { 
  temp <- fromJSON(paste("https://api.midway.tomtom.com/ranking/dailyStats/USA_", cities[i], sep = ""))  
  if (i == 1) {
    counter = nrow(temp)
    for (n in 1:counter) {
      daily_data[n,1] = temp[n,1]
    }
  }
  for (m in 1:counter) {
    daily_data[m,1+i] = temp[m,5]  
  }
}

adj_daily_data <- data.frame(which_date = '')
for (i in 1:ncol(daily_data)){
  if (i == 1) {
    counter = nrow(daily_data)
    for (n in 1:(counter-6)) {
      adj_daily_data[n,1] = daily_data[n+6,1]  
    }
  } else {
    for (n in 1:(counter-6)) {
      adj_daily_data[n,i] = rollmeanr(daily_data[,i], 7)[n]
    }
  }
}

names(adj_daily_data)[2:(length(cities)+1)] = cities

for (p in 1:ncol(adj_daily_data)){
  if (p == 1) {
    adj_daily_data[,p] <- as.Date(adj_daily_data[,p])  
  } else {
    adj_daily_data[,p] = adj_daily_data[,p] * 100
  }
}

for (i in 2:ncol(adj_daily_data)) {
  if (i == 2){
    plot(x = adj_daily_data[,1], y = adj_daily_data[,2], xlab = 'Date', ylab = '% Change', type = 'l', col = colors[i-1], lwd = 2, main = 'US Major Cities % Changes in Congestion Lvl 2021 v.s. 2019', ylim = c(-70,10))    
  } else {
    lines(x = adj_daily_data[,1], y = adj_daily_data[,i], type ='l', col = colors[i-1], lwd = 2)
  }
}

legend("bottomright", 
       legend = cities, 
       col = colors, 
       pch = 20, 
       bty = "o", 
       pt.cex = 1, 
       cex = 0.5, 
       text.col = colors, 
       inset = c(0.05, 0.05))