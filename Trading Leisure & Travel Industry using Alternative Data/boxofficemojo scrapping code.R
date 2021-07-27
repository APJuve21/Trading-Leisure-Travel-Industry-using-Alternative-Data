# source "https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/"

#Domestic Box Office

library(rvest)
library(magrittr)
library(stringr)
library(openxlsx)
library(aweek)

boxoffice = data.frame(year = 0, week_number = 0, date = as.Date('1990-01-01'), weekly_rev = 0)
years_looped = 0

for (nyear in 2019:2021) {
  #1. Load the website boxofficemojo
  url = paste("https://www.boxofficemojo.com/weekly/by-year/", nyear, "/", sep = "")
  
  webpage <- read_html(url)

  #2. Count the number of observations/week available
  for (i in 2:54) {
    html_node(webpage, paste("tr:nth-child(", i ,") .mojo-estimatable", sep = "")) %>% html_text() -> temp 
    if (is.na(temp)) {
      break 
    }
  }
  
  #3. Get weekly revenue
  num = i - 2
  for (n in 1:num) {
    html_node(webpage, paste("tr:nth-child(", num + 2 - n ,") .mojo-estimatable", sep = "")) %>% html_text() -> temp 
    assign(paste("week", n, "_rev", sep = ""), as.numeric(str_remove_all(temp, "[$,]")))
  }
  
  #4. Load data into data.frame
  for (n in 1:num) {
    boxoffice[n+years_looped*52, 1] = nyear
    boxoffice[n+years_looped*52, 2] = n
    boxoffice[n+years_looped*52, 3] = get_date(n, nyear, start = 5)
    boxoffice[n+years_looped*52, 4] = eval(parse(text = paste("week", n, "_rev", sep = "")))
  }
  years_looped = years_looped +1
}

# Plot in R 
plot(x = boxoffice$date, y = boxoffice$weekly_rev, xlab = 'Date', ylab = '$ Boxoffice', type = 'l', col = 'blue', lwd = 3, main = 'US Weekly Box Office')

# Export into excel
#write.xlsx(boxoffice, "USboxoffice.xlsx")