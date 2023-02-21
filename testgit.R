library('rvest')
library('tidyverse')
library("writexl")
library('readxl')

prop <- data.frame(adress1 = 
                            rep(NA,499*21+1), adress2=
                            rep(NA,499*21+1), adress3=
                            rep(NA,499*21+1), adress4=
                            rep(NA,499*21+1), adress5=
                            rep(NA,499*21+1), adress6=
                            rep(NA,499*21+1), adress7=
                            rep(NA,499*21+1), price=
                            rep(NA,499*21+1), rooms=
                            rep(NA,499*21+1), area=
                            rep(NA,499*21+1), discription=
                            rep(NA,499*21+1), link=
                            rep(NA,499*21+1)
                          )

for (page.num in 1:499){
  HTMLContent <- read_html(paste0("https://dom.ria.com/arenda-kvartir/kiev/?page=",page.num))
  for (prop.num in 1:21) {
    
    prop.block<-html_nodes(HTMLContent,paste0("#domSearchPanel > section:nth-child(",prop.num,")"))
    
    #link
    prop.block.links<- prop.block %>% html_nodes("a") %>% html_attr("href")
    prop.link <- if (!identical(prop.block.links,character(0))) {paste0("https://dom.ria.com",prop.block.links[[1]])} else NA
    if (is.na(prop.link)) break 
    
    #adress & price
    prop.block.expanded <- read_html(prop.link)
    prop.adress.list <-  prop.block.expanded %>% html_nodes("div.mt-10.mb-15") %>% html_nodes("a") %>% html_attr("title")
    prop.price <-  prop.block.expanded %>% html_nodes("div.mt-20.mb-20.price") %>% html_nodes("b")  %>% html_text()
    
    #discription
    prop.discription <- prop.block.expanded %>% html_nodes("div.mb-30")  %>% html_nodes("div.boxed") %>% html_text() %>%  paste(collapse = '')
    
    
    #rooms & area
    prop.rooms.area <- prop.block %>% html_nodes("div.wrap_desc.p-rel") %>% html_nodes("div.mt-10.chars") %>% html_nodes("span") %>% html_text()
    
    #add to df
    prop[((page.num-1)*21+prop.num),]<-c(prop.adress.list[2:8], prop.price, prop.rooms.area[1:2], prop.discription ,prop.link)
    
  }
  print(page.num)
}

prop.data.lesia <- prop[1:(212*21),]
save(prop.data.lesia, file = "prop.data.lesia.RData")

#ready final data
data = rbind(prop.data.lesia,prop.data)

#cleaning

data<-data[!is.na(data$link),]

data$microdistrict <- NA
data$district <- NA
data$city<-NA
data$metro<-NA


which(grepl("микрорайон",data$adress1,fixed = T))
which(grepl("микрорайон",data$adress2,fixed = T))
which(grepl("район",data$adress1,fixed = T))
which(grepl("район",data$adress2,fixed = T))
which(grepl("район",data$adress3,fixed = T))
which(grepl("Киев",data$adress2,fixed = T))
which(grepl("Киев",data$adress3,fixed = T))
which(grepl("Киев",data$adress4,fixed = T))
which(grepl("метро",data$adress3,fixed = T))
which(grepl("метро",data$adress4,fixed = T))
which(grepl("метро",data$adress5,fixed = T))





data[grepl("микрорайон",data$adress1,fixed = T),]$microdistrict<-data[grepl("микрорайон",data$adress1,fixed = T),]$adress1
data[grepl("микрорайон",data$adress2,fixed = T),]$microdistrict<-data[grepl("микрорайон",data$adress2,fixed = T),]$adress2
data[grepl("микрорайон",data$adress1,fixed = T),]$adress1<-NA
data[grepl("микрорайон",data$adress2,fixed = T),]$adress2<-NA

data[grepl("район",data$adress1,fixed = T),]$district<-data[grepl("район",data$adress1,fixed = T),]$adress1
data[grepl("район",data$adress2,fixed = T),]$district<-data[grepl("район",data$adress2,fixed = T),]$adress2
data[grepl("район",data$adress3,fixed = T),]$district<-data[grepl("район",data$adress3,fixed = T),]$adress3
data[grepl("район",data$adress1,fixed = T),]$adress1<-NA
data[grepl("район",data$adress2,fixed = T),]$adress2<-NA
data[grepl("район",data$adress3,fixed = T),]$adress3<-NA

data[grepl("Киев",data$adress2,fixed = T),]$city<-data[grepl("Киев",data$adress2,fixed = T),]$adress2
data[grepl("Киев",data$adress3,fixed = T),]$city<-data[grepl("Киев",data$adress3,fixed = T),]$adress3
data[grepl("Киев",data$adress4,fixed = T),]$city<-data[grepl("Киев",data$adress4,fixed = T),]$adress4
data[grepl("Киев",data$adress2,fixed = T),]$adress2<-NA
data[grepl("Киев",data$adress3,fixed = T),]$adress3<-NA
data[grepl("Киев",data$adress4,fixed = T),]$adress4<-NA

data[grepl("метро",data$adress3,fixed = T),]$metro<-data[grepl("метро",data$adress3,fixed = T),]$adress3
data[grepl("метро",data$adress4,fixed = T),]$metro<-data[grepl("метро",data$adress4,fixed = T),]$adress4
data[grepl("метро",data$adress5,fixed = T),]$metro<-data[grepl("метро",data$adress5,fixed = T),]$adress5
data[grepl("метро",data$adress3,fixed = T),]$adress3<-NA
data[grepl("метро",data$adress4,fixed = T),]$adress4<-NA
data[grepl("метро",data$adress5,fixed = T),]$adress5<-NA

fin.data <- data.frame(street = data$adress1, microdistrict = data$microdistrict, disctrict = data$district, city=data$city, metro = data$metro, price = data$price, discription = data$discription,rooms=data$rooms, area=data$area,link=data$link)

save(fin.data, file="fin.data.RData")

write_xlsx(fin.data, "C:\\Users\\bruse\\OneDrive\\Документы\\final-project\\FinData.xlsx")

FD <- read_xlsx('FinData.xlsx')

