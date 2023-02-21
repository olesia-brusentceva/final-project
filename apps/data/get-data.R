
load(url("http://github.com/olesia-brusentceva/dash-boardd/blob/main/apps/data/WDI_Countries.Rda?raw=true"))
load(url("https://github.com/olesia-brusentceva/dash-boardd/blob/main/apps/data/WDI_Indicators.Rda?raw=true"))
WB_CountryPolygons <-
  geojson_read("https://github.com/olesia-brusentceva/dash-boardd/blob/main/apps/data/WB_countries_Admin0_lowres.geojson?raw=true",
               what = "sp")

WB_CountryPolygons<-WB_CountryPolygons[WB_CountryPolygons$ISO_A3_EH %in% WDI_Countries$Country.iso3c,]
WDI_Countries <- WDI_Countries[WDI_Countries$Country.iso3c %in% WB_CountryPolygons$ISO_A3_EH,]

WB_CountryPolygons$NAME_EN<- as.character(lapply(WB_CountryPolygons$ISO_A3_EH,function(x){WDI_Countries[WDI_Countries$Country.iso3c == x,1]}))


