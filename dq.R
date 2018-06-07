library(ggmap)
library(dplyr)
library(rvest)
library(geosphere)

# get location list from DairyQueen.com
URL <- "https://www.dairyqueen.com/us-en/Sitemap/"
result <- read_html(URL)
nodes <- result %>% 
  html_nodes(xpath = "//div[@class = 'center-960']//ul//li//a") %>%
  html_text() %>%
  data.frame() %>%
  filter(grepl(", MN", .)) %>% # subset to only MN
  mutate(address = as.character(gsub("\\,", "", gsub("\\s\\s", " ", .))),
         address_clean = sapply(address, function(x) strsplit(x, "MN")[[1]][1], USE.NAMES=F))

# get lat/lon
api_key <- " "
latlngdq <- NULL

for(z in seq_along(nodes$address)){
  location <- gsub("\\s", "\\+", nodes$address[z])
  URL2 <- paste("https://maps.googleapis.com/maps/api/geocode/xml?address=", 
                location, "&key=", api_key, sep="")
  
  result <- read_html(URL2)
  lat <- result %>% html_nodes(xpath = "(//geometry//location//lat)[1]") %>% html_text()
  lon <- result %>% html_nodes(xpath = "(//geometry//location//lng)[1]") %>% html_text()
  address <- result %>% html_nodes(xpath = "(//result//formatted_address)[1]") %>% html_text()
  id <- result %>% html_nodes(xpath = "(//place_id)[1]") %>% html_text()
  row <- data.frame(address, id, lat, lon, input_address = nodes$address[z], stringsAsFactors=F)
  
  latlngdq <- rbind(latlngdq, row)
  print(z)
}
latlngdq <- latlngdq %>% select(1,3,4,2)
#write.csv(latlngdq, "dq_geocoded.csv", row.names=F)
latlngdq <- read.csv("dq_geocoded.csv", stringsAsFactors=F)

# create triangles of points - get all combinations of 3 pts
combinations <- combn(1:length(latlngdq$id), 3) %>% 
  t() %>% 
  data.frame() %>%
  mutate(mid_lon = NA, mid_lat= NA, mid_dist = NA)

# calculate the center of each cluster
# skipping it if other points fall inside that cluster
for(i in seq_along(combinations$X1)){
  these_pts <- latlngdq[c(combinations$X1[i], combinations$X2[i], combinations$X3[i]), ]
  not_these <- latlngdq[-c(combinations$X1[i], combinations$X2[i], combinations$X3[i]), ]
  
  mid_lon <- sum(these_pts$lon)/3
  mid_lat  <- sum(these_pts$lat)/3
  mid_dist  <- sqrt((these_pts$lon[1] - mid_lon)^2 + (these_pts$lat[1] - mid_lat)^2)
  
  # are there points inside of this cluster?
  closest <- min(sqrt((not_these$lon - mid_lon)^2 + (not_these$lat - mid_lat)^2))
  # if so, skip to the next cluster
  if(closest <= mid_dist) { next
  } else {
    combinations$mid_lon[i] <- mid_lon
    combinations$mid_lat[i] <- mid_lat
    combinations$mid_dist[i] <- mid_dist
  } 
  
}
# remove some that fell into WI
comb <- combinations %>% 
  filter(!is.na(mid_dist) & (mid_lon < -92.5 | mid_lat > 47)) %>%
  arrange(desc(mid_dist)) 

#write.csv(comb, "cluster_centers.csv", row.names=F)
comb <- read.csv("cluster_centers.csv", stringsAsFactors=F)

# take the top 1000 and re-calculate with Haversine distance
comb10 <- comb %>% top_n(1000, mid_dist) %>% mutate(hav = NA)
for(g in seq_along(comb10$mid_dist)){
  pt <- latlngdq[comb10$X1[g],]
  comb10$hav[g] <- distm(x=c(pt$lon, pt$lat), 
               y = c(comb10$mid_lon[g], comb10$mid_lat[g]), 
               fun = distHaversine) * 0.000621371
}


# Make the map
mn <- get_map(location = c(lon = -94.9083966, lat = 46.1818508), 
              zoom = 6, color="bw",
              maptype = "toner-lite") 
ggmap(mn) + 
  stat_density2d(data=latlngdq, 
                 aes(x=as.numeric(lon), y=as.numeric(lat), 
                     fill=..level.., alpha = ..level..),
                 geom="polygon", size=20, bins=20) +
  scale_fill_gradient(low="red", high="pink") +
  geom_point(aes(lon, lat), data = latlngdq, color = "dodgerblue4") + 
  annotate("point", x = comb10$mid_lon[1], y = comb10$mid_lat[1],  
           color = "red", size= 3, shape = 4, stroke = 3) +
  annotate("text", y = 48, x = -94.5, color = "red", hjust=0, vjust = 0.5,
           cex=3, label = "The MN town farthest\nfrom a DQ is Waskish, MN") +
  theme(legend.position = "none", 
        axis.title.x=element_blank(), axis.title.y=element_blank(), 
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) 

