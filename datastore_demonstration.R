list.of.packages <- c("data.table", "httr", "jsonlite", "dotenv", "dplyr",
                      "sp","rgdal","leaflet","ggplot2","scales","rgeos","maptools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/iati-ce-datastore-demo")
load_dot_env()

API_KEY = Sys.getenv("API_KEY")
authentication = add_headers(`Ocp-Apim-Subscription-Key` = API_KEY)

# Activity recipient country codes
activity_recipient_path <- paste0("https://api.iatistandard.org/datastore/activity/select?",
  "q=*:*&facet=true&facet.field=recipient_country_code&facet.limit=1000000&wt=json")
activity_recipient_request <- GET(url = activity_recipient_path, authentication)
activity_recipient_response <- content(activity_recipient_request, encoding = "UTF-8")

activity_recipient_facets = activity_recipient_response$facet_counts$facet_fields$recipient_country_code
activity_recipient_counts = data.frame(
  recipient_country_code=unlist(activity_recipient_facets[c(TRUE, FALSE)]),
  activity_recipient_count=unlist(activity_recipient_facets[c(FALSE, TRUE)])
)

# Transaction recipient country codes
transaction_recipient_path <- paste0("https://api.iatistandard.org/datastore/activity/select?",
  "q=*:*&facet=true&facet.field=transaction_recipient_country_code&facet.limit=1000000&wt=json")
transaction_recipient_request <- GET(url = transaction_recipient_path, authentication)
transaction_recipient_response <- content(transaction_recipient_request, encoding = "UTF-8")

transaction_recipient_facets = transaction_recipient_response$facet_counts$facet_fields$transaction_recipient_country_code
transaction_recipient_counts = data.frame(
  recipient_country_code=unlist(transaction_recipient_facets[c(TRUE, FALSE)]),
  transaction_recipient_count=unlist(transaction_recipient_facets[c(FALSE, TRUE)])
)

# Merge and view 
all_recipient_counts = merge(activity_recipient_counts, transaction_recipient_counts, by="recipient_country_code", all=T)
all_recipient_counts$total_recipient_count = rowSums(
  all_recipient_counts[,c("activity_recipient_count", "transaction_recipient_count")], na.rm=T
)
all_recipient_counts = all_recipient_counts[order(-all_recipient_counts$total_recipient_count),]
View(all_recipient_counts)

world_path = "ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp"
world = readOGR(world_path)
iati.pal4 = c(
  "#ffffb2",
  "#fed976",
  "#feb24c",
  "#fd8d3c",
  "#f03b20",
  "#bd0026"
)
world.f = fortify(world,region="ISO_A2")
setnames(world.f,"id","recipient_country_code")

world.f$order = 1:nrow(world.f)
world.f = merge(world.f,all_recipient_counts,by="recipient_country_code",all.x=T)
world.f = world.f[order(world.f$order),]
palbins = c(1,50,500,5000,10000)
names(palbins)=c("1 activity","50 activities","500 activities","5,000 activities","10,000+ activities")

ggplot(world.f)+
  geom_polygon( aes(x=long,y=lat,group=group,fill=total_recipient_count,color="#eeeeee",size=0.21))+
  coord_fixed(1) +
  scale_fill_gradientn(
    na.value="#d0cccf",
    guide="legend",
    breaks=palbins,
    colors=iati.pal4,
    values=rescale(palbins)
  ) +
  scale_color_identity()+
  scale_size_identity()+
  expand_limits(x=world.f$long,y=world.f$lat)+
  theme_classic()+
  theme(axis.line = element_blank(),axis.text=element_blank(),axis.ticks = element_blank())+
  guides(fill=guide_legend(title=""))+
  labs(x="",y="")
