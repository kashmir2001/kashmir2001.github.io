library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(readxl)
library(tidyr)
library(forcats)

#VIS 1

censor <- read_excel('censor.xlsx', sheet = "2024", skip = 1, range = "A2:Z40")
new_col_names <- c('iso_a2_censor','country','num_restrictions','scope','protest','election',
                   'politics','internet_law','social_media','local','nationwide','cases_2015','current',
                   'region')
colnames(censor) <- new_col_names
censor$iso_a2_censor[censor$iso_a2_censor == "J&K"] <- "IN"
censor <- censor[,1:14]
censor <- censor 
View(censor)

world <- ne_countries(scale = "medium", returnclass = "sf", type = "countries") %>%
  filter(continent %in% c("Europe", "Africa", "Asia"))   %>%
  st_crop(world, xmin = -25, xmax = 150, ymin = -40, ymax = 75)

censor_summary <- censor %>%
  group_by(iso_a2_censor) %>%
  summarize(total = sum(num_restrictions, na.rm = TRUE))%>%
  mutate(total = na_if(total, 0)) 

world_data <- left_join(world, censor_summary, by = c("iso_a2" = "iso_a2_censor")) 
View(world_data)

ggplot() +
  geom_sf(data = world_data, aes(fill = total), color = "black") +  
  scale_fill_gradient(
    low='lightpink',
    high='red4',
    name='Number of internet restrictions',
    na.value='lightgray') +  
  coord_sf(crs = st_crs(4326)) +  
  labs(title = "India leads in internet censorships imposed in 2024",
       subtitle = "Number of newly imposed internet restrictions by country in 2024",
       caption = "Source: Surfshark") +
  geom_sf_text(data = world_data[world_data$total>=8, ], 
               aes(label = admin), size = 3, color = "black") +
  theme_void()

#VIS 2
censor_summary <- censor %>%
  group_by(iso_a2_censor) %>%
  summarize(
    protests = sum(protest, na.rm = TRUE),
    elections = sum(election, na.rm = TRUE),
    politics = sum(politics, na.rm = TRUE),
    internet_law = sum(internet_law, na.rm = TRUE)
  )
View(censor_summary)

censor_long <- censor_summary %>%
  pivot_longer(cols = c(protests, elections, politics, internet_law), 
               names_to = "category", values_to = "count")
View(censor_long)

complete_data <- expand.grid(iso_a2_censor = world$iso_a2, 
                             category = c("protests", "elections", "politics", "internet_law")) %>%
  left_join(censor_long, by = c("iso_a2_censor", "category")) 
View(complete_data)

world_data <- left_join(world, complete_data, by = c("iso_a2" = "iso_a2_censor")) %>%
  mutate(count = na_if(count, 0)) 
world_data <- world_data[!is.na(world_data$category),]
View(world_data)

world_data$category <- factor(world_data$category, 
                              levels = c("protests", "politics", "internet_law", "elections"))
world_data$category <- fct_recode(world_data$category,
                                           "political turmoil" = "politics")

ggplot(data = world_data) +
  geom_sf(aes(fill = count), color = "black") +
  scale_fill_gradient(
    high = "midnightblue",
    low = "cyan1",
    name = "Count",
    na.value='lightgray'
  ) +
  coord_sf(crs = st_crs(4326)) +
  labs(title = "India's 2024 internet censorships highly related to protests and political turmoil",
       subtitle = "Count of internet restrictions imposed in 2024 related to protests, political turmoil, internet law, and elections by country",
       caption = "Source: Surfshark") +
  theme_void() +
  facet_wrap(~ category)  

#VIS 3
censor <- read_excel('censor.xlsx', sheet = "2024", skip = 43)
new_col_names <- c('iso_a2_censor','start_date','end_date','hours','protest','election',
                   'politics','internet_law','scope','region','pop','facebook','twitter',
                   'youtube','instagram','telegram','whatsapp','other_social','other_voip')
last_5_col_names <- colnames(censor)[(ncol(censor) - 4):ncol(censor)]
colnames(censor) <- c(new_col_names, last_5_col_names)
censor$iso_a2_censor[censor$iso_a2_censor == "J&K"] <- "IN"
View(censor)

censor_summary <- censor %>%
  group_by(iso_a2_censor) %>%
  summarize(
    facebook = sum(facebook, na.rm = TRUE),
    twitter = sum(twitter, na.rm = TRUE),
    youtube = sum(youtube, na.rm = TRUE),
    instagram = sum(instagram, na.rm = TRUE),
    telegram = sum(telegram, na.rm = TRUE),
    whatsapp = sum(whatsapp, na.rm = TRUE)
    )
View(censor_summary)

censor_long <- censor_summary %>%
  pivot_longer(cols = c(facebook, twitter, youtube, instagram,telegram,whatsapp), 
               names_to = "category", values_to = "count")
View(censor_long)

world <- ne_countries(scale = "medium", returnclass = "sf", type = "countries") %>%
  filter(continent %in% c("Europe", "Africa", "Asia", "South America")) 

complete_data <- expand.grid(iso_a2_censor = world$iso_a2, 
                             category = c("facebook", "twitter", "youtube", "instagram","telegram","whatsapp")) %>%
                 left_join(censor_long, by = c("iso_a2_censor", "category")) 
View(complete_data)

world_data <- left_join(world, complete_data, by = c("iso_a2" = "iso_a2_censor")) %>%
  mutate(count = na_if(count, 0)) 
world_data <- world_data[!is.na(world_data$category),]
View(world_data)
world_data$category <- factor(world_data$category,
                              levels = c("twitter", "instagram", "facebook", "youtube", "telegram", "whatsapp"))
world_data$count_factor <- factor(world_data$count, levels = c(4,3,2,1))

ggplot(data = world_data) +
  geom_sf(aes(fill = count_factor), color = "black") +
  scale_fill_manual(
    values = c("1" = "palegreen1", "2" = "palegreen3", "3" = "seagreen4", "4" = "darkgreen"),
    na.value = "gray90",
    name='Count'
  ) +
  coord_sf(crs = st_crs(4326)) +
  coord_sf(xlim = c(-90, 180), ylim = c(-60, 90), crs = st_crs(4326)) +  
  labs(title = "Pakistan dominates social media restrictions in 2024",
       subtitle = "Count of social media restrictions during 2024 by country and app",
       caption = "Source: Surfshark")+
  theme_void() +
  facet_wrap(~ category) 

