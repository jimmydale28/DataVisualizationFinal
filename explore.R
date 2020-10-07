library(tidyverse)
library(sf)
library(ggplot2)
library(scales)

setwd("C:/Users/Jimmy/OneDrive/Desktop/Grad/Data Visualiztion/FinalProject")

############################################################################
###################### HOUSE DATASET #######################################
############################################################################

house_data <- read_csv("data/City_Zhvi_AllHomes.csv")

keycol  <- "years"
valuecol <- "med_sales_price"
gathercols <- c()

raw_names <- names(house_data)
for (i in 1:length(raw_names)){
  if (raw_names[i] %in% c("RegionID","RegionName", "State", "Metro",
                "CountyName", "SizeRank")){
  } else {
    gathercols[length(gathercols)+1] <- raw_names[i]
  }
}
  
house_data_reshape <- gather_(house_data, key_col = keycol, 
                              value_col = valuecol,
                              gather_cols = gathercols)

house_data_reshape <- house_data_reshape %>%
  separate(years, sep = "-", into=c("year", "month"))


############################################################################
###################### GDP DATASET #########################################
############################################################################

yearly_gdp_data <- read_csv("data/yearly_gdp.csv")

yearly_gdp_data <- yearly_gdp_data %>%
  select(-c("GDP in billions of current dollars")) %>%
  rename("Yearly GDP in billions" = "GDP in billions of chained 2009 dollars")

names(yearly_gdp_data)

quarterly_gdp_data <- read_csv("data/quaterly_gdp.csv")

quarterly_gdp_data <- quarterly_gdp_data %>%
  separate("Year", sep = "q", into = c("year", "quarter")) %>%
  select(-c("GDP in billions of current dollars")) %>%
  spread(key = "quarter", value="GDP in billions of chained 2009 dollars") %>%
  rename("Q1 GDP" = "1", "Q2 GDP" = "2", "Q3 GDP" = "3", "Q4 GDP" = "4")
  

############################################################################
###################### UNIVERSITIES DATASET ################################
############################################################################


university_raw <- read.delim("data/university_towns.txt",
                      header = FALSE, col.names = 'raw_txt')  %>%
  as_tibble()

university_clean <- university_raw %>%
  mutate(is_state = if_else(str_detect(raw_txt, "\\[edit\\]"), TRUE,FALSE),
         state = if_else(is_state, str_remove(raw_txt, "\\[edit\\]"), NA_character_),
         college_and_city = if_else(is_state, NA_character_, str_remove_all(raw_txt, "\\[[:digit:]+\\]")),
         city = str_remove_all(college_and_city, "\\s\\(.+\\)"),
         college = str_remove_all(str_extract(college_and_city, "\\(.+\\)"), "\\(|\\)"))  %>%
  tidyr::fill(state, .direction = 'down') %>%
  filter(!is.na(college)) %>%
  select(state, city, college)


############################################################################
###################### STATE DATASET #######################################
############################################################################

#shape_path <- "C:/Users/Jimmy/OneDrive/Desktop/Grad/Data Visualiztion/ShapeFiles/tl_2019_us_county.shp"
#shape <- st_read(shape_path)

#state <- read_csv('Data/STATE.csv')

#shape$STATEFP <- as.character(shape$STATEFP)
#state$STATEFP <- as.character(state$STATEFP)

#shape_and_state <- inner_join(shape, state, by = "STATEFP")

############################################################################
###################### FINAL DATASET #######################################
############################################################################

#house_data_reshape$year <- as.character(house_data_reshape$year)
#yearly_gdp_data$Year <- as.character(yearly_gdp_data$Year)

#final_data <- house_data_reshape %>% left_join(yearly_gdp_data,
#                                                by = c("year" = "Year")) %>%
#  left_join(quarterly_gdp_data, by = c("year" = "year")) %>%
#  left_join(university_clean, by = c("RegionName" = "state")) %>%
#  left_join(shape_and_state, by = c("RegionName" = "State", "CountyName" = "NAME"))


#data <- head(final_data, 100)
#output <- data %>%select(-c("geometry"))
#write.csv(output, "data.csv")

plot1_data <- yearly_gdp_data %>%
  filter(Year >= 1996) %>%
  group_by(Year) %>%
  summarise(GDP = na.omit(first(`Yearly GDP in billions`)))

ggplot(plot1_data, aes(x=Year, y=GDP, group=1)) +
  geom_line() +
  xlab("Year") + 
  ylab("Yearly GDP in Billions") + 
  scale_y_continuous(labels = dollar) +
  scale_x_continuous(breaks = pretty(plot1_data$Year, n = 15)) + 
  ggtitle("United States GDP from 1996-2015") +
  theme_minimal() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(size = 0.5, colour = "black"), 
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))

plot2_data <- house_data_reshape %>%
  filter(year <= 2015) %>%
  group_by(year) %>%
  summarise(average_price = mean(na.omit(med_sales_price)))
  
ggplot(plot2_data, aes(x=year, y=average_price, group=1)) +
  geom_line() +
  xlab("Year") + 
  ylab("Average Home Sale Price") + 
  scale_y_continuous(labels = dollar) +
  ggtitle("Average Home Sales Price from 1996-2015") +
  theme_minimal() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(size = 0.5, colour = "black"), 
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))


shapefile <- st_read("C:/Users/Jimmy/OneDrive/Desktop/Grad/Data Visualiztion/USStates/cb_2018_us_state_500k.shp")

shapes <- st_crop(shapefile,  xmin = -200, xmax = 0,
                  ymin = 20, ymax = 150)
plot2_data <- house_data_reshape %>%
  mutate(year_bin = cut(as.numeric(year), 
                        breaks = c(0, 1999, 2003, 2007, 2011, 2016),
                        labels = c("1996-1999", "2000-2003", "2004-2007", 
                                   "2008-2011", "2012-2015"))) %>%
  group_by(year_bin, State) %>%
  summarise(mean_sales = mean(na.omit(med_sales_price))) %>%
  inner_join(shapes, by=c("State" = "STUSPS"))
  

ggplot(plot2_data) +
  geom_sf(data=plot2_data, aes(fill=mean_sales, geometry=geometry)) +
  scale_fill_gradient(low = "blue", high = "red", 
                      name = "Average House Price",                              
                      labels = dollar) +      
  ggtitle("Average Home Sales Price per State from 1996-2015") +
  xlab("Longitude") +         
  ylab("Latitude") +          
  facet_wrap(~ year_bin) +         
  theme_minimal() +         
  theme(panel.border = element_blank(),               
        panel.grid.major = element_blank(),               
        panel.grid.minor = element_blank(),               
        axis.line = element_line(size = 0.5, colour = "black"),
        plot.title = element_text(hjust = 0.5))

house_data_state_groups <- house_data_reshape %>%
  group_by(year, State) %>%
  summarise(med_sales_price = mean(na.omit(med_sales_price)))

top10_states_2015 <- house_data_state_groups %>%
  filter(year == "2015") %>%
  arrange(-med_sales_price) %>%
  distinct(State, .keep_all = TRUE) %>%
  slice(1:10) %>%
  select(c(State, med_sales_price, year))

top10_states_1996 <- house_data_state_groups %>%
  filter(year == "1996") %>%
  arrange(-med_sales_price) %>%
  distinct(State, .keep_all = TRUE) %>%
  slice(1:10) %>%
  select(c(State, med_sales_price, year))

plot4_data <- bind_rows(top10_states_1996, top10_states_2015)

ggplot(data=plot4_data, aes(x=State, y=med_sales_price)) +
  geom_bar(stat = "identity", width = 0.5, fill="steelblue") +
  facet_wrap(~ year) +
  ggtitle("Top 10 States Average Home Price") +
  scale_y_continuous(labels = dollar) +
  xlab("State") +         
  ylab("Home Price") +          
  theme_minimal() +         
  theme(panel.border = element_blank(),               
        panel.grid.major = element_blank(),               
        panel.grid.minor = element_blank(),               
        axis.line = element_line(size = 0.5, colour = "black"),
        plot.title = element_text(hjust = 0.5))
#################################################################
#################################################################
 
#unique_state <- house_data_reshape %>%
#  arrange(State) %>%
#  distinct(State) 

#first_25_state <- house_data_reshape %>%
#  filter(State %in% unique_state$State[1:24]) %>%
#  mutate(state_bin = 0)

#last_25_state <- house_data_reshape %>%
#  filter(State %in% unique_state$State[25:50]) %>%
#  mutate(state_bin = 1)

#plot5_data <- bind_rows(first_25_state , last_25_state) %>%
#  drop_na(med_sales_price)

#ggplot(plot5_data, aes(x=State, y=med_sales_price)) +
#  geom_boxplot() +
#  coord_flip() +
#  facet_wrap(~ state_bin, scales = "free_y") +
#  ggtitle("State Home Sales Distribution") +
#  scale_y_continuous(labels = dollar) +
#  xlab("State") +         
#  ylab("Average Home Price from 1996 - 2015") +          
#  theme_minimal() +         
#  theme(panel.border = element_blank(),               
#        panel.grid.major = element_blank(),               
#        panel.grid.minor = element_blank(),               
#        axis.line = element_line(size = 0.5, colour = "black"),
#        plot.title = element_text(hjust = 0.5))

#ggplot(plot5_data, aes(x=State, y=med_sales_price)) +
#  geom_point()
#################################################################
#################################################################

state_data <- read_csv("data/STATE.csv")

plot5_data <- university_clean %>%
  separate_rows(college, sep=",") %>% 
  group_by(state) %>%
  summarise(n_colleges = n()) %>%
  inner_join(state_data, by = c("state" = "State")) %>%
  right_join(house_data_reshape, by = c("ABV" = "State")) %>%
  drop_na(med_sales_price, state) %>%
  group_by(state) %>%
  summarise(avg_sales_price = mean(med_sales_price),
            n_colleges = mean(n_colleges),
            region = first(Region))

write_csv(plot5_data, "plot5_data.csv")

ggplot(plot5_data, aes(x=avg_sales_price, y=n_colleges)) +
  geom_point(aes(color = region), size=3) +
  scale_color_manual(values=c("#999999", "#008000", "#56B4E9", "#FF0000")) +
  scale_x_continuous(label = dollar) +
  scale_y_continuous() +
  ggtitle("Number of Colleges VS Median Home Sales Price") +
  xlab("Median Sales Price") +         
  ylab("Number of Colleges per State") + 
  labs(color = "Region") +
  theme_minimal() +         
  theme(panel.border = element_blank(),               
        panel.grid.major = element_blank(),               
        panel.grid.minor = element_blank(),               
        axis.line = element_line(size = 0.5, colour = "black"),
        plot.title = element_text(hjust = 0.5))

plot6_data <- house_data_reshape %>%
  drop_na(med_sales_price) %>%
  filter(State %in% c("FL","CA", "NY", "NJ", "WA")) %>%
  group_by(State, year) %>%
  summarise(med_sales_price = mean(med_sales_price)) %>%
  mutate(pct_change = (med_sales_price/lag(med_sales_price, k = 1) - 1))

write_csv(plot6_data, "plot6_data.csv")

ggplot(plot6_data, aes(x=as.numeric(year), y=pct_change, group=1)) +
  geom_point(aes(color = State)) +
  scale_x_continuous() +
  ggtitle("State Home Sales Distribution") +
  scale_y_continuous(labels = percent) +
  xlab("Year") +         
  ylab("Percent Change over Time") +          
  theme_minimal() +         
  theme(panel.border = element_blank(),               
        panel.grid.major = element_blank(),               
        panel.grid.minor = element_blank(),               
        axis.line = element_line(size = 0.5, colour = "black"),
        plot.title = element_text(hjust = 0.5))
