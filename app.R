library(tidyr)
library(shiny)
library(dplyr)
library(stringr)
library(tidyverse)
library(skimr)
library(ggmap)
library(ggplot2)
library(writexl)
library(plotly)
library(shinydashboard)
library(DT)
library(highcharter)
library(corrplot)
library(srvyr)
library(leaflet)
library(ggpubr)
library(lubridate)
library(RColorBrewer)
library(gridExtra)
library(shinyalert)
library(shinycssloaders)
library(zoo)

# 1 Loading and cleaning the data

# Replace the path with the location where you have extracted the files 
org_data <- load("D:/Archana DSTI/Big Data Processing with R/A23_Archana_Project_R/AirBnB.Rdata")  

# 2. Clean the AirBnB data -----------------------------------------------------
## 2.1 Select only the necessary variables and rename if necessary -------------

New_data <- select(L, listing_id =id, Host_id= host_id, Host_name= host_name, bathrooms, bedrooms, beds, bed_type, Equipments= amenities, Property_type= property_type, Room_type= room_type, Nb_of_guests= accommodates,Price= price, guests_included, minimum_nights, maximum_nights,availability_over_one_year= availability_365, instant_bookable, cancellation_policy, city, Adresse= street, Neighbourhood=neighbourhood_cleansed, city_quarter=zipcode, latitude, longitude, security_deposit, transit, host_response_time, Superhost= host_is_superhost, Host_since= host_since, Listing_count= calculated_host_listings_count, Host_score= review_scores_rating, reviews_per_month,number_of_reviews,square_feet)

#2.2 Removing the '$' character :

New_data$Price <- substring(gsub(",", "", as.character(New_data$Price)),2)

#2.3 Converting the data type
New_data$bedrooms <- as.numeric((New_data$bedrooms))
New_data$beds <- as.numeric((New_data$beds))
New_data$Price <- as.numeric((New_data$Price))
New_data$guests_included <- as.numeric((New_data$guests_included))
New_data$minimum_nights <- as.numeric((New_data$minimum_nights))
New_data$maximum_nights <- as.numeric((New_data$maximum_nights))
New_data$availability_over_one_year <- as.numeric((New_data$availability_over_one_year))
New_data$security_deposit <- as.numeric((New_data$security_deposit))
New_data$Listing_count <- as.numeric((New_data$Listing_count))
New_data$Host_score <- as.numeric((New_data$Host_score))
New_data$number_of_reviews <- as.numeric((New_data$number_of_reviews))
New_data$square_feet <- as.numeric((New_data$square_feet))
New_data$Neighbourhood <- as.character(New_data$Neighbourhood)
New_data$Host_since <- as.Date(New_data$Host_since)



#2.4 Setting the price range
New_data <- New_data %>%
  filter(New_data$Price >= 20 &
           New_data$Price <= 1300)

#2.5 Filling the missing values with the mean of bathrooms, bedrooms, and beds.

#Bathrooms
# Calculate the mean value of the "bathrooms" column
mean_value <- mean(New_data$bathrooms, na.rm = TRUE)
# Replace missing values with the mean value
New_data$bathrooms <- na.aggregate(New_data$bathrooms)
# Calculate the mean value again to see the value with which missing values were filled
mean_value_filled <- mean(New_data$bathrooms)
print(paste("Value with which missing values are filled:", mean_value_filled))

#Bedrooms
# Calculate the mean value of the "bedrooms" column
mean_value_bedrooms <- mean(New_data$bedrooms, na.rm = TRUE)
# Replace missing values with the mean value
New_data$bedrooms <- na.aggregate(New_data$bedrooms)
# Calculate the mean value again to see the value with which missing values were filled
mean_value_filled_bedrooms <- mean(New_data$bedrooms)
print(paste("Value with which missing values are filled", mean_value_filled_bedrooms))

#Beds
# Calculate the mean value of the "beds" column
mean_value_beds <- mean(New_data$beds, na.rm = TRUE)
# Replace missing values with the mean value
New_data$beds <- na.aggregate(New_data$beds)
# Calculate the mean value again to see the value with which missing values were filled
mean_value_filled_beds <- mean(New_data$beds)
print(paste("Value with which missing values are filled", mean_value_filled_beds))




#2.6 Removing the duplicates : 

New_data %>% distinct(listing_id, .keep_all = TRUE)

#2.7 Cleaning the city quarters (Arrondissements):

New_data$city = str_sub(New_data$city,1, 5)
New_data$city_quarter = str_sub(New_data$city_quarter, -2)
New_data <- subset(New_data, New_data$city == 'Paris' & New_data$city_quarter != "" & New_data$city_quarter <= 20 & New_data$city_quarter != '00' & New_data$city_quarter != ' ')


#2.8 A subset of the `New_data` dataset is created to find the relationship between price and apartment feature.

features_and_price <- New_data %>%
  select(Property_type,
         Room_type,
         bathrooms,
         bedrooms,
         beds,
         Neighbourhood,
         Nb_of_guests,
         Price)



#2.8 Going to keep the relevant and explicit property types to perform our analysis
list_property_types <- c("Apartment",
                         "Bed & Breakfast",
                         "Boat",
                         "Condominium",
                         "Dorm",
                         "House",
                         "Loft",
                         "Townhouse", 
                         "Villa")

features_and_price <- features_and_price %>%
  filter(Property_type %in% list_property_types)

#3 Computation
#3.1 Computing needed for visit frequency over year :

table <- inner_join(New_data, R,by = "listing_id")
tab1 <- select(New_data,listing_id,city,city_quarter)
table = mutate(table,year = as.numeric(str_extract(table$date, "^\\d{4}")))


#3.2 Computing needed for the number of apartments per owner :
count_by_host_1 <- New_data %>% 
  group_by(Host_id) %>%
  summarise(number_apt_by_host = n()) %>%
  ungroup() %>%
  mutate(groups = case_when(
    number_apt_by_host == 1 ~ "001",
    between(number_apt_by_host, 2, 50) ~ "002-050",
    number_apt_by_host > 50 ~ "051-153"))

count_by_host_2 <- count_by_host_1 %>%
  group_by(groups) %>%
  summarise(counting = n())

# Sort the count_by_host_2 data frame by the 'counting' column in descending order
count_by_host_2 <- count_by_host_2[order(-count_by_host_2$counting), ]



#3.3 Distribution by room type
room_types_counts <- table(features_and_price$Room_type)
room_types <- names(room_types_counts)
counts <- as.vector(room_types_counts)
percentages <- scales::percent(round(counts/sum(counts), 2))
room_types_percentages <- sprintf("%s (%s)", room_types, percentages)
room_types_counts_df <- data.frame(group = room_types, value = counts)

#3.4 Distribution by Property type :
property_type_df <- features_and_price %>%
  count(Property_type) %>%
  mutate(Percentage = n / sum(n))
table["date"] <- table["date"] %>% map(., as.Date)




#3.5Average price per Neighbourhood :

# Calculate average daily price per city quarter
average_prices_per_arrond <- aggregate(cbind(New_data$Price),
                                       by = list(arrond = New_data$city_quarter),
                                       FUN = function(x) mean(x))

#3.6 Price range within the neighbourhood
# Filter data for Paris
paris_data <- New_data %>%
  filter(city == "Paris" & !is.na(longitude) & !is.na(latitude) & longitude != "" & latitude != "")

# Calculate average price for each neighborhood
avg_price_per_neighborhood <- paris_data %>%
  group_by(Neighbourhood) %>%
  summarize(Avg_Price = mean(Price))

#3.7 Whole data map :

df <- select(L,longitude,neighbourhood,latitude,price)
df %>% select(longitude,neighbourhood,
              latitude,price)

#3.8 Superhost map :

dfsuperhost <- select(New_data,longitude,Neighbourhood,latitude,Price)
dfsuperhost <- filter(New_data, Superhost =="t")

#3.9 Hosts table :

listings_per_host <- New_data %>%
  group_by(Host_id, Host_name) %>%
  summarize(Num_Listings = n_distinct(listing_id)) %>%
  arrange(desc(Num_Listings))

## `summarise()` has grouped output by 'Host_id'. You can override using the
## `.groups` argument.

# Select the top 20 hosts with the highest number of listings
top_20_listings <- head(listings_per_host, 20)




###################################################################################



#4 Building the shiny app:


ui <- dashboardPage(
  
  dashboardHeader(title = "Airbnb Analysis"),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Dashboard",tabName="dashboard", icon=icon("dashboard")),
      
      menuItem("Analysis",tabName="firstanalysis", icon=icon("bar-chart-o"),  
               menuSubItem("Prices vs Apartments", tabName="apartments"),
               menuSubItem("Apartments per owner", tabName ="hosts"),
               menuSubItem("Price per Quarter", tabName="arrondissements"),
               menuSubItem("Visit Frequency", tabName="visits")),
      
      menuItem("Additional Analysis", tabName = "analysis", icon=icon("table"),
               menuSubItem("Price versus Features", tabName ="priceother"),
               menuSubItem("Further analysis", tabName="furtheranalysis")),
      
      menuItem("Maps",tabName="map", icon=icon("map")),
      
      menuItem("Raw Data",tabName="rawdata", icon=icon("database"))
    )),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "dashboard", 
              useShinyalert(),
              fluidRow(tags$head(tags$style(HTML(".small-box {height: 100px}"))),
                       valueBox("Paris", "France", icon = icon("map"), width = 3,color="teal"),
                       valueBoxOutput("Avg_price",  width = 3),
                       valueBoxOutput("Number_of_superhost",  width = 3),
                       valueBoxOutput("Listing_count",  width = 3)),
              fluidPage(tags$img(src= 'Paris image.png', width = "100%"))),
      
      tabItem(tabName ="apartments",
              #h1("Listings Dashboard"),
              
              fluidRow(
                box(title= "Distribution by room type", width = 6,
                    plotOutput("roomtype")%>% withSpinner(color="#0dc5c1"), status = "primary", solidHeader = FALSE, collapsible = TRUE),
                
                box(title= "Distribution by property type",width =6,
                    plotOutput("propertytype")%>% withSpinner(color="#0dc5c1"), status = "primary", solidHeader = FALSE, collapsible = TRUE)),
              fluidRow(
                box(title="Average Price according to Room Type", plotOutput("priceroom")%>% withSpinner(color="#0dc5c1"), width=6, 
                    solidHeader = FALSE, collapsible = TRUE ),
                box(title= "< Interactive plot >Distribution of listings under 1000 $",width =6,
                    plotlyOutput("numbertypelistings")%>% withSpinner(color="#0dc5c1"), status = "primary", solidHeader = FALSE, collapsible = TRUE))
      ),  
      
      tabItem(tabName ="hosts",
              #h1("Hosts Dashboard"),
              fluidRow(
                box(title= "Number of Apartments per owner", width = 6,  plotOutput("numberapart")%>% withSpinner(color="#0dc5c1"), 
                    status = "primary", solidHeader = FALSE, collapsible = TRUE),
                
                box(title= "Hosts in contrast with Superhosts",  plotOutput("superhosts")%>% withSpinner(color="#0dc5c1"), 
                    status = "primary", solidHeader = FALSE, width = 6, collapsible = TRUE)
              ),
              fluidRow(
                box(title = "Airbnb growth: Evolution of new hosts over time", width=12,plotOutput ("airbnbgrowth")%>% withSpinner(color="#0dc5c1"),
                    status = "primary", solidHeader = FALSE, collapsible = TRUE )
              ),
              fluidRow(
                box(title = "TOP 20 owners within the data (according to their number of Listings)", width=12, status= "success",solidHeader = FALSE, collapsible = TRUE,
                    DTOutput('tablehost')))
      ),
      
      tabItem(tabName ="visits",
              #h2("Analysis of the price / different features"),
              fluidRow(
                box(title= "< Interactive plot >    Listings by neighbourhood and room type",plotlyOutput("listings")%>% withSpinner(color="#0dc5c1"), width = 12, 
                    status = "success", solidHeader = FALSE, collapsible = TRUE)),
              fluidRow(  
                box(title= "< Interactive plot >    Visit frequency over the years", plotlyOutput("visitfreq")%>% withSpinner(color="#0dc5c1"), width = 12, 
                    status = "success", solidHeader = FALSE, collapsible = TRUE)
              ),
      ),
      
      tabItem(tabName ="priceother",
              #h2("Further analysis of renting prices"),
              
              fluidRow(
                box(title="Comparing Price with your selected feature",plotOutput("features")%>% withSpinner(color="#0dc5c1"), height = 600, width = 12, 
                    status = "warning", solidHeader = FALSE, collapsible = TRUE,
                    selectInput(inputId = "variable", "Choose a feature to analyse:", 
                                choices = c("Beds", "Bathrooms","Bedrooms"),
                                selected = NULL, multiple = FALSE))),
              fluidRow(
                box(title="Comparing all features", plotOutput("allfeatures")%>% withSpinner(color="#0dc5c1"), height = 600, width = 12, 
                    status = "warning", solidHeader = FALSE, collapsible = TRUE),
                
                box(title="Price versus features", plotOutput("pfeatures")%>% withSpinner(color="#0dc5c1"), height = 600, width = 12, 
                    status = "warning", solidHeader = FALSE, collapsible = TRUE),
              )),
      
      tabItem(tabName ="furtheranalysis",
              fluidRow(
                box(title= "Price and availability", plotOutput ("priceavailability")%>% withSpinner(color="#0dc5c1"),
                    status = "warning", solidHeader = FALSE, collapsible = TRUE),
                box(title= "Availability of the listings over a year", highchartOutput ("availabilityoveryear") %>% withSpinner(color="#0dc5c1"),
                    status = "warning", solidHeader = FALSE, collapsible = TRUE)),
              fluidRow(
                box(title= "Impact of 'Instant bookable' on the price", plotOutput("instantbookable")%>% withSpinner(color="#0dc5c1"), width=4, 
                    status = "warning", solidHeader = FALSE, collapsible = TRUE),
                box(title= "Impact of 'Cancellation policy' on the price",width = 4, plotOutput("cancelpolicy")%>% withSpinner(color="#0dc5c1"), 
                    status = "warning", solidHeader = FALSE, collapsible = TRUE),
                box(title= "Impact of 'Host response time' on the price", width = 4, plotOutput("responsetime")%>% withSpinner(color="#0dc5c1"), 
                    status = "warning", solidHeader = FALSE, collapsible = TRUE))),
      
      tabItem(tabName ="arrondissements",
              
              fluidRow(
                box(title= "< Interactive plot > Average price per neighbourhood",width = 12,
                    plotlyOutput("averageprice")%>% withSpinner(color="#0dc5c1"), 
                    status = "success", solidHeader = FALSE, collapsible = TRUE)),
              fluidRow(
                box(title= "Rented Apartments  over the years",width = 12,
                    plotOutput("numbrented")%>% withSpinner(color="#0dc5c1"), 
                    status = "success", solidHeader = FALSE, collapsible = TRUE)),
              fluidRow(
                box(title= "< Interactive plot > Price range within Neighbourhood", width = 12,
                    plotlyOutput("pricerangeneighbourhood",height = "600px")%>% withSpinner(color="#0dc5c1"),
                    status = "primary", solidHeader = FALSE, collapsible = TRUE),
                
                box(title= "< Interactive plot > Top 10 neighbourhoods by Number of listings"
                    ,width =12,plotlyOutput("top10neighbourhoods")%>% withSpinner(color="#0dc5c1"), 
                    status = "primary", solidHeader = FALSE, collapsible = TRUE)
              )),
      
      tabItem(tabName ="rawdata",
              skin = "blue",
              fluidRow(
                box(title = "Data used for the analysis", width=12, status= "success",solidHeader = FALSE, collapsible = TRUE,
                    DTOutput('table'))),
              fluidRow(
                box(status = "success", solidHeader = FALSE, 
                    shinyjs::useShinyjs(),
                    useShinyalert(), 
                    actionButton("init", "Download", icon = icon("download")),
                    downloadButton('downloadData', 'Download',style = "visibility: hidden;"))
              )),
      
      tabItem(tabName ="map",
              fluidPage(
                box(title = "Overview of the whole data", width = 12,
                    leafletOutput("mapoverview"), status = "success", solidHeader = FALSE, collapsible = TRUE),
                box(title = "Plotting only Superhosts listings", width = 12,
                    leafletOutput("superhostmap"), status = "success", solidHeader = FALSE, collapsible = TRUE)
              ))
    ) #tabItems
  )  #dashbody
  
)#dashPage

server <- function(input,output){
  
  # SHINY ALERT # 
  
  shinyalert("Welcome aboard !", "My name is Archana Khandelwal
  
              Analysing  Airbnb data for Paris
                     ")
  
  # VALUE BOXES #
  
  output$Avg_price <- renderValueBox({
    valueBox(
      round(mean(New_data$Price),0), "Average Price", icon = icon("dollar"),
      color = "aqua"
    )
  })
  
  output$Number_of_superhost <- renderValueBox({
    valueBox(
      sum(New_data$Superhost == "t"), "No. of superhosts", icon = icon("user"),
      color = "light-blue"
    )
  })
  
  output$Listing_count <- renderValueBox({
    valueBox(
      nrow(New_data), "Total no. of listings", icon = icon("list"),
      color = "blue"
    )
  })
  
  # PLOT 1 : No. of listings by Neighbourhood #
  
  output$listings <- renderPlotly({
    
    p4 <-  ggplot(New_data, aes(x = fct_infreq(Neighbourhood), fill = Room_type)) +
      geom_bar() +
      labs( x = "Neighbourhood", y = "Number of Listings") +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 75, hjust = 1), 
            plot.title = element_text(color = "black", size = 12,  hjust = 0.5))
    #scale_fill_brewer(palette="blues")
    ggplotly(p4)
  })
  
  # PLOT 2 : Average price by Neighbourhood #
  
  output$averageprice <- renderPlotly ({
    
    
    p3 <-price_arrond <- ggplot(data = average_prices_per_arrond, aes(x = arrond, y = V1)) +
      geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
      geom_text(aes(label = round(V1, 2)), size = 4) +
      coord_flip() +
      labs(title = "Average Daily Price per City Quarter",
           x = "City Quarters", y = "Average Daily Price") +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, hjust = 1), 
            plot.title = element_text(color = "black", size = 12,  hjust = 0.5))+
      scale_fill_brewer(palette= "Dark2") 
    ggplotly(p3)
  })
  
  # PLOT 3 : Number of Apartments by host #
  
  output$numberapart <- renderPlot ({
    
    ggplot(count_by_host_2, aes(x = groups, y = counting , fill = factor(groups))) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = counting), vjust = ifelse(count_by_host_2$groups == "001", 0.0, -0.3), size = 3) +  
      labs(title = "Number of Apartments per Host Group \n ",
           x = "Host Group",
           y = "Number of Apartments",
           fill = "Group") +
      theme_minimal()
   
  })
  
  # PLOT 4 : No. of Superhosts in the dataset #
  
  output$superhosts <- renderPlot ({
    
    ggplot(New_data) +
      geom_bar(aes(x = '', fill = Superhost)) +
      labs(title = "Contrast between Hosts and Superhosts",
           x = NULL,
           y = "Count",
           fill = "Superhost") +
      theme_minimal()
   
    
  })
  
  # PLOT 5: airbnb growth : number of new hosts over time #
  
  output$airbnbgrowth <- renderPlot({
    
    
    new_hosts_data <- drop_na(New_data, c("Host_since"))
    
    # Calculate the number of new hosts for each year (except for 2017 since our data is not complete for this year)
    new_hosts_data$Host_since <- as.Date(new_hosts_data$Host_since, '%Y-%m-%d')
    new_hosts_data <- new_hosts_data[new_hosts_data$Host_since < as.Date("2017-01-01"),]
    new_hosts_data <- new_hosts_data[order(as.Date(new_hosts_data$Host_since, format="%Y-%m-%d")),]
    new_hosts_data$Host_since <- format(as.Date(new_hosts_data$Host_since, "%Y-%m-%d"), format="%Y-%m")
    new_hosts_data_table <- table(new_hosts_data$Host_since)
    
    # Plot
    plot(as.Date(paste(format(names(new_hosts_data_table), format="%Y-%m"),"-01", sep="")), as.vector(new_hosts_data_table), type = "l", xlab = "Time", ylab = "Number of new hosts", col = "Blue")
  })
  
  # PLOT 6 : Distribution by Room type #
  
  output$roomtype <- renderPlot ({
     ggplot(room_types_counts_df, aes(x = "", y = value, fill = room_types_percentages)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      scale_fill_brewer("Room Types", palette = "BuPu") +
      #ggtitle("Distribution of Room types") +
      theme(plot.title = element_text(color = "black", size = 12, hjust = 0.5)) +
      ylab("") +
      xlab("") +
      labs(fill="") +
      theme(axis.ticks = element_blank(), panel.grid = element_blank(), axis.text = element_blank()) +
      geom_text(aes(label = percentages), size = 5, position = position_stack(vjust = 0.5))+theme_void()
  })
  
  # PLOT 7 :Distribution by Property type #
  
  output$propertytype <- renderPlot ({
    
    ggplot(property_type_df, aes(x = "", y = Percentage, fill = Property_type)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      scale_fill_manual("Property Types", values = custom_colors, labels = paste0(property_type_df$Property_type, ": ", scales::percent(property_type_df$Percentage))) +
      labs(#title = "Distribution of Property Types", 
           fill = "Property Types",
           y = "Percentage") +
      theme_void() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      guides(fill = guide_legend(title = "Property Types", label.position = "right"))+
      theme_void()
    
  })
  
  # PLOT 8 : Distribution of Listings Under $1,000 by Room type #
  
  output$numbertypelistings <- renderPlotly ({
    
    
    p7 <-ggplot(features_and_price, aes(x = Price, fill = Room_type)) +
      geom_histogram(position = "dodge") +
      scale_fill_manual(values = c("#efa35c", "#4ab8b8", "#1b3764"), name = "Room Type") +
      labs( x = "Price per night", y = "Number of listings") +
      theme(plot.title=element_text(vjust=2), 
            axis.title.x=element_text(vjust=-1, face = "bold"),
            axis.title.y=element_text(vjust=4, face = "bold"))
    ggplotly(p7)
  })
  
  # PLOT 9 : Top 10 neighbourhood by listings #
  
  output$top10neighbourhoods <- renderPlotly ({
    
    p30<- New_data %>%
      group_by(Neighbourhood) %>%
      dplyr::summarize(num_listings = n(), borough = unique(Neighbourhood)) %>%
      top_n(n = 10, wt = num_listings) %>%
      ggplot(aes(x = fct_reorder(Neighbourhood, num_listings), y = num_listings, fill = borough)) +
      geom_col() +
      coord_flip() +
      labs( x = "Neighbourhood", y = "Nb. of listings")
    
    ggplotly(p30)
  })
  
  # PLOT 10 : Price range by neighbourhood #
  
  output$pricerangeneighbourhood <- renderPlotly({
    
   
    p8 <- ggplot(paris_data, aes(x = Neighbourhood, y = Price, fill = Neighbourhood)) +
      geom_violin() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      labs( x = "Neighbourhood", y = "Price") +
      scale_fill_manual(values = rainbow(length(unique(paris_data$Neighbourhood))), 
                        guide = guide_legend(title = "Neighbourhood "),
                        breaks = avg_price_per_neighborhood$Neighbourhood,
                        labels = paste(avg_price_per_neighborhood$Neighbourhood,
                                " (", round(avg_price_per_neighborhood$Avg_Price, 2), ")"))
    ggplotly(p8)
  })
  
  # PLOT 11 : Price & Cancellation policy #
  
  output$cancelpolicy <- renderPlot ({
    
    ggplot(data = New_data, 
           aes(x = cancellation_policy, y = Price, color=cancellation_policy)) +
      geom_boxplot(outlier.shape = NA) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(plot.title = element_text(color = "#971a4a", size = 12, face = "bold", hjust = 0.5))+
      coord_cartesian(ylim = c(0, 1300))
    
  })
  
  # PLOT 12 : Price & host response time #
  
  output$responsetime <- renderPlot ({
    
    host_data_without_null_host_response_time <- subset(New_data, host_response_time != "N/A" & host_response_time != "")
    
    price_response_time <- ggplot(data = host_data_without_null_host_response_time, 
                                  aes(x = host_response_time, y = Price, color = host_response_time)) + 
      geom_boxplot(outlier.shape = NA) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(plot.title = element_text(color = "#971a4a", size = 12, face = "bold", hjust = 0.5)) +
      coord_cartesian(ylim = c(0, 500))
    price_response_time
  })
  
  # PLOT 13 : Price & selected feature #
  
  output$features <- renderPlot ({
    
    
    if(input$variable == 'Bathrooms'){
      
      a<- ggplot(data = features_and_price, aes(x = bathrooms, y = Price, color=bathrooms)) +
        geom_jitter(width = 0.1,height = 0.2,size=0.1)
      
      plot(a)
      
    }
    
    if(input$variable == 'Bedrooms'){
      b <- ggplot(data = features_and_price, aes(x = bedrooms, y = Price, color=bedrooms)) +
        geom_jitter(width = 0.1,height = 0.2,size=0.1)
      plot(b)
    }
    
    if(input$variable == 'Beds'){
      c <- ggplot(data = features_and_price, aes(x = beds, y = Price, color=beds)) +
        geom_jitter(width = 0.1,height = 0.2,size=0.1)
      plot(c)
    }
    
  })
  
  # PLOT 14 : Price & all features #
  
  output$allfeatures <- renderPlot ({
    
    a1<- ggplot(data=features_and_price) +
      geom_smooth(mapping = aes(x=Price,y=beds), method = 'gam', col='grey')
    a2<- ggplot(data=features_and_price) +
      geom_smooth(mapping = aes(x=Price,y=bedrooms),method = 'gam', col='blue')
    a3<- ggplot(data=features_and_price) +
      geom_smooth(mapping = aes(x=Price,y=bathrooms),method = 'gam', col='violet')
    a4<- ggplot(data=features_and_price) +  
      geom_smooth(mapping = aes(x=Price,y=Nb_of_guests),method = 'gam', col='black')
    
    ggarrange(
      a1,
      a2,
      a3,
      a4,
      nrow=2,
      ncol=2,
      align = "hv")
    
  })
  
  # PLOT 15 : Average price by room type #
  
  output$priceroom <- renderPlot ({
    
    features_and_price %>% 
      group_by(Room_type) %>% 
      summarise(mean_price = mean(Price, na.rm = TRUE)) %>% 
      ggplot(aes(x = reorder(Room_type, mean_price), y = mean_price, fill = Room_type)) +
      geom_col(stat ="identity", fill="#56478b") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Room Type", y = "Price") +
      geom_text(aes(label = round(mean_price,digit = 2)), hjust = 1.0, color = "white", size = 4.5) +
      #ggtitle("Average Price by Room Type") + 
      xlab("Room Type") + 
      ylab("Average Price")
  })
  
  # PLOT 16 : Visit frequency over years #
  
  output$visitfreq <- renderPlotly ({
    
    p6 <- ggplot(table) +
      geom_bar(aes(y =city_quarter ,fill=factor(year)))+
      scale_size_area() +
      labs( x="Frequency", y="City quarter",fill="Year")+
      scale_fill_brewer(palette ="Spectral")
    
    ggplotly(p6)
  })
  
  # PLOT 17 : Number of rented apartments #
  
  output$numbrented <- renderPlot ({
    table["date"] <- table["date"] %>% map(., as.Date)
    
    #No. of rented apartment over the year
    longitudinal  <- table %>%
      group_by(date, Neighbourhood) %>%
      summarise(count_obs = n())
    
   
    ggplot(longitudinal,aes(x = date,y = count_obs,group = 1))+
      geom_line(size = 0.5,colour = "lightblue")+
      stat_smooth(color = "darkblue", method = "loess")+
      scale_x_date(date_labels = "%Y")+
      labs(x = "Year",y = "No. Rented Appartment")+
      facet_wrap(~ Neighbourhood)
    
  })
  
  # PLOT 18 : Price and availability #
  
  output$priceavailability <- renderPlot ({
    
    ggplot( New_data, aes(availability_over_one_year, Price)) +
      geom_point(alpha = 0.2, color = "#971a4a") +
      geom_density(stat = "identity", alpha = 0.2) +
      xlab("Availability over a year") +
      ylab("Price") 
      #ggtitle("Relationship between availability and price") 
  })
  
  # PLOT 19 : availability over year #
  
  output$availabilityoveryear <- renderHighchart ({
    
    hchart(New_data$availability_over_one_year, color = "#336666", name = "Availability") %>%
      hc_title(text = "Availability of listings") %>%
      hc_add_theme(hc_theme_ffx())
  })
  
  # PLOT 20 : price & 'instant bookable' #
  
  output$instantbookable <- renderPlot({
    
    ggplot(data = New_data, aes(x = instant_bookable, y = Price, color = instant_bookable)) +
      geom_boxplot(outlier.shape = NA) +coord_cartesian(ylim = c(0, 500))
    
  })
  
  # PLOT 21 : map overview #
  
  output$mapoverview <- renderLeaflet ({
    
    leaflet(df) %>%  
      setView(lng = 2.3488, lat = 48.8534 ,zoom = 10) %>%
      addTiles() %>% 
      addMarkers(clusterOptions = markerClusterOptions()) %>%
      addMiniMap()
    
  })
  
  # PLOT 22 : map overview #
  
  output$superhostmap <- renderLeaflet ({
    
    leaflet(dfsuperhost %>% select(longitude,Neighbourhood,
                                   latitude,Price))%>%
      setView(lng = 2.3488, lat = 48.8534 ,zoom = 10) %>%
      addTiles() %>% 
      addMarkers(clusterOptions = markerClusterOptions()) %>%
      addMiniMap()
    
  })
  
  # PLOT 23 : Price versus features
  output$pfeatures <- renderPlot ({
     ggplot(data = features_and_price) +
      geom_smooth(mapping = aes(x = Price, y = beds, col = 'beds'), method = 'gam') +
      geom_smooth(mapping = aes(x = Price, y = bedrooms, col = 'bedrooms'), method = 'gam') +
      geom_smooth(mapping = aes(x = Price, y = bathrooms, col = 'bathrooms'), method = 'gam') +
      geom_smooth(mapping = aes(x = Price, y = Nb_of_guests, col = 'Nb_of_guests'), method = 'gam') +
      ggtitle("Price versus features") + labs(y = "Features", x = "Price") +
      scale_fill_manual()

    
    # Return the plotly object
    pfeatures
   
    
  })
  
  # OUTPUT 23 : DATA SET #
  
  output$table <- renderDT(
    New_data[,c("listing_id","Host_name","Price","Neighbourhood","city_quarter")], 
    caption = 'This is a simple caption of the table',
    options = list(searching = FALSE,pageLength = 5, dom = 't' )
  )
  
  # OUTPUT 24 : DATA TOP 20 HOSTS #
  
  output$tablehost <- renderDT(
    top_20_listings, options = list(searching = FALSE,pageLength = 5
    ))
  
  # OUTPUT 25 : DOWNOALD THE DATA SET #
  
  global <- reactiveValues(response = FALSE)
  
  observeEvent(input$init,{
    shinyalert("Confirmation", 
               "Do you want to download the data?", 
               type = "success",
               callbackR = function(x) {
                 global$response <- x
               },
               showCancelButton = TRUE
    )
  })
  
  
  # OUTPUT 26 : DOWNOALD BUTTON #
  
  observeEvent(global$response,{
    if(global$response){
      shinyjs::runjs("document.getElementById('downloadData').click();")
      global$response <- FALSE}
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {paste("data-", Sys.Date(), ".csv", sep="")},
    content = function(con) {write.csv(New_data, con)})
  
}

shinyApp(ui, server)



















