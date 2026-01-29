# Load the tidyverse library
setwd("/Users/muhammadkhalil/Documents/Upenn /SPRING 2026/CPLN 5920/Github/CPLN5920_yusufkhalil/labs/lab_0")

library(tidyverse)

# Read in the car sales data
# Make sure the data file is in your lab_0/data/ folder
car_data <- read_csv("data/car_sales_data.csv")

# Use glimpse() to see the data structure
glimpse(car_data)

# Check the column names
colnames(car_data)

# Look at the first few rows
head(car_data)

car_data

# Looking at normal data frames
car_df <- as.data.frame(car_data)
car_df

# Select just Model and Mileage columns
select(car_data, "Model","Mileage")

car_data %>% select(Manufacturer, Mileage)

# Select Manufacturer, Price, and Fuel type
select(car_data, "Manufacturer", "Price", "Fuel type")

car_data %>% select(Manufacturer, Price, 'Fuel type')

# Challenge: Select all columns EXCEPT Engine Size
car_data %>% select(!('Engine size'))

# Rename 'Year of manufacture' to year
##car_data %>% rename(year = `Year of manufacture`)

car_data <- rename(car_data, year = 'Year of manufacture')

# Check that it worked
names(car_data)

# Create an 'age' column (2025 minus year of manufacture)
car_data <- mutate(car_data, age = 2025 - year)

car_data

# Create a mileage_per_year column  
car_data <- mutate(car_data, mileage_per_year = Mileage/age)
car_data

# Look at your new columns
#select(car_data, Model, year, age, Mileage, mileage_per_year)
car_data %>% select(Model, year, age, Mileage, mileage_per_year)

# Create a price_category column where if price is < 15000, its is coded as budget, between 15000 and 30000 is midrange and greater than 30000 is mid-range (use case_when)
car_data <- mutate(car_data, price_category = case_when(Price < 15000 ~ "budget", Price > 30000 ~ "Luxury", Price >= 15000 & Price <= 30000 ~ "midrange"))

# Check your categories select the new column and show it
car_data %>% select(price_category)

# Find all Toyota cars
car_data %>% filter(Manufacturer == "Toyota")

# Find cars with mileage less than 30,000
car_data %>% filter(Mileage < 30000)

# Find luxury cars (from price category) with low mileage
car_data %>% filter(price_category == "Luxury", Mileage > 30000)


# Find cars that are EITHER Honda OR Nissan
car_data %>% distinct(Manufacturer)
car_data %>% filter(Manufacturer == "Honda" | Manufacturer == "Nissan")

# Find cars with price between $20,000 and $35,000
car_data %>% filter(Price >= 20000 & Price <= 35000)

# Find diesel cars less than 10 years old
car_data %>% filter(`Fuel type` == "Diesel" & age < 10)



# Calculate average price by manufacturer
avg_price_by_brand <- car_data %>%
  group_by(Manufacturer) %>%
  summarize(avg_price = mean(Price, na.rm = TRUE))

avg_price_by_brand


# Calculate average mileage by fuel type
avg_mileage_by_fueltype <- car_data %>% group_by(`Fuel type`) %>% summarise(avg_mileage = mean(Mileage))
avg_mileage_by_fueltype

# Count cars by manufacturer
car_count <- car_data %>% group_by(Manufacturer) %>% count(Manufacturer)

# Frequency table for price categories  
car_data %>% count(price_category)


  
  
