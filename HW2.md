HW2
================
Xuesen Zhao
2022-10-01

## Question 1

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(readxl)
```

``` r
Transit = read_csv("./data/NYC_Transit_Subway_Entrance_And_Exit_Data.csv",col_types = cols(Route8 = "c", Route9 = "c", Route10 = "c", Route11 = "c")) %>%
  janitor::clean_names() %>% 
  select(line:entry,exit_only, vending,  
    ada) %>%
  mutate(entry = ifelse(entry=="YES",TRUE,FALSE)) %>%
  mutate(vending = ifelse(vending=="YES",TRUE,FALSE)) 
```

The original dataset contains information about the entrance and exit
for each listed subway station in NYC and it had 1868 observations and
32 rows. The column names were formatted and coerced into lower case,
and 20 variables were selected from the original dataset, including
line, station name, station latitude and longitude, whether a route
(1-11) stops at that station, entrance type, entry or exit, vending, and
ADA compliance. The resulting dataset now contains 1868 rows and 20
columns.

Yet, the dataset is not completely clean. There are many rows that
contains missing values, route number should be a separate variable.

``` r
# distinct station
distinct(Transit, station_name,line,.keep_all = TRUE) 
```

    ## # A tibble: 465 × 20
    ##    line     station_…¹ stati…² stati…³ route1 route2 route3 route4 route5 route6
    ##    <chr>    <chr>        <dbl>   <dbl> <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
    ##  1 4 Avenue 25th St       40.7   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  2 4 Avenue 36th St       40.7   -74.0 N      R      <NA>   <NA>   <NA>   <NA>  
    ##  3 4 Avenue 45th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  4 4 Avenue 53rd St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  5 4 Avenue 59th St       40.6   -74.0 N      R      <NA>   <NA>   <NA>   <NA>  
    ##  6 4 Avenue 77th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  7 4 Avenue 86th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  8 4 Avenue 95th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  9 4 Avenue 9th St        40.7   -74.0 F      G      R      <NA>   <NA>   <NA>  
    ## 10 4 Avenue Atlantic …    40.7   -74.0 B      Q      D      N      R      2     
    ## # … with 455 more rows, 10 more variables: route7 <chr>, route8 <chr>,
    ## #   route9 <chr>, route10 <chr>, route11 <chr>, entrance_type <chr>,
    ## #   entry <lgl>, exit_only <chr>, vending <lgl>, ada <lgl>, and abbreviated
    ## #   variable names ¹​station_name, ²​station_latitude, ³​station_longitude

``` r
# ADA compliant
Transit %>% 
  filter(ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 84 × 2
    ##    station_name                   line           
    ##    <chr>                          <chr>          
    ##  1 Atlantic Av-Barclays Ctr       4 Avenue       
    ##  2 DeKalb Av                      4 Avenue       
    ##  3 Pacific St                     4 Avenue       
    ##  4 Grand Central                  42nd St Shuttle
    ##  5 34th St                        6 Avenue       
    ##  6 47-50th Sts Rockefeller Center 6 Avenue       
    ##  7 Church Av                      6 Avenue       
    ##  8 21st St                        63rd Street    
    ##  9 Lexington Av                   63rd Street    
    ## 10 Roosevelt Island               63rd Street    
    ## # … with 74 more rows

``` r
# Entry = TRUE, AND vending = FALSE
Transit %>% 
  filter(vending == "NO") %>% 
  pull(entry) %>% 
  mean
```

    ## [1] NaN

There are 465 stations that have a distinct combination of station name
and line. 84 Stations are ada compliant. There is no station that has
entrance/exits without vending but allow entrance.

``` r
Transit %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A") %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 60 × 2
    ##    station_name                  line           
    ##    <chr>                         <chr>          
    ##  1 Times Square                  42nd St Shuttle
    ##  2 125th St                      8 Avenue       
    ##  3 145th St                      8 Avenue       
    ##  4 14th St                       8 Avenue       
    ##  5 168th St - Washington Heights 8 Avenue       
    ##  6 175th St                      8 Avenue       
    ##  7 181st St                      8 Avenue       
    ##  8 190th St                      8 Avenue       
    ##  9 34th St                       8 Avenue       
    ## 10 42nd St                       8 Avenue       
    ## # … with 50 more rows

``` r
Transit %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A", ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 17 × 2
    ##    station_name                  line            
    ##    <chr>                         <chr>           
    ##  1 14th St                       8 Avenue        
    ##  2 168th St - Washington Heights 8 Avenue        
    ##  3 175th St                      8 Avenue        
    ##  4 34th St                       8 Avenue        
    ##  5 42nd St                       8 Avenue        
    ##  6 59th St                       8 Avenue        
    ##  7 Inwood - 207th St             8 Avenue        
    ##  8 West 4th St                   8 Avenue        
    ##  9 World Trade Center            8 Avenue        
    ## 10 Times Square-42nd St          Broadway        
    ## 11 59th St-Columbus Circle       Broadway-7th Ave
    ## 12 Times Square                  Broadway-7th Ave
    ## 13 8th Av                        Canarsie        
    ## 14 Franklin Av                   Franklin        
    ## 15 Euclid Av                     Fulton          
    ## 16 Franklin Av                   Fulton          
    ## 17 Howard Beach                  Rockaway

There are 60 distinct stations that serve the A train, and 17 of them
are ADA compliant.

## Question 2

``` r
# The row with pic was excluded
Mr_wheel = read_excel("./data/Trash_Wheel.xlsx",sheet = 1, range = "A2:N549") %>%
  janitor::clean_names() %>% 
  mutate(Type="Mr",sports_balls=as.numeric(round(sports_balls,0)))

# The row with pic was excluded, 
Professor_wheel = read_excel("./data/Trash_Wheel.xlsx",sheet = 2, range = "A2:M96") %>%
  janitor::clean_names()%>%
  mutate(Type="Professor")
```

The first row in the original excel sheet was excluded because it
contained only a picture. The resulting dataframe Mr_wheel, was
generated based on the data from sheet 1, has 547 rows and 15 columns.
It included variables such as the amount of plastic bottles,
polystyrene, sports balls, grocery bags and other different types of
trash removed by the wheel. For instance, the mean value of sports balls
collected among these dumpster sites was NA. The Professor_wheel
dataframe was generated based on the second sheet of the original excel
file, and the first row which contained a picture was also skipped. The
resulting dataframe contains 94 rows and14 columns, including all
variables listed in Mr_wheel except the number of sports balls
collected. A type variable was added to each of the dataframe to
indicate the type of trash wheel.

``` r
# The larger df Trash_wheel(x) is left_joined to the Professor trash wheel (y)
combined_wheel = left_join(
  Mr_wheel, Professor_wheel, by="dumpster"
)
combined_wheel %>%
  filter(Type.y=="Professor") %>%
  pull(weight_tons.y)%>%
  sum
```

    ## [1] 190.12

``` r
combined_wheel %>%
  filter(Type.x=="Mr",year.x==2020)%>%
  pull(sports_balls)%>%
  sum
```

    ## [1] 856

The Mr_wheel and Professor_wheel data sets were combined, based on the
dumpster variable, to generate single dataframe that has 547 rows and 28
columns. The total weight of trash collected by Professor Trash Wheel
was 190.12 tons. The total number of sports balls collected by Mr. Trash
Wheel in 2020 was 856.

## Question 3

``` r
pol_month = read_csv("./data/pols-month.csv") %>%
  janitor::clean_names() %>%
  separate(mon, into = c("year","month","day"),sep = "-") %>%
  filter(prez_gop!=2)%>%
  mutate(
    year = as.numeric(year),
    month=month.abb[as.numeric(month)],
    president = ifelse(prez_gop==1,"gop","dem"))%>%
  select(-day,-prez_gop,-prez_dem)
```

    ## Rows: 822 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
snp = read_csv("./data/snp.csv")%>%
  janitor::clean_names() %>%
  separate(date, into = c("month","day","year"),sep = "/") %>% 
  mutate(
    year = as.numeric(year), 
    year=ifelse(year<=20,2000+year,1900+year),
    month=month.abb[as.numeric(month)],
        ) %>%
  select(year,month,close,-day)
```

    ## Rows: 787 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
unemploy = read_csv("./data/unemployment.csv") %>%
  janitor::clean_names()%>%
  pivot_longer(
    jan:dec,
    names_to = "month",
    values_to = "unemployment percentage"
  ) %>% 
  mutate(month=str_to_title(month))
```

    ## Rows: 68 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

The pol_month dataset contains information on the number of national
politicians who are democratic or republican from 1947 to 2015. The
cleaned dataset has 817 rows and 9 observations, including the variables
year, month, and president (indicator of if the president is a
republican or democratic), and the number of republican or democratic
governors (gov), senators (sen), and representative (rep) on the same
associated date. It does not make sense for a president to be both a
republican and a democrat, so the rows with a value of 2 for prez_gop
were omitted.

A month and year variable with the same format were also created in the
other two data frames as key variable for merging them together. The snp
dataset contains 787 rows and 3 variables, including year (with an range
of 1950, 2015), month and the closing values of the S&P stock index on
the associated date. The unemploy date contains 816 rows and 3 columns.
It includes the unemployment percentage of all months with a year range
of 1948, 2015.

``` r
merge1 = left_join(pol_month,snp,by=c("year","month"))
merged_final = left_join(merge1, unemploy, by=c("year","month"))
```

The final merged dataframe of all three contains 817 rows and 11
columns, including all the variables from the three dataframe. The range
of year for the merged data set is 1947, 2015.
