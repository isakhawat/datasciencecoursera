#####################################################
# Corona Virus Data Visualization
####################################################


#Loading Packages
library(tidyverse) 
library(dplyr)
library(plotly)
library(ggplot2)
library(IRdisplay)
library(leaflet)
library(leafpop)
library(dplyr)
library(purrr) 

#Load data
coronavirus <- read.csv("../coronavirus_dataset.csv",header=TRUE)
coronavirus  <- coronavirus %>% rename(dateVals=date)
coronavirus$dateVals = as.Date(coronavirus$dateVals)

#confirmed_color <- "purple"
#confirmed_color <- "#FFF857"
#active_color <- "#1f77b4"
#recovered_color <- "forestgreen"
#death_color <- "red"

confirmed_color <- "#FFF857"
active_color <- "#FF7600"
recovered_color <- "#FF3C00"
death_color <- "red"


df <- coronavirus %>% 
  dplyr::group_by(Country.Region, type) %>%
  dplyr::summarise(total = sum(cases)) %>%
  tidyr::pivot_wider(names_from =  type, 
                     values_from = total) %>%
  dplyr::mutate(unrecovered = confirmed - ifelse(is.na(recovered), 0, recovered) - ifelse(is.na(death), 0, death)) %>%
  dplyr::arrange(-confirmed) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(country = dplyr::if_else(Country.Region == "United Arab Emirates", "UAE", as.character(Country.Region))) %>%
  dplyr::mutate(country = dplyr::if_else(country == "Mainland China", "China", as.character(country))) %>%
  dplyr::mutate(country = dplyr::if_else(country == "North Macedonia", "N.Macedonia", as.character(country))) %>%
  dplyr::mutate(country = trimws(country)) %>%
  dplyr::mutate(country = factor(country, levels = country))

df1 <- coronavirus %>% dplyr::filter(dateVals == max(dateVals))

df_daily <- coronavirus %>% 
  dplyr::group_by(dateVals, type) %>%
  dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = type,
                     values_from = total) %>%
  dplyr::arrange(dateVals) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(active =  confirmed - death - recovered) %>%
  dplyr::mutate(confirmed_cum = cumsum(confirmed),
                death_cum = cumsum(death),
                recovered_cum = cumsum(recovered),
                active_cum = cumsum(active))


df1 <- coronavirus %>% dplyr::filter(dateVals == max(dateVals))



#Raw Data
glimpse(coronavirus)


#Curated Data
glimpse(df)

####Visualisations
#BarChart

p<-plot_ly(data = df[1:30,], 
           x = ~ country, 
           y = ~ unrecovered, 
           # text =  ~ confirmed, 
           # textposition = 'auto',
           type = "bar", 
           name = "Active",
           marker = list(color = active_color)) %>%
  add_trace(y = ~ recovered, 
            # text =  ~ recovered, 
            # textposition = 'auto',
            name = "Recovered",
            marker = list(color = recovered_color)) %>%
  add_trace(y = ~ death, 
            # text =  ~ death, 
            # textposition = 'auto',
            name = "Death",
            marker = list(color = death_color)) %>%
  layout(barmode = 'stack',
         yaxis = list(title = "Total Cases (log scaled)", type = "log"),
         xaxis = list(title = ""),
         hovermode = "compare",
         margin =  list(
           # l = 60,
           # r = 40,
           b = 10,
           t = 10,
           pad = 2
         )
  )

ggplotly(p)

## Trend
# Time series of total, recovered and death cases

p2<-plot_ly(data = df_daily) %>%
  add_trace(x = ~ dateVals,
            y = ~ active_cum,
            type = "scatter",
            mode = "lines+markers",
            name = "Active",
            line = list(color = active_color),
            marker = list(color = active_color)) %>%
  add_trace(x = ~ dateVals,
            y = ~ recovered_cum,
            type = "scatter",
            mode = "lines+markers",
            name = "Recovered",
            line = list(color = recovered_color),
            marker = list(color = recovered_color)) %>%
  add_trace(x = ~ dateVals,
            y = ~ death_cum,
            type = "scatter",
            mode = 'lines+markers',
            name = "Death",
            line = list(color = death_color),
            marker = list(color = death_color)) %>%
  add_annotations(x = as.Date("2020-03-01"),
                  y = 42716,
                  text = paste("# of recovered cases surpass", "<br>", "the # of active cases"),
                  xref = "x",
                  yref = "y",
                  arrowhead = 5,
                  arrowhead = 3,
                  arrowsize = 1,
                  showarrow = TRUE,
                  ax = -10,
                  ay = 90) %>%
  layout(title = "",
         yaxis = list(title = "Cumulative Number of Cases"),
         xaxis = list(title = "Date"),
         legend = list(x = 0.1, y = 0.9),
         hovermode = "compare")

ggplotly(p2)


### New Cases today - Top 15 Countries (2020-04-30)
max_date <- max(coronavirus$dateVals)
coronavirus %>% 
  dplyr::filter(type == "confirmed", dateVals == max_date) %>%
  dplyr::group_by(Country.Region) %>%
  dplyr::summarise(total_cases = sum(cases)) %>%
  dplyr::arrange(-total_cases) %>%
  dplyr::mutate(country = factor(Country.Region, levels = Country.Region)) %>%
  dplyr::ungroup() %>%
  dplyr::top_n(n = 25, wt = total_cases) %>%
  plotly::plot_ly(x = ~ country,
                  y = ~ total_cases,
                  text = ~ total_cases,
                  textposition = 'auto',
                  type = "bar") %>%
  plotly::layout(yaxis = list(title = "Number of Cases"),
                 xaxis = list(title = ""),
                 margin =  list(
                   l = 10,
                   r = 10,
                   b = 10,
                   t = 10,
                   pad = 2
                 ))



#Daily New Cases - India vs. Rest of the World

daily_confirmed <- coronavirus %>%
  dplyr::filter(type == "confirmed") %>%
  dplyr::mutate(country = dplyr::if_else(Country.Region == "Switzerland", 
                                         "switzerland", 
                                         "Rest of the World")) %>%
  dplyr::group_by(dateVals, country) %>%
  dplyr::summarise(total = sum(cases, rm.na=TRUE)) %>% 
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = country, values_from = total) 

daily_confirmed %>%
  plotly::plot_ly() %>% 
  plotly::add_trace(x = ~ dateVals, 
                    y = ~ switzerland, 
                    type = "scatter", 
                    mode = "lines+markers",
                    name = "switzerland") %>% 
  plotly::add_trace(x = ~ dateVals, 
                    y = ~ `Rest of the World`, 
                    type = "scatter", 
                    mode = "lines+markers",
                    name = "Rest of the World") %>% 
  plotly::layout(title = "",
                 legend = list(x = 0.1, y = 0.9),
                 yaxis = list(title = "Number of New Cases"),
                 xaxis = list(title = "Date"),
                 # paper_bgcolor = "black",
                 # plot_bgcolor = "black",
                 # font = list(color = 'white'),
                 hovermode = "compare",
                 margin =  list(
                   # l = 60,
                   # r = 40,
                   b = 10,
                   t = 10,
                   pad = 2
                 ))


### Recovery and death rates for countries with at least 100 cases

coronavirus2 <- coronavirus %>% 
  dplyr::group_by(Country.Region, type) %>%
  dplyr::summarise(total_cases = sum(cases)) %>%
  tidyr::pivot_wider(names_from = type, values_from = total_cases) %>%
  dplyr::arrange(- confirmed) %>%
  dplyr::filter(confirmed >= 100) %>%
  dplyr::mutate(recover_rate = recovered / confirmed,
                death_rate = death / confirmed) %>% 
  dplyr::mutate(recover_rate = dplyr::if_else(is.na(recover_rate), 0, recover_rate),
                death_rate = dplyr::if_else(is.na(death_rate), 0, death_rate)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(confirmed_normal = as.numeric(confirmed) / max(as.numeric(confirmed)))


plotly::plot_ly(y = ~ round(100 * coronavirus2$recover_rate, 1),
                x = ~ round(100 * coronavirus2$death_rate, 1),
                size = ~  log(coronavirus2$confirmed),
                sizes = c(5, 70),
                type = 'scatter', mode = 'markers',
                color = ~ coronavirus2$Country.Region,
                marker = list(sizemode = 'diameter' , opacity = 0.5),
                hoverinfo = 'text',
                text = ~paste("</br>", coronavirus2$Country.Region, 
                              "</br> Confirmed Cases: ", coronavirus2$confirmed,
                              "</br> Recovery Rate: ", paste(round(100 * coronavirus2$recover_rate, 1), "%", sep = ""),
                              "</br> Death Rate: ",  paste(round(100 * coronavirus2$death_rate, 1), "%", sep = ""))
) %>%
  plotly::layout(yaxis = list(title = "Recovery Rate", ticksuffix = "%"),
                 xaxis = list(title = "Death Rate", ticksuffix = "%", 
                              dtick = 1, 
                              tick0 = 0),
                 hovermode = "compare")






#################################################################
#Analyzing and Predicting COVID-19 In Brazil
#################################################################



#Load data 
# Importing libraries
library(tidyverse)
library(tidymodels)
library(gridExtra)
library(geobr)
library(sf)

# Configuring enviroment
options(warn=-1)
options(repr.plot.width = 14, repr.plot.height = 8)

theme <-
  theme_minimal() +
  theme(
    plot.title = element_text(size=18,face="bold", color="gray30"),
    plot.subtitle = element_text(size=15, color="gray30"),
    axis.title = element_text(size=15, color="gray30"),
    plot.caption = element_text(size=15, color = "gray60"),
    legend.text = element_text(size=15, color="gray30"),
    legend.title = element_text(size=15, color="gray30"),
    strip.text.x = element_text(size=13, color="gray30")
  )

theme_set(theme)

# Importing and printing dataset
covid <- read_csv("../brazil-covid19/brazil_covid19.csv")
population <- read_csv("../brazil-covid19/population.csv")
states <- read_state(year=2018)

tail(covid,5)
max_date <- max(covid$date)
min_date <- min(covid$date)
print(paste("Last execution: ", format(Sys.Date(),format="%Y-%m-%d")))
print(paste("Data from ", min_date, " to ", max_date))


# creating dataset for the overall charts
covid_overall <-
  group_by(covid,date) %>%
  filter(cases > 0) %>%
  summarise(cases = sum(cases),deaths = sum(deaths)) %>%
  mutate(
    new_cases = cases - lag(cases,1,0),
    growth_cases = round((new_cases/cases) * 100,2),
    new_deaths = deaths - lag(deaths,1,0),
    growth_deaths = round(replace_na(new_deaths/deaths,0) * 100,2),
    day = row_number(date)-1
  ) %>%
  arrange(date)

# creating datasets for the regions chart
covid_regions <-
  group_by(covid,region,date) %>%
  filter(cases > 0) %>%
  summarise(cases = sum(cases),deaths = sum(deaths)) %>%
  mutate(
    new_cases = cases - lag(cases,1,0),
    growth_cases = round((new_cases/cases) * 100,2),
    new_deaths = deaths - lag(deaths,1,0),
    growth_deaths = round(replace_na(new_deaths/deaths,0) * 100,2),
    day = row_number(date)-1
  ) %>%
  arrange(region,date)

# creating dataset for states chart
covid_states <-
  group_by(covid,state,region,date) %>%
  filter(cases > 0) %>%
  summarise(cases = sum(cases),deaths = sum(deaths)) %>%
  mutate(
    new_cases = cases - lag(cases,1,0),
    growth_cases = round((new_cases/cases) * 100,2),
    new_deaths = deaths - lag(deaths,1,0),
    growth_deaths = round(replace_na(new_deaths/deaths,0) * 100,2),
    day = row_number(date)-1
  ) %>%
  arrange(state,region,date)

# printing datasets
print("Overall Dataframe")
tail(covid_overall,10)
print("Regional Dataframe")
tail(covid_regions,10)
print("State Dataframe")
tail(covid_states,10)


######### Analyzing
# With the new variables, we're going to create some visualizations in order to get a broader view of the disease and answer some questions.
# Plotting the new daily cases and deaths using a logarithmic scale on the y-axis, give us the perception that the cases and deaths are probably increasing in a similar rate, although deaths are increasing on a lower scale.

ggplot(covid_overall) + 
  geom_line(aes(day,new_cases, color="cyan4"),size=1) + 
  geom_point(aes(day,new_cases, color="cyan4"),size=3) + 
  geom_line(aes(day,new_deaths, color="tomato1"),size=1) +
  geom_point(aes(day,new_deaths, color="tomato1"),size=3) +
  scale_y_log10() +
  labs(
    title = "New Cases and Deaths by Day Since First Case",
    subtitle = paste("Pearson's Correlation: ",round(cor(covid_overall$new_cases,covid_overall$new_deaths),3)),
    x = "Days Since First Case",
    y = "New Cases/Deaths",
    caption = paste("Logarithmic scale. Data from ", min_date, " to ", max_date)
  ) +
  scale_color_identity(
    name = "Line",
    breaks = c("cyan4", "tomato1"),
    labels = c("Cases", "Deaths"),
    guide = "legend"
  )

#This perception is not quite clear when seeing the information without the logarithmic scale or comparing the growth rates.

ggplot(covid_overall) + 
  geom_line(aes(day,cases, color="cyan4"),size=1) + 
  geom_point(aes(day,cases, color="cyan4"),size=2) + 
  geom_line(aes(day,deaths, color="tomato1"),size=1) +
  geom_point(aes(day,deaths, color="tomato1"),size=2) +
  labs(
    title = "Cases and Deaths by Day Since First Case",
    x = "Days Since First Case",
    y = "Cases/Deaths",
    caption = paste("Data from ", min_date, " to ", max_date)
  ) +
  scale_color_identity(
    name = "Line",
    breaks = c("cyan4", "tomato1"),
    labels = c("Cases", "Deaths"),
    guide = "legend"
  )

# Calculating the lethality of a disease

data_deaths <-
  filter(covid_overall, deaths > 0) %>%
  mutate(
    cumulative_mean = round((deaths/cases)*100,2),
    day = row_number(date)-1
  )

ggplot(data_deaths) + 
  geom_line(aes(day,cumulative_mean, color="tomato1"),size=1) + 
  geom_point(aes(day,cumulative_mean, color="tomato1"),size=3) + 
  labs(
    title = "Cumulative Lethality Rate Mean by Day Since First Death",
    subtitle = paste("Current Lethality Rate: ",filter(data_deaths,day == max(day))$cumulative_mean, "%"),
    x = "Days Since First Death",
    y = "Lethality Rate",
    caption = paste("Data from ", min_date, " to ", max_date)
  ) +
  scale_color_identity(
    name = "Line",
    breaks = "tomato1",
    labels = "Deaths",
    guide = "legend"
  )


# How are the regions' cases increasing?
# Using a chart comparing the number of cases by day since the first case is useful to compare the regions because they started to report cases in different moments, and with this chart, we can bring everyone to the same time frame.

ggplot(covid_regions) +
  geom_line(aes(day,cases, color = region), size = 1) +
  labs(
    title = "Regional Cumulative Cases by Day Since First Case",
    x = "Days Since First Case",
    y = "Cases",
    caption = paste("Data from ", min_date, " to ", max_date)
  )


ggplot(covid_regions) +
  geom_line(aes(day,deaths, color = region), size = 1) +
  labs(
    title = "Regional Cumulative Deaths by Day Since First Case",
    x = "Days Since First Case",
    y = "Cases",
    caption = paste("Data from ", min_date, " to ", max_date)
  )


#Confirmed Cases
# We'll start by seeing only the total number of cases by state.

data <-
  group_by(covid_states,state,region) %>%
  summarise(cases = max(cases), deaths = max(deaths))

ggplot(data) +
  geom_bar(aes(x = reorder(state,-cases), y = cases, fill = region), stat = "identity") +
  geom_text(aes(x = reorder(state,-cases), y = cases, label = cases), hjust = .5, vjust = -.5, size = 4.5) +
  labs(title = "Total Number of Confirmed Cases by State", x = "State", y = "Cases") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10), axis.text.y = element_blank())




# Deaths
# As we've done with the number of cases. We'll start plotting the total number of deaths by state.

ggplot(data) +
  geom_bar(aes(x = reorder(state,-deaths), y = deaths, fill = region), stat = "identity") +
  geom_text(aes(x = reorder(state,-deaths), y = deaths, label = deaths), hjust = .5, vjust = -.5) +
  labs(title = "Total Number of Deaths by State", x = "State", y = "Deaths") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12), axis.text.y = element_blank())


# Modeling
# To create the model, I've chosen the polynomial regression. This special type of linear regression will be useful because, as we saw earlier, the accumulated number of cases by day since the first case looks like an increasing function with a certain degree of smoothness. Hence, it's going to be pretty easy to fit a polynomial curve.

##### Training The Model\
# We're going to use the approach described earlier to train the model and choose the best polynomial degree. Below we can see an ordered table (according to our best degree criteria) showing the train and test RMSE for each degree

set.seed(1995)

train_test_split <- initial_time_split(data = covid_overall, prop = 0.8)
covid_train <- training(train_test_split)
covid_test <- testing(train_test_split)
trade_off <- tibble(degree = double(), train_rmse = double(), test_rmse = double())

for(d in 1:10){
  poly_model <- lm(cases ~ poly(day,degree = d,raw = TRUE), covid_train)
  covid_train["fitted"] <- predict(poly_model, covid_train)
  covid_test["predicted"] <- predict(poly_model, covid_test)
  trade_off <- add_row(
    trade_off,
    degree = d, 
    train_rmse = rmse_vec(covid_train$cases,covid_train$fitted), 
    test_rmse = rmse_vec(covid_test$cases,covid_test$predicted)
  )
}

best_degree <- arrange(trade_off,test_rmse,train_rmse)$degree[1]
best_test_rmse <- round(arrange(trade_off,test_rmse,train_rmse)$test_rmse[1],2)
best_train_rmse <- round(arrange(trade_off,test_rmse,train_rmse)$train_rmse[1],2)
poly_model <- lm(cases ~ poly(day,best_degree,raw = TRUE),covid_train)

print(paste("The best degree is: ",best_degree))
print("Ordered Trade off Degree Table - Train vs Test RMSE")
arrange(trade_off,test_rmse,train_rmse,degree)



ggplot(trade_off,aes(x = degree)) +
  geom_vline(xintercept = best_degree, linetype="dashed", color="gray60") +
  geom_point(aes(y = train_rmse), color = "tomato1", size = 4) +
  geom_point(aes(y = test_rmse), color = "cyan4", size = 4) +
  geom_line(aes(y = train_rmse, color = "tomato1"), size = 1.5, alpha = .5) +
  geom_line(aes(y = test_rmse, color = "cyan4"), size = 1.5, alpha = .5) +
  annotate(
    "text",
    x = best_degree - .5,
    y = max(c(trade_off$train_rmse,trade_off$test_rmse)),
    label = "Best RMSE",
    size = 5,
    color = "gray40"
  ) +
  labs(
    title = "Train and Test RMSE Trade Off by Polynomial Degree",
    x = "Polynomial Degree",
    y = "RMSE",
    caption = paste(
      "Best Degree: ",
      best_degree,
      ". Best Train RMSE: ", 
      best_train_rmse,
      ". Best Test RMSE: ",
      best_test_rmse
    )
  ) +
  scale_color_identity(
    name = "Line",
    breaks = c("cyan4", "tomato1","gray60"),
    labels = c("Test", "Train", "Best"),
    guide = "legend"
  ) +
  theme(plot.caption = element_text(hjust = .5))



# Test Data Fitted Model

covid_test["pred"] = predict(poly_model,covid_test)

ggplot(covid_test, aes(x = day)) +
  geom_point(aes(y = cases,color = "cyan4"), size = 4) +
  geom_line(aes(y = pred, color = "tomato1"), size = 1.5, alpha = .8) +
  labs(
    title = "Test Data Fitted Model",
    x = "Days Since First Case",
    y = "Cases",
    caption = paste("Polynomial degree: ", best_degree,"."," RMSE: ",best_test_rmse)
  ) +
  scale_color_identity(
    name = "Legend",
    breaks = c("cyan4", "tomato1"),
    labels = c("Real", "Fitted"),
    guide = "legend"
  )

### Applying The Model
#After selecting the best degree and training the model. We'll train the model with the whole dataset using the best degree chosen and use it to predict the next 3 days.

poly_model <- lm(cases ~ poly(day,best_degree,raw = TRUE),covid_overall)
max_date <- max(covid_overall$date)

new_data <- tibble(
  date = seq(as.Date(max_date+1), by="day", length = 3),
  day = seq(max(covid_overall$day)+1,max(covid_overall$day)+3,by=1),
  cases = c(0)
)

covid_final <- select(covid_overall,date,day,cases)
covid_final["predicted"] <- predict(poly_model,covid_final)
new_data["predicted"] <- predict(poly_model,new_data)

rmse <- round(rmse_vec(covid_final$cases,covid_final$predicted),3)
r2 <- round(rsq_vec(covid_final$cases,covid_final$predicted)*100,3)
mae <- round(mae_vec(covid_final$cases,covid_final$predicted),3)


ggplot() +
  geom_point(data = covid_final, aes(x = day, y = cases, color="cyan4"), size = 3) +
  geom_point(data = new_data, aes(x = day, y = predicted, color="tomato1"), size = 3) +
  geom_line(data = covid_final, aes(x = day, y = predicted), size = 1.5, alpha = .5, color="tomato1") +
  labs(
    title = "Polynomial Model - Prediction for the next 3 days",
    subtitle = paste("RMSE: ",rmse," | R2: ",r2, " | MAE: ",mae),
    x = "Days Since First Case",
    y = "Cases",
    caption = paste("Data until: ", max_date,".")
  ) +
  scale_color_identity(
    name = "Legend",
    breaks = c("cyan4", "tomato1"),
    labels = c("Real", "Predicted"),
    guide = "legend"
  )


# Finally, below we can see the table of the fitted model in the training data, along with the table of the next three days' prediction.
print("Fitted Model, last 30 days")
select(covid_final,date,day,cases,predicted) %>% mutate(difference = cases - predicted) %>% tail(30)
print("Next 3 Days Prediction")
new_data
