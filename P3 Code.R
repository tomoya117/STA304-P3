#Set Up
library(tidyverse)
library(knitr)
library(dplyr)

#Overall Life Satisfaction Table
satisfaction <- read_csv(file = "Graph 1_ Overall life satisfaction by Age, 2014 - 2020.csv")
satisfaction <- data.frame(satisfaction)
satisfaction <- subset(satisfaction, select = c(1, 3, 4))
satisfaction <- satisfaction[c(2, 3, 4, 5, 6), ]
colnames(satisfaction) <- c('Age Group', '2019', '2020')

g1 <- 
  satisfaction |>
  gather(Year, Score, 2:3)

#Face to Face Contact Table
contact <- read_csv(file = "Graph 4_ Face to face contact with family or friends living outside their household by Age, 2019 and 2020.csv")
contact <- data.frame(contact)
contact <- subset(contact, select = c(1, 2, 3))
contact <- contact[c(2, 3, 4, 5, 6), ]
colnames(contact) <- c('Age Group', '2019', '2020')

g2 <- 
  contact |>
  gather(Year, Percentage, 2:3)

#Visualization of Tables 1 and 2
satisfaction |>
  select(`Age group`, `2019`, `2020`) |>
  slice(1:5) |>
  kable(
    caption = "Overall Life Satisfaction by Age", 
    col.names = c("Age Group", "2019 Score", "2020 Score"),
    digits = 1,
    booktabs = TRUE, 
    linesep = ""
  )
contact |>
  select(`Age group`, `2019`, `2020`) |>
  slice(1:5) |>
  kable(
    caption = "Face to Face Contact with Family or Friends Outside of Their Household", 
    col.names = c("Age Group", "2019 (%)", "2020 (%)"),
    digits = 1,
    booktabs = TRUE, 
    linesep = ""
  )

#Visualization of Graphs 1 and 2
g1 |> 
  ggplot(aes(x = `Age group`, y = Score, group = Year, fill = Year)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() + 
  labs(x = "Age Group", y = "Score", title = "Figure 1: Overall Life Satisfaction by Age", caption = "General Social Survey: Summary Results, Australia")


g2 |> 
  ggplot(aes(x = `Age group`, y = Percentage, group = Year, fill = Year)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() + 
  labs(x = "Age Group", y = "Percentage (%)", title = "Figure 2: Face to Face Contact with Friends or Family Outside of Household", caption = "General Social Survey: Summary Results, Australia")

#Combined Table of the two
change <- merge(satisfaction, contact, by = "Age Group")

change <- 
  change |>
  select(`Age Group`, `2019.x`, `2020.x`, `2019.y`, `2020.y`) |>
  mutate(s_decrease = (as.numeric(`2019.x`) - as.numeric(`2020.x`))/as.numeric(`2019.x`), c_decrease = (as.numeric(`2019.y`) - as.numeric(`2020.y`))/as.numeric(`2019.y`))
colnames(change) <- c("Age Group", "s_2019", "s_2020", "c_2019", "c_2020", "s_decrease", "c_descrease")

g3 <- 
  change |>
  gather(decrease, Percentage, 6:7)

#Visualization of the table 3
change <- 
  change |> 
  select(`Age Group`, s_decrease, c_decrease) |>
  mutate(s_decrease = as.numeric(s_decrease)*100, c_decrease = as.numeric(c_decrease) * 100)

change |>
  select(`Age Group`, "s_decrease", "c_decrease") |>
  slice(1:5) |>
  kable(
    caption = "Percentage Decrease in Satisfaction Score and Face to Face Contact between 2019 and 2020", 
    col.names = c("Age Group", "Satifaction (%)", "Contact (%)"),
    digits = 1,
    booktabs = TRUE, 
    linesep = ""
  )
#Visualization of the graph 3
g3 |> 
  ggplot(aes(x = `Age Group`, y = Percentage, group = `decrease`, fill = `decrease`)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() + 
  labs(x = "Age Group", y = "Decreases in scores between 2019 and 2020", title = "Figure 3: Relationship between Face to Face Contact and Life Satisfaction", caption = "General Social Survey: Summary Results, Australia")
