#Set Up
library(tidyverse)
library(knitr)
library(dplyr)

#Overall Life Satisfaction Table - Data Section
satisfaction <- read_csv(file = "Graph 1_ Overall life satisfaction by Age, 2014 - 2020.csv")
satisfaction <- data.frame(satisfaction)
satisfaction <- subset(satisfaction, select = c(1, 3, 4))
satisfaction <- satisfaction[c(2, 3, 4, 5, 6), ]
colnames(satisfaction) <- c('Age Group', '2019', '2020')

g1 <- 
  satisfaction |>
  gather(Year, Score, 2:3)

#Face to Face Contact Table - Data Section
contact <- read_csv(file = "Table 17.1_Proportion-Table 1.csv")
contact <- data.frame(contact)
contact <- subset(contact, select = c(1, 4, 8, 12, 16, 20))
contact <- contact[c(23), ]
colnames(contact) <- c('Description', '2006', '2010', '2014', '2019', '2020')

g2 <- 
  contact |>
  select(Description, `2006`, `2010`, `2014`, `2019`, `2020`) |>
  gather(Year, Percentage, 2:6)

#Visualization of Tables 1 and 2
satisfaction |>
  select(`Age Group`, `2019`, `2020`) |>
  slice(1:5) |>
  kable(
    caption = "Overall Life Satisfaction by Age", 
    col.names = c("Age Group", "2019 Score", "2020 Score"),
    digits = 1,
    booktabs = TRUE, 
    linesep = ""
  )
contact |>
  select(`Age Group`, `2019`, `2020`) |>
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
  ggplot(aes(x = `Age Group`, y = as.numeric(Score), group = Year, fill = Year)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() + 
  labs(x = "Age Group", y = "Score", title = "Figure 1: Overall Life Satisfaction by Age", caption = "General Social Survey: Summary Results, Australia")

g2 |> 
  ggplot(aes(x = Description, y = as.numeric(Percentage), group = Year, fill = Year)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() + 
  labs(x = "Description", y = "Percentage (%)", title = "Figure 2: Face to Face Contact with Friends or Family Outside of Household", caption = "General Social Survey: Summary Results, Australia") + 
  scale_fill_discrete(labels = c("2006", "2010", "2014", "2019", "2020"))

#Combined Table of the two - Result Section
change <- merge(satisfaction, contact, by = "Age Group")

change <- 
  change |>
  select(`Age Group`, `2019.x`, `2020.x`, `2019.y`, `2020.y`) |>
  mutate(s_decrease = (as.numeric(`2019.x`) - as.numeric(`2020.x`))/as.numeric(`2019.x`), c_decrease = (as.numeric(`2019.y`) - as.numeric(`2020.y`))/as.numeric(`2019.y`))
colnames(change) <- c("Age Group", "s_2019", "s_2020", "c_2019", "c_2020", "s_decrease", "c_decrease")

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
  ggplot(aes(x = `Age Group`, y = as.numeric(Percentage), group = `decrease`, fill = `decrease`)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() + 
  labs(x = "Age Group", y = "Decreases in scores between 2019 and 2020", title = "Figure 3: Relationship between Face to Face Contact and Life Satisfaction", caption = "General Social Survey: Summary Results, Australia")

#Demographics Table
demographics <- read_csv(file = "GSS_Table1.csv")
demographics <- data.frame(demographics)
demographics <- subset(demographics, select = c(1, 2, 3, 4, 6, 7, 8))
demographics <- demographics[c(95:99), ]
colnames(demographics) <- c('Age Group', '2019_Male', '2019_Female', '2019_Total', '2020_Male', '2020_Female', '2020_Total')
demographics <- 
  demographics |>
  select(`Age Group`, `2019_Male`, `2019_Female`, `2019_Total`, `2020_Male`, `2020_Female`, `2020_Total`) |>
  mutate(`2019_Total` = gsub(",", "", `2019_Total`)) |>
  mutate(`2019_Male` = gsub(",", "", `2019_Male`)) |>
  mutate(`2019_Female` = gsub(",", "", `2019_Female`)) |>
  mutate(`2020_Total` = gsub(",", "", `2020_Total`)) |>
  mutate(`2020_Male` = gsub(",", "", `2020_Male`)) |>
  mutate(`2020_Female` = gsub(",", "", `2020_Female`)) |>
  mutate(`2019_Other` = as.numeric(`2019_Total`) - as.numeric(`2019_Male`) - as.numeric(`2019_Female`)) |> 
  mutate(`2020_Other` = as.numeric(`2020_Total`) - as.numeric(`2020_Male`) - as.numeric(`2020_Female`))

demographics$`2019_Male` <- as.integer(demographics$`2019_Male`)
demographics$`2019_Female` <- as.integer(demographics$`2019_Female`)
demographics$`2019_Other` <- as.integer(demographics$`2019_Other`)
demographics$`2019_Total` <- as.integer(demographics$`2019_Total`)
demographics$`2020_Male` <- as.integer(demographics$`2020_Male`)
demographics$`2020_Female` <- as.integer(demographics$`2020_Female`)
demographics$`2020_Other` <- as.integer(demographics$`2020_Other`)
demographics$`2020_Total` <- as.integer(demographics$`2020_Total`)

#Visualization of Table 4
demographics |>
  select(`Age Group`, `2019_Male`, `2019_Female`,  `2019_Other`, `2019_Total`, `2020_Male`, `2020_Female`, `2020_Other`, `2020_Total`) |>
  slice(1:5) |>
  kable(
    caption = "Demographics of the Survey Respondents", 
    col.names = c("Age Group", "Male", "Female", "Other", "Total", "Male", "Female", "Other", "Total"),
    digits = 1,
    booktabs = TRUE, 
    linesep = ""
  ) |>  
  add_header_above(c(" " = 1, "2019" = 4, "2020" = 4))

#Visualization of Graphs 4 and 5 (Age and Gender Distribution) - Data Section
age <- 
  demographics |>
  select(`Age Group`, `2019_Total`, `2020_Total`) |>
  gather(Total, Count, 2:3)

age |> 
  ggplot(aes(x = `Age Group`, y = Count, group = Total, fill = Total)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() + 
  labs(x = "Age Group", title = "Figure 4: Survey Respondent's Age", caption = "General Social Survey: Summary Results, Australia") + scale_fill_discrete(labels = c("2019", "2020"))

gender <- 
  demographics |>
  select(`2019_Male`, `2019_Female`, `2019_Other`, `2020_Male`, `2020_Female`, `2020_Other`) |>
  gather(Gender, Count, 1:6)

gender <- 
  gender |>
  select(Gender, Count) |>
  mutate(Year = 
           case_when(Gender == "2019_Male" ~ "2019", 
                     Gender == "2019_Female" ~ "2019",
                     Gender == "2019_Other" ~ "2019",
                     Gender == "2020_Male" ~ "2020", 
                     Gender == "2020_Female" ~ "2020", 
                     Gender == "2020_Other" ~ "2020")) |>
  mutate(Gender = 
           case_when(Gender == "2019_Male" ~ "Male", 
                     Gender == "2019_Female" ~ "Female",
                     Gender == "2019_Other" ~ "Other",
                     Gender == "2020_Male" ~ "Male", 
                     Gender == "2020_Female" ~ "Female",
                     Gender == "2020_Other" ~ "Other"))

gender |> 
  ggplot(aes(x = Gender, y = Count, group = Year, fill = Year)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() + 
  labs(x = "Gender", title = "Figure 5: Survey Respondent's Gender Distribution", caption = "General Social Survey: Summary Results, Australia") + scale_fill_discrete(labels = c("2019", "2020"))

#Finance - Result Section
finance <- read_csv("Table 16.1_proportion-Table 1.csv")
finance <- data.frame(finance)
finance <- subset(finance, select = c(1, 2, 3, 4, 5, 6))
finance <- finance[c(8, 11, 14), ]
colnames(finance) <- c('Description', '2006', '2010', '2014', '2019', '2020')
finance <- 
  finance |>
  select(Description, `2006`, `2010`, `2014`, `2019`, `2020`) |>
  gather(Year, Percentage, 2:6)

#Visualization
finance |> 
  ggplot(aes(x = Description, y = as.numeric(Percentage), group = Year, fill = Year)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() + 
  labs(x = "Description", y = "Percentage (%)", title = "Figure 6: Impact on Households' Financial Status", caption = "General Social Survey: Summary Results, Australia") + 
  scale_fill_discrete(labels = c("2006", "2010", "2014", "2019", "2020")) + 
  scale_x_discrete(guide=guide_axis(n.dodge=3))

#Discrimination
discrimination <- read_csv(file = "Table 17.1_Proportion-Table 1.csv")
discrimination <- data.frame(discrimination)
discrimination <- subset(discrimination, select = c(1, 12, 16, 20))
discrimination <- discrimination[c(31), ]
colnames(discrimination) <- c('Description', '2014', '2019', '2020')
discrimination <- 
  discrimination |>
  select(Description, `2014`, `2019`, `2020`) |>
  gather(Year, Percentage, 2:4)

#Visualization
discrimination |> 
  ggplot(aes(x = Description, y = as.numeric(Percentage), group = Year, fill = Year)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() + 
  labs(x = "Description", y = "Percentage (%)", title = "Figure 8: Discrimination", caption = "General Social Survey: Summary Results, Australia") + scale_fill_discrete(labels = c("2014", "2019", "2020"))

#Trust
trust <- read_csv(file = "Table 17.1_Proportion-Table 1.csv")
trust <- data.frame(trust)
trust <- subset(trust, select = c(1, 4, 8, 12, 16, 20))
trust <- trust[c(35, 36, 37), ]
colnames(trust) <- c('Description', '2006', '2010', '2014', '2019', '2020')
trust <- 
  trust |>
  select(Description, `2006`, `2010`, `2014`, `2019`, `2020`) |>
  gather(Year, Percentage, 2:6)

#Visualization
trust |> 
  ggplot(aes(x = Description, y = as.numeric(Percentage), group = Year, fill = Year)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() + 
  labs(x = "Feels Most People can be Trusted", y = "Percentage (%)", title = "Figure 9: Trust", caption = "General Social Survey: Summary Results, Australia") + scale_fill_discrete(labels = c("2006", "2010", "2014", "2019", "2020")) + scale_x_discrete(guide=guide_axis(n.dodge=2))

#Stressors
stressors <- read_csv(file = "Table 17.1_Proportion-Table 1.csv")
stressors <- data.frame(stressors)
stressors <- subset(stressors, select = c(1, 12, 16, 20))
stressors <- stressors[c(40, 41, 42, 43), ]
colnames(stressors) <- c('Description', '2014', '2019', '2020')
stressors <- 
  stressors |>
  select(Description, `2014`, `2019`, `2020`) |>
  gather(Year, Percentage, 2:4)

#Vizualization
stressors |> 
  ggplot(aes(x = Description, y = as.numeric(Percentage), group = Year, fill = Year)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() + 
  labs(x = "Description", y = "Percentage (%)", title = "Figure 10: Stressors", caption = "General Social Survey: Summary Results, Australia") + scale_fill_discrete(labels = c("2014", "2019", "2020"))+ scale_x_discrete(guide=guide_axis(n.dodge=4))

