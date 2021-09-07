library(tidyverse)
library(showtext)
library(directlabels)
library(ggalt)

font_add_google('Dosis', 'dosis')
showtext_auto()


#Import data
trend_data_sex <- read_csv("data/trend_data_sex.csv")
trend_data_age <- read_csv("data/trend_data_age.csv")
trend_data_ei <- read_csv("data/trend_data_ei.csv")
trend_data_lti <- read_csv("data/trend_data_lti.csv")


#Sex breakdown wrangling
trend_data <- 
  trend_data_sex %>% 
  select(Year, Categories, Sex, Percent, LowerCI, UpperCI)


trend_All <- trend_data %>% filter(Sex == 'All')

trend_female <- trend_data %>% filter(Sex == 'Female')

trend_male <- trend_data %>% filter(Sex == 'Male')


trend_All_1 <- 
  trend_All %>% select(Categories, Year, Percent, LowerCI, UpperCI) %>%
  group_by(Categories) %>% 
  pivot_wider(names_from = Year, values_from = c(Percent, LowerCI, UpperCI))


trend_female_1 <- 
  trend_female %>% select(Categories, Year, Percent, LowerCI, UpperCI) %>%
  group_by(Categories) %>% 
  pivot_wider(names_from = Year, values_from = c(Percent, LowerCI, UpperCI))

trend_male_1 <- 
  trend_male %>% select(Categories, Year, Percent, LowerCI, UpperCI) %>%
  group_by(Categories) %>% 
  pivot_wider(names_from = Year, values_from = c(Percent, LowerCI, UpperCI))
  

df1 <- trend_All_1 %>% 
  mutate(`2012-2013` = Percent_2013-Percent_2012,
         `2013-2014` = Percent_2014-Percent_2013,
         `2014-2015` = Percent_2015-Percent_2014,
         `2015-2016` = Percent_2016-Percent_2015,
         `2016-2017` = Percent_2017-Percent_2016,
         `2017-2018` = Percent_2018-Percent_2017,
         `2018-2019` = Percent_2019-Percent_2018,
         eightyrdiff = Percent_2019-Percent_2012,
         LCI = LowerCI_2019-LowerCI_2012,
         UCI = UpperCI_2019-UpperCI_2012
         ) %>% select(Categories, `2012-2013`, `2013-2014`, `2014-2015`, `2015-2016`,
                      `2016-2017`, `2017-2018`, `2018-2019`, eightyrdiff, LCI, UCI)

F_df1 <- trend_female_1 %>% 
  mutate(`2012-2013` = Percent_2013-Percent_2012,
         `2013-2014` = Percent_2014-Percent_2013,
         `2014-2015` = Percent_2015-Percent_2014,
         `2015-2016` = Percent_2016-Percent_2015,
         `2016-2017` = Percent_2017-Percent_2016,
         `2017-2018` = Percent_2018-Percent_2017,
         `2018-2019` = Percent_2019-Percent_2018,
         female = Percent_2019-Percent_2012,
         LCI = LowerCI_2019-LowerCI_2012,
         UCI = UpperCI_2019-UpperCI_2012
  ) %>% select(Categories, female, LCI, UCI)

M_df1 <- trend_male_1 %>% 
  mutate(`2012-2013` = Percent_2013-Percent_2012,
         `2013-2014` = Percent_2014-Percent_2013,
         `2014-2015` = Percent_2015-Percent_2014,
         `2015-2016` = Percent_2016-Percent_2015,
         `2016-2017` = Percent_2017-Percent_2016,
         `2017-2018` = Percent_2018-Percent_2017,
         `2018-2019` = Percent_2019-Percent_2018,
         male = Percent_2019-Percent_2012,
         LCI = LowerCI_2019-LowerCI_2012,
         UCI = UpperCI_2019-UpperCI_2012
  ) %>% select(Categories, male, LCI, UCI)



Plot <- merge(F_df1, M_df1, by= 'Categories')

plot1 <- Plot %>% gather(female, male, key='sex', value= 'value')


#Wesanderson palette
IsleofDogs1 = c("#9986A5", "#79402E", "#CCBA72", "#0F0D0E", "#D9D0D3", "#8D8680")


#Plot theme function
theme_SHeS <- function() {
  theme_classic() +
    theme(panel.background = element_rect(fill = "#CCBA72"),
          plot.background = element_rect(fill = "#CCBA72"),
          text = element_text(color = 'black', family = 'dosis'),
          plot.title = element_text(size = 20, hjust = 1),
          axis.title = element_text(size = 16),
          axis.line = element_line(color = 'black', size = 1.7),
          axis.text = element_text(size=12, color= 'black', family= 'dosis'),
          axis.ticks = element_line(color = 'black'),
          legend.position = 'bottom',
          legend.background = element_rect(fill="transparent"),
          legend.text = element_text(size = 12),
          plot.caption = element_text(size = 12)
    ) 
}



#8 year bar change bar chart
ggplot(plot1, aes(x= Categories, y= value, fill= sex)) +
  geom_bar(stat = 'identity', position= 'dodge') +
  scale_x_discrete(limits=c('Very low activity', 'Low activity', 
                            'Some activity', 'Meets recommendations')) +
  scale_fill_manual(values = c("#9986A5", "#79402E")) +
  labs(x= NULL,
       y= '2012 - 2019 difference',
       fill = NULL,
       title= '8 year change in physical activity levels',
       caption = "Scottish Health Survey | @nicci_potts"
  )  + 
  theme_SHeS() +
  scale_y_continuous(labels = function(x) paste0(x, "%"))



#Age breakdown wrangling
trend_age_df1 <- 
  trend_data_age %>% 
  select(Categories, Year, Age, Percent, LowerCI, UpperCI)

trend_age_mr <-
  trend_age_df1 %>%
  group_by(Age) %>%
  filter(Categories == 'Meets recommendations',
         Age != 'All',
         Year %in% c('2012', '2019')) %>%
  select(Age, Year, Percent) %>%
  spread(Year, Percent)

trend_age_75 <- trend_age_df1 %>% 
  filter(Age == '75+')


#Age change wrangling
twelvediff <-  trend_age_mr %>% 
  pivot_wider(names_from = Age, values_from = c(`2012`, `2019`)) %>%
  mutate(twelve_25 = `2012_25-34`- `2012_16-24`,
         twelve_35 = `2012_35-44`- `2012_25-34`,
         twelve_45 = `2012_45-54`- `2012_35-44`,
         twelve_55 = `2012_55-64`- `2012_45-54`,
         twelve_65 = `2012_65-74`- `2012_55-64`,
         twelve_75 = `2012_75+`- `2012_65-74`) %>%
  select(twelve_25, twelve_35, twelve_45, twelve_55, twelve_65, twelve_75) %>%
  pivot_longer(
    cols = starts_with("twelve"),
    names_to = "Age",
    names_prefix = "twelve_",
    values_to = "2012",
    values_drop_na = TRUE
  )


nineteendiff <-  trend_age_mr %>% 
  pivot_wider(names_from = Age, values_from = c(`2012`, `2019`)) %>%
  mutate(nineteen_25 = `2019_25-34`- `2019_16-24`,
         nineteen_35 = `2019_35-44`- `2019_25-34`,
         nineteen_45 = `2019_45-54`- `2019_35-44`,
         nineteen_55 = `2019_55-64`- `2019_45-54`,
         nineteen_65 = `2019_65-74`- `2019_55-64`,
         nineteen_75 = `2019_75+`- `2019_65-74`) %>%
  select(nineteen_25, nineteen_35, nineteen_45, nineteen_55, nineteen_65, nineteen_75) %>%
  pivot_longer(
    cols = starts_with("nineteen"),
    names_to = "Age",
    names_prefix = "nineteen_",
    values_to = "2019",
    values_drop_na = TRUE
  )


AgeDiff <- merge(twelvediff, nineteendiff, by= 'Age')


AgeDiff1 <- AgeDiff %>% 
  gather(`2012`, `2019`, key='year', value= 'value')


#change across age range plot
ggplot(AgeDiff1, aes(
  x = Age,
  y = value,
  group = year,
  color = year
)) +
  geom_point(size = 4) +
  geom_line(size = 1.7) +
  geom_dl(aes(label = year), method = list(dl.trans(x = x + 0.5), "last.points", cex = 0.8)) +
  geom_dl(aes(label = year), method = list(dl.trans(x = x - 0.5), "first.points", cex = 0.8)) +
  scale_color_manual(name = "", values = c("#9986A5", "#79402E")) +
  labs(
    x = 'Age',
    y = 'Change in those meeting recommendations (%)',
    color = NULL,
    title = 'Change in physical activity across age groups',
    caption = "Scottish Health Survey | @nicci_potts"
  ) +
  theme_SHeS() +
  theme(legend.position = 'none') +
  scale_y_continuous(labels = function(x) paste0(x, "%"))


#75_ physical activity levels
ggplot(trend_age_75,
       aes(
         x = factor(Year),
         y = Percent,
         group = Categories,
         color= factor(Categories,
                      levels = c('Very low activity', 'Low activity', 
                                 'Some activity', 'Meets recommendations'))
       )) +
  geom_smooth(aes(ymin = LowerCI, ymax = UpperCI),
              linetype = 0,
              fill = 'white') +
  geom_line(size = 1.7) +
  geom_point(size = 4) +
  scale_color_manual(values =
                       c("#9986A5", "#79402E", "#0F0D0E",
                         "#8D8680")) +
  labs(
    x = NULL,
    y = 'Proportion of age group',
    color = NULL,
    title = '75+ year old physical activity levels',
    caption = "Scottish Health Survey | @nicci_potts"
  ) +
  theme_SHeS() +
  scale_y_continuous(labels = function(x) paste0(x, "%"))



#Equivalised income data wrangling
trend_ei_df1 <- trend_data_ei %>% 
  select(Year, Categories, EI, Percent, LowerCI, UpperCI)


trend_ei_mr <- trend_ei_df1 %>% 
  filter(Categories == 'Meets recommendations')

trend_ei_19 <- trend_ei_df1 %>% 
  filter(Year == '2019')

#EI plot
ggplot(trend_ei_19,
       aes(
         x = Percent,
         y = EI,
         group = Categories,
         fill = factor(
           Categories,
           levels = c(
             'Very low activity',
             'Low activity',
             'Some activity',
             'Meets recommendations'
           )
         )
       )) +
  geom_bar(position = "stack", stat = "identity") +
  scale_y_discrete(
    limits = c(
      '5th-Bottom quintile',
      '4th quintile',
      '3rd quintile',
      '2nd quintile',
      '1st-Top quintile'
    )
  ) +
  
  scale_fill_manual(values =
                      c("#9986A5", "#79402E", "#D9D0D3", "#0F0D0E")) +
  labs(
    fill = NULL,
    y = NULL,
    title = '2019 physical activity levels',
    x = 'Proportion of population (%)',
    caption = "Scottish Health Survey | @nicci_potts"
  ) +
  theme_SHeS() +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  




#limiting health condition data wrangling
trend_lti_df1 <- 
  trend_data_lti %>% 
  filter(Categories == 'Meets recommendations') 


trend_lti_df2 <- trend_lti_df1 %>%
  select(Year, LTI, Percent) %>%
  spread(Year, Percent) %>%
  select(LTI, `2012`, `2019`)

#LTI plot
df2 = tidyr::gather(trend_lti_df2, group, value,-LTI)



ggplot(trend_lti_df2, aes(y = LTI)) +
  geom_point(data = df2, aes(x = value, color = group), size = 3) +
  scale_color_manual(name = "", values = c("#9986A5", "#79402E")) +
  geom_segment(aes(
    x = `2012`,
    xend = `2019`,
    y = LTI,
    yend = LTI
  ),
  color = "black",
  size = 2) +
  geom_dumbbell(
    aes(
      x = `2012`,
      xend = `2019`,
      y = LTI,
      group = LTI
    ),
    color = "black",
    size_x = 6,
    size_xend = 6,
    colour_x = "#9986A5",
    colour_xend = "#79402E"
  ) +
  
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_discrete(limits=c('Limiting long-term illness', 'Non-limiting long-term illness', 
                            'No long-term illness')) +
  
  labs(
    y = NULL,
    x = 'Proportion of population',
    fill = "",
    title = 'Meets physical activity recommendations',
    caption = "Scottish Health Survey | @nicci_potts"
  ) +
  theme_SHeS() 