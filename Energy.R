library(tidyverse)
library(tidymodels)
library(ggridges)
library(naniar)
#install.packages("geofacet")
library(geofacet)
library(grid)
library(png)
library(dplyr)
#install.packages("tibble")
library(ggrepel)
library(moderndive)
library(tidytext)
energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')
country_totals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')


miss_var_summary(energy_types)

energy_df <- energy_types%>%
  drop_na()%>%
  mutate(type=case_when(type=="Conventional thermal"~"Non-Renewable",
            type=="Nuclear"~"Nuclear",
            TRUE~"Renewable Energy"))%>%
 pivot_longer(`2016`:`2018`,names_to = "year",values_to = "value")%>%
  mutate(value2 =scale(value))%>%
  ggplot(aes(value,type))+
  geom_density_ridges()+
  facet_wrap(~country_name,scale="free_x")
 

energy_plot%>%
  ggplot(aes(log(mean_val),fct_reorder(country_name,log(mean_val)),fill=type))+
  geom_col(alpha=0.5)+
  facet_wrap(~type)



 energy_raw <- energy_types %>%
  mutate(country_name = case_when(country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Bosnia & Herzegovina" ~ "Bosnia & H.",
                                  country == "UK" ~ "UK",
                                  country == "EL" ~ "Greece",
                                  TRUE ~ country_name)) %>%
  filter(country_name %in% europe_countries_grid2$name) %>%
  pivot_longer(cols = c(`2016`,`2017`,`2018`), names_to = "year") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  select(-country)
sessionInfo()

## Work out percentages of the total and tidy the data
energy_plot <- left_join(energy_raw %>% filter(level == "Level 1") %>% select(-level) %>% janitor::remove_empty("cols"),
                         energy_raw %>% filter(level == "Level 2") %>% select(-level) %>% janitor::remove_empty("cols")) %>% 
  janitor::clean_names() %>%
  rowwise() %>%
  mutate(tot = sum(conventional_thermal, nuclear, hydro, wind, solar, geothermal, other, pumped_hydro_power),
         across(where(is.numeric), ~ 100 * . / tot)) %>%
  select(-tot) %>%
  ungroup() %>%
  pivot_longer(names_to = "energy", values_to = "value", 
               c(conventional_thermal, nuclear, hydro, wind, solar, geothermal, other, pumped_hydro_power)) %>%
  rbind(expand.grid(country_name = setdiff(europe_countries_grid2$name, energy_raw$country_name),
                    year = 2016:2018, energy = "No Data Available", value = 100)) %>%
  mutate(energy = case_when(energy == "conventional_thermal" ~ "Conventional Thermal",
                            energy == "nuclear" ~ "Nuclear",
                            energy == "No Data Available" ~ energy,
                            TRUE ~ "Renewable"),
         energy = factor(energy, 
                         levels = c("Conventional Thermal", "Nuclear", 
                                    "Renewable", "No Data Available"))) %>%
  group_by(energy, year, country_name) %>%
  summarise(value = sum(value)) %>% mutate(country_name = str_trunc(country_name, 11))

## Plot!



ggplot(energy_plot) +
  
  # The actual proper plotting stuff here isn't actually that complicated!
  aes(x = year, y = value, fill = energy) +
  geom_col(color = "#30332E") +
  coord_flip() +
  scale_y_reverse() +
  
  # facet_geo from the "geofacet" package - I truncate the names here just to be sure
  facet_geo( ~ country_name,
             grid = europe_countries_grid2 %>% mutate(name = str_trunc(name, 11))) +
  
  # colours and themes
  scale_fill_manual(values = c("#EF476F", "#FFD166", "#06D6A0", "#4B5446")) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#30332E"),
    text = element_text(color = "white"),
    strip.text = element_text(color = "white"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "white"),
    plot.margin = unit(c(1, 1, 1.5, 1.2), "cm"),
    legend.position = c(0.075, .125),
    legend.title = element_text(size = 15),
    plot.title = element_text(size = 40, face = "bold"),
    plot.subtitle = ggtext::element_markdown(size = 13),
    plot.caption = ggtext::element_markdown(color = "#5E7054", size = 10)
  ) +
  labs(
    x = "",
    y = "",
    fill = "Type of Energy",
    title = "EUROPEAN ENERGY\nGENERATION",
    subtitle = "Each bar represents the <b>total energy generation</b> for each country per year.<br>The colours represent the proportion of energy generated <b>a)</b> using <b style='color:#EF476F'>conventional<br> thermal power plants</b>, which is to say those that use coal, oil or natural gas,<br><b>b)</b> using <b style='color:#FFD166'>nuclear power stations</b>, and <b>c)</b> using other <b style='color:#06D6A0'>renewable sources</b>.<br><br>",
    caption = "Data from <b>'Electricity generation statistics - First Results'</b> (ec.europa.eu/eurostat/statistics-explained) <br> Visualisation by <b>Simbu</b><br>"
  )


#Energy Tidy
energy_types<-energy_types%>%
  pivot_longer(cols=starts_with("2"),names_to = "year",values_to = "gigawatt_hours")%>%
  mutate(year=as.integer(year))%>%
  replace_na(list(country_name="United Kingdom"))


###Energy Totals
energy_total <- energy_types%>%
  group_by(year,type)%>%
  summarize(total_power=sum(value))%>%
  ungroup()%>%
  mutate(type=fct_reorder(type,total_power))

energy_total%>%
  ggplot(aes(year,total_power,fill=type))+
  geom_col()+
  scale_y_continuous(labels = comma)+
  labs(x="Year",
       y="Total Power (Gigawatt Hours)",
       fill="Type")

##Geom Bar
energy_total%>%
  filter(year==2018)%>%
  ggplot(aes(total_power,type))+
  geom_col()

energy_types%>%
  count(type,sort=TRUE)

library(tidytext)
energy_data_prepared <- energy_types %>%
  filter(level=="Level 1",year==2018,gigawatt_hours>0)%>%
  group_by(type)%>%
  mutate(country_name=fct_lump(country_name,10,w=gigawatt_hours),
                               country=ifelse(country=="UK","GB",country),
                               country=ifelse(country=="EL","GR",country),
                               country=as.character(fct_lump(country,10,w=gigawatt_hours)))%>%
  mutate(country_name=reorder_within(country_name,gigawatt_hours,type,fun=sum),
         type=fct_reorder(type,-gigawatt_hours,sum))

energy_data_prepared %>%
  filter(country!="Other")%>%
  ggplot(aes(gigawatt_hours,country_name))+
  geom_col()+
  scale_y_reordered()+
  facet_wrap(~type,scales="free")
  

library(ggflags)
#devtools::install_github("rensa/ggflags")

energy_data_prepared %>%
  filter(country!="Other")%>%
  filter(level=="Level 1",year==2018,gigawatt_hours>0,type!="Others")%>%
  mutate(country=str_to_lower(country))%>%
  ggplot(aes(gigawatt_hours,country_name))+
  geom_col(width=0.1)+
  geom_flag(aes(country=country))+
  scale_y_reordered()+
  facet_wrap(~type,scales="free")+
  scale_country(guide=FALSE)+
  scale_x_continuous(label=comma)+
  theme_minimal()+
 theme(panel.grid.minor.x = element_blank(),
     panel.grid.minor.y = element_blank())+
  labs(x="Total Power Production in Europe (Gigawatt Hours)",y="")


ggplot(tactile_prop_red, aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 50 balls that were red", 
       title = "Distribution of 33 proportions red") 

library(tidytext)
library(SnowballC)
library(tm)
library(widyr)

#animalfarm <- read_csv("animal_farm.csv")

glimpse(animalfarm)

stemmed_animal_farm <- animalfarm %>%
  unnest_tokens(word,text_column,token = "words")%>%
  anti_join(stop_words)%>%
  #mutate(word=wordStem(word))%>%
  count(chapter,word,sort = TRUE)%>%
  bind_tf_idf(chapter,word,n)%>%
  pairwise_similarity(chapter,word,n)%>%
  arrange(desc(similarity))


animalfarm %>%
  unnest_tokens(word, text_column) %>%
  anti_join(stop_words) %>%
  count(chapter, word) %>%
  bind_tf_idf(chapter, word, n)%>%
pairwise_similarity(chapter, word, n) %>%
  arrange(desc(similarity))

sent <- animalfarm %>%
  unnest_tokens(sentences, text_column,token="sentences") 


sent$boxer <- grepl("boxer",sent$sentences)
sent$napolean <- grepl("napoleon",sent$sentences)

sent$sentences <- gsub("boxer","animal X",sent$sentences)
sent$sentences <- gsub("napoleon","animal X",sent$sentences)

animal_sent <- sent[sent$boxer+sent$napolean==1,]

animal_sent$Name <- as.factor(ifelse(animal_sent$boxer,"boxer","napoleon"))

animal_sent <- rbind(animal_sent[animal_sent$Name=="boxer",][c(1:75),],
                     animal_sent[animal_sent$Name=="napoleon",][c(1:75),]
  
)

rm()
animal_tokens <- animal_sent%>%
  unnest_tokens(word,token="words",input=sentences)%>%
  anti_join(stop_words)%>%
  mutate(word=wordStem(word))

animal_matrix <- animal_tokens%>%count(sentence_id,word)%>%
  cast_dtm(sentence_id,word,value=n,weighting = tm::weightTfIdf)
  

animal_sent$sentence_id <- c(1:dim(animal_sent)[1])

animal_split <- initial_split(animal_sent)

animal_train<-training(animal_split)
animal_test<-testing(animal_split)

animal_mod <- rand_forest() %>%
  set_engine("randomForest")%>%
  set_mode("classification")

animal_Res <- animal_mod%>%
  fit(Name~.,data=animal_train)


animal_res_rf <- animal_test %>%
  bind_cols(predict(animal_Res, animal_test) %>%
              rename(.pred_rf = .pred_class))

animal_res_rf%>%
conf_mat(truth = Name, estimate = .pred_rf)

stemmed_animal_farm%>%
filter(chapter=="Chapter 1")%>%
  count(tf,sort=TRUE)
library(widyr)

russian_tweets <- read_csv("russian.csv")

russian_tweets%>%
  unnest_tokens(word,content,"words")%>%
 anti_join(custom_stop)%>%
  count(X1,word,sort=TRUE)%>%
  bind_tf_idf(word,X1, n)%>5%
 


tweet_corpus <- VCorpus(VectorSource(russian_tweets$content))

# Attach following and followers
meta(tweet_corpus, 'following') <- russian_tweets$following
meta(tweet_corpus, 'followers') <- russian_tweets$followers

# Review the meta data
head(meta(tweet_corpus))

custom_stop <- add_row(stop_words,word=c("t.co","https","http"),lexicon="custom")
