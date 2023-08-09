#libraries---------------
library(tidyverse)
library(here)
library(janitor)
library(broom)
library(readxl)
#functions---------------
av_wrapper <- function(term){
  car::avPlot(mod, variable=term)
  recordPlot()
}

# the program------------------------
mapping <- read_excel(here("data","onet2019_soc2018_noc2016_noc2021_crosswalk.xlsx"))%>%
  mutate(noc2021=str_pad(noc2021, "left", pad="0", width=5))%>%
  unite(noc, noc2021, noc2021_title, sep=": ")%>%
  select(noc, o_net_soc_code = onetsoc2019)%>%
  distinct()

skills_raw <- read_excel(here("data", "Skills.xlsx"))%>%
    clean_names()%>%
    select(o_net_soc_code, element_name, scale_name, data_value)%>%
    pivot_wider(names_from = scale_name, values_from = data_value)%>%
    mutate(score=log(sqrt(Importance*Level)+1))%>%
    select(-Importance, -Level)%>%
    inner_join(mapping)%>%
    select(-o_net_soc_code)%>%
    select(noc, everything())%>%
    group_by(noc, element_name)%>%
    summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))%>% #mapping from SOC to NOC is not one to one: mean give one value per NOC
    pivot_wider(names_from = element_name, values_from = score)%>%
    separate(noc, into=c("noc2021", "noc2021_title"), sep=": ")

income <- readxl::read_excel(here("data",
                                  "Employment income by NOC5_2021 census.xlsx"),
                             skip = 3,
                             na = "x")%>%
  select(noc="...1", income = `Median employment income ($)`)%>%
  mutate(Income = log(income), .keep="unused")%>%
  separate(noc, into=c("noc2021", "noc2021_title"), sep = 6)%>%
  mutate(noc2021=trimws(noc2021))


#join the income and skill data-----------------------
tbbl <- inner_join(income, skills_raw)%>%
  unite("noc", noc2021, noc2021_title, sep=": ")%>%
  column_to_rownames("noc")
#fit a linear model to log transformed data-------------------------
mod <- lm(Income ~ ., data=tbbl)
sorted_estimates <- mod%>%
  tidy()%>%
  filter(term!="(Intercept)")%>%
  arrange(desc(estimate))
#plot all the elasticities---------------------------
(elasticity_plt <- sorted_estimates%>%
   mutate(term=str_replace_all(term,"`",""))%>%
  ggplot(aes(estimate, fct_reorder(term, estimate)))+
    geom_col()+
    labs(x="Income Elasticity with respect to skill",
         y="",
         title="Relationship between Skills and Income across Occupations",
         subtitle="holding all other skills constant")+
    theme_minimal())

sorted_estimates <- sorted_estimates%>%
  mutate(av_plot=map(term, av_wrapper))

par(mfrow=c(2,2))
plot(mod, pch=".")
diag_plot <- recordPlot()
