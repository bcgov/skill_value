#libraries---------------
library(tidyverse)
library(here)
library(janitor)
library(broom)
#functions---------------
av_wrapper <- function(term){
  car::avPlot(mod, variable=term)
  recordPlot()
}
#read in the data--------------------
income <- readxl::read_excel(here("data",
                                  "Employment income by NOC5_2021 census.xlsx"),
                             skip = 3,
                             na = "x")%>%
  select(noc="...1", income = `Median employment income ($)`)%>%
  mutate(log_med_income=log(income), .keep="unused")%>%
  separate(noc, into=c("noc2021", "noc2021_title"), sep = 6)%>%
  mutate(noc2021=trimws(noc2021))

skills_raw <- readxl::read_excel(here::here("data","skills_original.xlsx"),
                         col_types = c("text","text","text","numeric"))%>%
  mutate(noc2021=str_pad(noc2021, width=5, side="left", pad="0"))%>%
  pivot_wider(id_cols=contains("noc"), names_from = contains("name"), values_from = contains("value"))%>%
  clean_names()%>%
  mutate(across(where(is.numeric), log))

#join the income and skill data-----------------------
tbbl <- inner_join(income, skills_raw)%>%
  unite("noc", noc2021, noc2021_title, sep=": ")%>%
  column_to_rownames("noc")
#fit a linear model to log transformed data-------------------------
mod <- lm(log_med_income ~ ., data=tbbl)
sorted_estimates <- mod%>%
  tidy()%>%
  filter(term!="(Intercept)")%>%
  arrange(desc(estimate))
#plot all the elasticities---------------------------
elasticity_plt <- sorted_estimates%>%
  mutate(term=str_to_title(str_replace_all(term,"_"," ")),
         estimate=estimate/100
         )%>%
ggplot(aes(estimate, fct_reorder(term, estimate)))+
  geom_col()+
  scale_x_continuous(labels = scales::percent)+
  labs(x="Income Elasticity with respect to skill",
       y="",
       title="Relationship between Skills and Income across Occupations",
       subtitle="holding all other skills constant")+
  theme_minimal()

sorted_estimates <- sorted_estimates%>%
  mutate(av_plot=map(term, av_wrapper))

par(mfrow=c(2,2))
plot(mod, pch=".")
diag_plot <- recordPlot()
