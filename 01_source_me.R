#libraries---------------
library(tidyverse)
library(here)
library(janitor)
library(broom)
library(readxl)
#constants---------------------
jo_start <- "2024"
jo_end <- "2033"
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

skills_long <- read_excel(here("data", "Skills.xlsx"))%>%
  clean_names()%>%
  select(o_net_soc_code, element_name, scale_name, data_value)%>%
  inner_join(mapping)%>%
  select(-o_net_soc_code)%>%
  select(noc, everything())%>%
  group_by(noc, element_name, scale_name)%>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) #mapping from SOC to NOC is not one to one: mean give one value per NOC

write_csv(skills_long, here("out","skills_long.csv"))


#job openings-------------------

jo <- read_excel(here("data", "jo_rich.xlsx"), skip = 3)%>%
  pivot_longer(cols=jo_start:jo_end, names_to = "year", values_to = "jo")%>%
  group_by(NOC, Description)%>%
  summarize(job_openings=sum(jo))%>%
  filter(NOC!="#T")%>%
  mutate(NOC=str_replace_all(NOC, "#",""))%>%
  unite(noc, "NOC","Description", sep=": ")%>%
  ungroup()%>%
  mutate(jo_prop=job_openings/sum(job_openings))

#income data--------------------------

income <- readxl::read_excel(here("data",
                                  "Employment income by NOC5_2021 census.xlsx"),
                             skip = 3,
                             na = "x")%>%
  select(noc="...1", income = `Median employment income ($)`)%>%
  separate(noc, into=c("noc2021", "noc2021_title"), sep = 5)%>%
  unite(noc, noc2021, noc2021_title, sep=":")%>%
  ungroup()%>%
  mutate(income_prop=income/sum(income, na.rm=TRUE))

# create skill measure-----------------

skills <- skills_long%>%
  pivot_wider(names_from = scale_name, values_from = data_value)%>%
  mutate(skill=sqrt(Level*Importance))%>%
  ungroup()%>%
  select(-Level,-Importance)

skills%>%
  pivot_wider(names_from = element_name, values_from = skill)%>%
  column_to_rownames("noc")%>%
  write_rds(here("out","just_skills.rds"))

skills%>%
  inner_join(jo)%>%
  inner_join(income)%>%
  mutate(skill_by_jo = skill * jo_prop,
         skill_by_income = skill * income_prop
         )%>%
  group_by(element_name)%>%
  summarize(skill_by_jo=sum(skill_by_jo, na.rm=TRUE),
            skill_by_income=sum(skill_by_income, na.rm = TRUE)
            )%>%
  write_csv(here("out","skills.csv"))

#get elasticities-------------------------

log_skills <- skills%>%
  mutate(skill=log(skill+1))%>%
  pivot_wider(names_from = element_name, values_from = skill)

log_income <- income%>%
  mutate(Income=log(income))%>%
  select(-income_prop, -income)

tbbl <- inner_join(log_income, log_skills)%>%
  column_to_rownames("noc")

mod <- lm(Income ~ ., data=tbbl)

sorted_estimates <- mod%>%
  tidy()%>%
  filter(term!="(Intercept)")%>%
  arrange(desc(estimate))%>%
  mutate(av_plot=map(term, av_wrapper))%>%
  write_rds(here("out","sorted_estimates.rds"))


