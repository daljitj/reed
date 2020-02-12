library(tidyverse)
library(tidytext)
library(wordcloud2)
library(scales)
data("stop_words")



######## get work directory
setwd("C:/Users/inetc/OneDrive/Documents/R/Job listings")
getwd()

## load data
raw <- read_csv("job listing.csv")
raw <- raw %>% filter(!job_title=="Similar jobs")

## create extra stop words
extra_stopwords <- data_frame(word=c("scientist","london","apply"))

## remove "r" from stop word list
stop_words <- stop_words %>% filter(!word=="r")

## unnest tokens
descriptions <- unnest_tokens(raw,word,description) %>%
  anti_join(stop_words) %>%
  anti_join(extra_stopwords) %>%
  count(word,sort = TRUE) %>% ungroup()

## top words across all job titles

descriptions %>%
  filter(n > 400) %>%
  group_by(word) %>%
  mutate(sumn = sum(n)) %>%
  ungroup() %>%
  mutate(word = fct_reorder(word,sumn)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  ylab("Number of words") +
  coord_flip()

# wordcloud
filter(descriptions,n>50) %>%
wordcloud2()

## filter to languages and software

#words to filter one
software <- c("python","r","sql","spss","sas","powerbi","tableau","hadoop","spark","java","matlab","scala","excel")

descriptions %>% filter(word %in% software)



############# description bullet points


## unnest tokens
descriptions_bullets <- unnest_tokens(raw,word,description_bulletpoints) %>%
  anti_join(stop_words) %>%
  anti_join(extra_stopwords) %>%
  count(word,sort = TRUE) %>% ungroup() %>%
  drop_na()

## top words across all job titles

descriptions_bullets %>%
  filter(n > 300) %>%
  group_by(word) %>%
  mutate(sumn = sum(n)) %>%
  ungroup() %>%
  mutate(word = fct_reorder(word,sumn)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  ylab("Word count") +
  coord_flip()

# wordcloud
filter(descriptions_bullets,n>10) %>%
  wordcloud2()


## filter to languages and software

#words to filter one
software <- c("python","r","sql","spss","sas","powerbi","tableau","hadoop","spark","java","matlab","scala","excel")

descriptions_bullets %>% filter(word %in% software) %>%
  ggplot(aes(x=reorder(word,n),n))  +
  geom_col() +
  xlab(NULL) +
  ylab("Word count") +
  labs(title="Most common statisical languages and tools found on Data Science job listings",subtitle="Listings from www.reed.co.uk")+
  coord_flip() +
  geom_text(aes(x=word,y=n,label=n),vjust=0,hjust=-0.5) +
  theme_classic()


#### most common job titles

raw %>% count(job_title,sort=TRUE) %>% 
  filter(n>5) %>%
  ggplot(aes(reorder(job_title,n), n))+
  geom_col() +
  xlab(NULL) +
  ylab(NULL) +
  labs(title="Number of listings by job title",subtitle="Listings from www.reed.co.uk, job titiles with more than 5 incidences only") +
  geom_text(aes(x=job_title,y=n,label=n),vjust=0,hjust=-0.5) +
  coord_flip() +
  theme_classic()



#################### salary ##################

######### JUNIOR DATA SCIENTIST DISAPPEARING DUE TO NO SALARY SPECIFIED

## seperate salary into minimum and maximum
## remove salaries in per day basis or in non sterling currency
salaryset <- raw %>% select(job_title,salary,`employment type`) %>% filter(str_detect(salary,"per annum")) %>% 
  filter(str_detect(salary,"£")) %>% tidyr::separate(salary,c("min_salary","max_salary")," - ")

## extract only numbers
salaryset$min_salary <- parse_number(salaryset$min_salary)
salaryset$max_salary <- parse_number(salaryset$max_salary)

## calculate average

salaryset <- mutate(salaryset,average_salary=(max_salary+min_salary)/2)

salaryset <- mutate(salaryset,average_salary = if_else(is.na(average_salary)==TRUE,min_salary,average_salary))

## plot histogram

plot(density(salaryset$average_salary))

salaryset %>%
  filter(average_salary < 200000) %>%
  filter(job_title==c("Data Scientist","Junior Data Scientist","Senior Data Scientist","Graduate Data Scientist")) %>%
  ggplot( aes(x=average_salary,fill=job_title)) +
  geom_histogram( alpha=0.9) +
  scale_x_continuous(labels=dollar_format(prefix = "£"),breaks = seq(from = 0, to = 150000, by = 20000)) +
  theme_classic()

## plot density

salaryset %>%
  filter(average_salary < 200000) %>%
  ggplot( aes(x=average_salary)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  scale_x_continuous(labels=dollar_format(prefix = "£"),breaks = seq(from = 0, to = 150000, by = 20000)) +
  theme_classic() +
  labs(title="Salaries for data science jobs",subtitle="Based on job listings from reed.co.uk") +
  xlab("Average salary")


salaryset %>%
  filter(average_salary < 200000) %>%
  filter(job_title==c("Data Scientist","Junior Data Scientist","Senior Data Scientist","Graduate Data Scientist")) %>%
  ggplot( aes(x=average_salary,fill=job_title)) +
  geom_density(alpha=0.8) +
  scale_x_continuous(labels=dollar_format(prefix = "£"),breaks = seq(from = 0, to = 150000, by = 20000)) +
  theme_classic() 

## ridge plot plot
library(ggridges)

salaryset %>%
  filter(average_salary < 200000) %>%
  ggplot( aes(x=average_salary,y=reorder(fct_lump(job_title,9),average_salary),fill=fct_lump(job_title,10))) +
  geom_density_ridges() +
  theme_ridges(grid=FALSE) + 
  theme(legend.position = "none") +
  scale_x_continuous(labels=dollar_format(prefix = "£"),breaks = seq(from = 0, to = 150000, by = 20000)) +
  labs(x= "Average salary",y="")



## plot cdf
salaryset %>%
  filter(average_salary < 200000) %>%
ggplot(aes(average_salary,fill=fct_lump(job_title,4))) + stat_ecdf(geom = "point")


## by job title

salaryset %>% group_by(job_title) %>% summarise(average_wage = mean(average_salary)) %>%
  filter(average_wage < 200000) %>%
  top_n(25) %>%
  ggplot(aes(x=reorder(job_title,average_wage),average_wage)) +
  geom_col() +
  coord_flip() +
  labs(title="Highest paying data science positions",subtitle="Listings from www.reed.co.uk") +
  geom_text(aes(x=job_title,y=average_wage,label=round(average_wage,0),vjust=0,hjust=0)) +
  scale_y_continuous(labels=comma)  +
  theme_classic()


#### permanent vs contract

permanent <- raw %>% filter(str_detect(raw$`employment type`,"Permanent")==TRUE)
contract <- raw %>% filter(str_detect(raw$`employment type`,"Contract")==TRUE)




#################################### data analyst data
raw1 <- read_csv("data analyst job listing.csv")
raw1 <- raw1 %>% filter(!job_title=="Similar jobs")


## seperate salary into minimum and maximum
## remove salaries in per day basis or in non sterling currency
salaryset1 <- raw1 %>% select(job_title,salary,`employment type`) %>% filter(str_detect(salary,"per annum")) %>% 
  filter(str_detect(salary,"£")) %>% tidyr::separate(salary,c("min_salary","max_salary")," - ")

## extract only numbers
salaryset1$min_salary <- parse_number(salaryset1$min_salary)
salaryset1$max_salary <- parse_number(salaryset1$max_salary)

## calculate average

salaryset1 <- mutate(salaryset1,average_salary=(max_salary+min_salary)/2)

salaryset1 <- mutate(salaryset1,average_salary = if_else(is.na(average_salary)==TRUE,min_salary,average_salary))

########

## plot density



bind_rows(salaryset,salaryset1, .id = "id") %>%
  filter(average_salary < 200000) %>%
  ggplot( aes(x=average_salary,fill=id)) +
  geom_density(alpha=0.8)+
  scale_x_continuous(labels=dollar_format(prefix = "£"),breaks = seq(from = 0, to = 150000, by = 20000)) +
  scale_fill_discrete(name=NULL,labels=c("Data Scientist","Data Analyst")) +
  xlab("Average salary") +
  labs(title="Distribution of salaries",subtitle = "Data from www.reed.co.uk job listings") +
  theme_classic() 


## plot cdf


bind_rows(salaryset,salaryset1, .id = "id") %>%
  filter(average_salary < 200000) %>%
  ggplot(aes(average_salary,fill=id,color=id)) + stat_ecdf() +
  theme_classic() +
  scale_x_continuous(labels=dollar_format(prefix = "£"),breaks = seq(from = 0, to = 150000, by = 20000))

