##### REPLICATION CODE FOR STATS IN REPORT

# % talk to close friend outside neighborhood at least once per week
gssdata %>% 
  filter(year == 2021) %>% 
  mutate(PID = case_when(partyid == 0 | partyid == 1 | partyid == 2 ~ 'D',
                         partyid == 3 ~ 'I',
                         partyid == 4 | partyid == 5 | partyid == 6 ~ 'R'),
         outside = ifelse(socfrend == 1 | socfrend == 2, 1, 0)) %>% # add 3 + 4 if you want to look at "at least once a month"
  drop_na(PID, outside) %>% 
  group_by(PID) %>% 
  count(outside, wt = wtssps) %>% 
  mutate(pct = prop.table(n))

# frequency outdoor activities GSS
gssdata %>% 
  filter(year == 2021) %>% 
  mutate(PID = case_when(partyid == 0 | partyid == 1 | partyid == 2 ~ 'D',
                         partyid == 3 ~ 'I',
                         partyid == 4 | partyid == 5 | partyid == 6 ~ 'R'),
         outside = case_when(activnat == 1 ~ 'daily', 
                             activnat == 2 ~ 'other', 
                             activnat == 3 ~ 'other',
                             activnat == 4 ~ 'other',
                             activnat == 5 ~ 'other')) %>% 
  drop_na(PID, outside) %>% 
  group_by(PID) %>% 
  count(outside, wt = wtssps) %>% 
  mutate(pct = prop.table(n))

# Importance of being popular GSS
gssdata <- read_dta('gsscumulative.dta')


gssdata %>%
  filter(year == 2021) %>%
  mutate(PID = case_when(partyid == 0 | partyid == 1 | partyid == 2 ~ 'D',
                         partyid == 3 ~ 'I',
                         partyid == 4 | partyid == 5 | partyid == 6 ~ 'R'),
         popular = ifelse(popular == 1, 1, 0)) %>% 
  drop_na(popular, PID) %>% 
  group_by(PID) %>%
  dplyr::count(popular, wt = wtssps) %>% 
  mutate(pct = prop.table(n))


# religious facts by party CES
cesdata <- read_csv('cces20.csv')

                      # imp
cesdata %>% 
  mutate(PID = case_when(pid7 == 5 | pid7 == 6 | pid7 == 7 ~ 'R',
                         pid7 == 1 | pid7 == 2 | pid7 == 3 ~ 'D',
                         T ~ 'I'),
         VERYIMP = ifelse(pew_religimp == 1, 1, 0)) %>% 
  drop_na(PID, VERYIMP) %>% 
  group_by(PID) %>% 
  dplyr::count(VERYIMP, wt = commonweight) %>% 
  mutate(pct = prop.table(n))

                     # at least once/week
cesdata %>% 
  mutate(PID = case_when(pid7 == 5 | pid7 == 6 | pid7 == 7 ~ 'R',
                         pid7 == 1 | pid7 == 2 | pid7 == 3 ~ 'D',
                         T ~ 'I'),
         ONEWEEK = ifelse(pew_churatd == 1 | pew_churatd == 2, 1, 0)) %>% 
  drop_na(PID, ONEWEEK) %>% 
  group_by(PID) %>% 
  dplyr::count(ONEWEEK, wt = commonweight) %>% 
  mutate(pct = prop.table(n))

                       # non_religious
cesdata %>% 
  mutate(PID = case_when(pid7 == 5 | pid7 == 6 | pid7 == 7 ~ 'R',
                         pid7 == 1 | pid7 == 2 | pid7 == 3 ~ 'D',
                         T ~ 'I'),
         NONE = ifelse(religpew == 9 | religpew == 10 | religpew == 11, 1, 0)) %>% 
  drop_na(PID, NONE) %>% 
  group_by(PID) %>% 
  dplyr::count(NONE, wt = commonweight) %>% 
  mutate(pct = prop.table(n))



# % say CEOs should earn less than 250000 GSS
gssdata %>% 
  filter(year == 2021) %>% 
  drop_na(givexec) %>% 
  mutate(PID = case_when(partyid == 0 | partyid == 1 | partyid == 2 ~ 'D',
                         partyid == 3 ~ 'I',
                         partyid == 4 | partyid == 5 | partyid == 6 ~ 'R'),
         UNDER250 = ifelse(givexec < 250000, 1, 0)) %>% 
  drop_na(PID, UNDER250) %>% 
  group_by(PID) %>% 
  dplyr::count(UNDER250, wt = wtssps) %>% 
  mutate(pct = prop.table(n))



# % think premarital sex is wrong GSS

gssdata %>% 
  filter(year == 2021) %>% 
  mutate(PID = case_when(partyid == 0 | partyid == 1 | partyid == 2 ~ 'D',
                         partyid == 3 ~ 'I',
                         partyid == 4 | partyid == 5 | partyid == 6 ~ 'R'),
         NOT_WRONG = ifelse(premarsx == 4, 1, 0)) %>% 
  drop_na(PID, NOT_WRONG) %>% 
  group_by(PID) %>% 
  dplyr::count(NOT_WRONG, wt = wtssps) %>% 
  mutate(pct = prop.table(n))


# % parents by party GSS
gssdata$agekdbrn <- gssdata$agekdbrn %>% replace_na(0)
gssdata %>% 
  filter(year == 2021) %>% 
  mutate(PID = case_when(partyid == 0 | partyid == 1 | partyid == 2 ~ 'D',
                         partyid == 3 ~ 'I',
                         partyid == 4 | partyid == 5 | partyid == 6 ~ 'R'),
         PARENT = ifelse(agekdbrn > 0, 1, 0)) %>% # ordinary child variable 
  drop_na(PID, PARENT) %>% 
  group_by(PID) %>% 
  dplyr::count(PARENT, wt = wtssps) %>% 
  mutate(pct = prop.table(n))

# share home owner by party ces

cesdata <- read_csv('cces20.csv')

cesdata %>% 
  mutate(PID = case_when(pid7 == 1 | pid7 == 2 | pid7 == 3 ~ 'D',
                         pid7 == 4 ~ 'I',
                         pid7 == 5 | pid7 == 6 | pid7 == 7 ~ 'R'),
         OWN = ifelse(ownhome == 1, 1, 0)) %>% 
  drop_na(PID, OWN) %>% 
  group_by(PID) %>% 
  count(OWN, wt = commonweight) %>% 
  mutate(pct = prop.table(n))


# % unhappy w financial situation gss

gssdata %>% 
  filter(year == 2021) %>% 
  mutate(PID = case_when(partyid == 0 | partyid == 1 | partyid == 2 ~ 'D',
                         partyid == 3 ~ 'I',
                         partyid == 4 | partyid == 5 | partyid == 6 ~ 'R'),
         unsatisfied = case_when(satfin == 1 ~ 'no', satfin == 2 ~ 'no',
                                 satfin == 3 ~ 'yes')) %>% 
  drop_na(PID, unsatisfied) %>% 
  group_by(PID) %>% 
  count(unsatisfied, wt = wtssps) %>% 
  mutate(pct = prop.table(n))


# relig is "very important" by party
cesdata <- read_csv('cces20.csv')

cesdata %>% 
  mutate(PID = case_when(pid7 == 1 | pid7 == 2 | pid7 == 3 ~ 'D',
                         pid7 == 4 ~ 'I',
                         pid7 == 5 | pid7 == 6 | pid7 == 7 ~ 'R'),
         VERYIMP = ifelse(pew_religimp == 1, 1, 0)) %>% 
  drop_na(PID, VERYIMP) %>% 
  group_by(PID) %>% 
  count(VERYIMP, wt = commonweight) %>% 
  mutate(pct = prop.table(n))



# Happiness level by party GSS

gssdata %>% 
  filter(year == 2021) %>% 
  mutate(PID = case_when(partyid == 0 | partyid == 1 | partyid == 2 ~ 'D',
                         partyid == 3 ~ 'I',
                         partyid == 4 | partyid == 5 | partyid == 6 ~ 'R'),
         unhappy = case_when(happy == 1 ~ 'no',  happy == 2 ~ 'no', 
                             happy == 3 ~ 'yes')) %>% 
  drop_na(PID, unhappy) %>% 
  group_by(PID) %>% 
  count(unhappy, wt = wtssps) %>% 
  mutate(pct = prop.table(n))




# luck is more important than party
gssdata %>%
  filter(year == 2021) %>%
  mutate(PID = case_when(partyid == 0 | partyid == 1 | partyid == 2 ~ 'D',
                         partyid == 3 ~ 'I',
                         partyid == 4 | partyid == 5 | partyid == 6 ~ 'R'),
         ahead = case_when(getaheadnv == 1 ~ 'no', getaheadnv == 2~ 'no',
                           getaheadnv == 3 ~ 'yes')) %>% 
  drop_na(PID, ahead) %>% 
  group_by(PID) %>%
  dplyr::count(ahead, wt = wtssps) %>% 
  mutate(pct = prop.table(n))


# Race differences are due to discrimination R/I/D GSS
gssdata %>% 
  filter(year == 2021) %>% 
  mutate(PID = case_when(partyid == 0 | partyid == 1 | partyid == 2 ~ 'D',
                         partyid == 3 ~ 'I',
                         partyid == 4 | partyid == 5 | partyid == 6 ~ 'R'),
         Disc = case_when(racdif1 == 1 ~ 'Yes', racdif1 == 2 ~ 'No')) %>% 
  drop_na(PID, Disc) %>% 
  group_by(PID) %>% 
  count(Disc, wt = wtssps) %>% 
  mutate(pct = prop.table(n))



# hours watch tv a day R/I/D GSS
gssdata %>% 
  filter(year == 2021,
         tvhours >=0) %>% 
  mutate(PID = case_when(partyid == 0 | partyid == 1 | partyid == 2 ~ 'D',
                         partyid == 3 ~ 'I',
                         partyid == 4 | partyid == 5 | partyid == 6 ~ 'R')) %>% 
  drop_na(PID, tvhours) %>% 
  group_by(PID) %>% 
  dplyr::summarize(pct = mean(tvhours, w = wtspss))



# Percent D/I/R with broadband at home

cesdata %>% 
  mutate(int = ifelse(internethome == 1, 1, 0)) %>% 
  group_by(PID = case_when(pid7 == 1 | pid7 == 2 | pid7 == 3 ~ 'D',
                           pid7 == 4 ~ 'I',
                           pid7 == 5 | pid7 == 6 | pid7 == 7 ~ 'R')) %>% 
  drop_na(int, PID) %>% 
  group_by(PID) %>% 
  count(int, wt = commonweight) %>% 
  mutate(pct = prop.table(n))



# Percent of D/I/R/all who get news from social media

cesdata %>% 
  mutate(not_use = ifelse(CC20_300d_6 == 1, 1, 0)) %>% 
  group_by(PID = case_when(pid7 == 1 | pid7 == 2 | pid7 == 3 ~ 'D',
                           pid7 == 4 ~ 'I',
                           pid7 == 5 | pid7 == 6 | pid7 == 7 ~ 'R')) %>% 
  drop_na(PID, not_use) %>%
  group_by(PID) %>% 
  count(not_use, wt = commonweight) %>% 
  mutate(pct = prop.table(n))

             # overall
cesdata %>% 
  mutate(not_use = ifelse(CC20_300d_6 == 1, 1, 0)) %>% 
  drop_na(not_use) %>%
  count(not_use, wt = commonweight) %>% 
  mutate(pct = prop.table(n))









