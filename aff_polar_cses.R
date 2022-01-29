setwd("C:/Users/dapon/Dropbox/Harvard")
library(tidyverse)

raw <- readRDS("G2/Varieties/Project/cses_clean.Rdata")

#recode values of "like" variables as NA
cses <- raw %>% 
  mutate(id = seq(1, nrow(raw), 1)) %>% 
  mutate_at(vars(starts_with("like")), ~ ifelse(.x > 10, NA, .x)) 

#reverse the ratings on 0-10 scale, then re-merge with cses 
rev_ratings <- cses %>% 
  select(id, starts_with("like")) %>% 
  pivot_longer(cols = likeA:likeI, 
               names_to = "party",
               values_to = "rating") %>% 
  mutate(rating_reverse = 10 - rating) %>% 
  pivot_wider(id_cols = id, names_from = "party",
              values_from = "rating_reverse")

cses <- cses %>% 
  select(-starts_with("like")) %>% 
  left_join(., rev_ratings, by = "id")

#get vote toals
total_votes <- cses %>% 
  dplyr::select(id, starts_with("voteshare")) %>%
  pivot_longer(cols = starts_with("voteshare"),
               names_to = "party", values_to = 'share') %>% 
  group_by(id) %>% 
  mutate(total_vote = sum(share, na.rm = TRUE),
         voteshare_norm = share / total_vote,
         party_letter = str_remove(party, "voteshare")) 

#cses <- cses %>% left_join(., total_votes, by  = "id") 

#get df of which party-letter each person is a partisan of 
parties <- cses %>% 
  select(id, starts_with("party")) %>% 
  mutate(partyNA = 9999999) %>% as_tibble() %>% 
  pivot_longer(cols = partyA:partyNA,
               names_to = "party", values_to = "value") %>% 
  filter(party.id == value) %>% 
  mutate(party_letter = ifelse(party.id == 9999999, NA,
                               str_remove(party, "party"))) %>% 
  select(id, party.id, party_letter)




like_long <- cses %>% 
  select(id, starts_with("like")) %>% 
  mutate(likeNA = NA ) %>% 
  pivot_longer(starts_with("like"),
               values_to = "like") 

#put all the datasets together, to get weighted outparty dislike scores
outparty_dislikes <- left_join(parties, like_long, by = "id") %>% 
  mutate(name_party = ifelse(name == "likeNA", "NA", str_remove(name, "like")),
         party_id_letter = ifelse(party.id == 9999999, "NA", party_letter)) %>% 
  select(-party_letter) %>% 
  filter(party_id_letter != name_party) %>% 
  left_join(total_votes, by = c("id","name_party" = "party_letter")) %>% 
  mutate(weighted_like = like * voteshare_norm) %>% 
  select(-name, -party) %>% 
  #now get the vote share of the respondent's own party
  left_join(
    total_votes %>% 
      select(id, party_letter, voteshare_norm) %>% 
      rename(voteshare_norm_party_id = voteshare_norm), 
    by = c("id", "party_id_letter" = 'party_letter')) %>% 
  #now weight the outparty dislike scores by the size of the respondent's own party
  #if the respondent is a partisan of no party, assign their "party" a voteshare of 0
  mutate(voteshare_norm_party_id = 
           case_when(
             party.id == 9999999 ~ 0,
             TRUE ~ voteshare_norm_party_id), 
         weighted_like_norm = (like * voteshare_norm) / (1 - voteshare_norm_party_id)) 
  
#now sum the outparty dislike scores
outparty_dislikes_sum <- outparty_dislikes %>% 
  select(id, weighted_like_norm) %>% 
  group_by(id) %>% 
  summarize(outparty_dislike = sum(weighted_like_norm, na.rm = TRUE))


#get df of how much each person likes their own party
#obviously this applies only to partisans, not non-partisans
inparty_likes <- left_join(parties, like_long, by = "id") %>% 
  rename(party_id_letter = party_letter) %>% 
  mutate(party_name = str_remove(name, "like")) %>% 
  select(-name) %>% 
  filter(party_id_letter == party_name) %>% 
  select(id, like, party_id_letter) %>% 
  rename(like_own_party = like)


likes <- left_join(outparty_dislikes_sum, inparty_likes,
                   by = "id") %>% 
  #create varaible to measure how much the person likes hte inparty relative to outparties
  #negative values implies person likes inparty more than dislikes outparties 
  mutate(inparty_outparty_like_diff = like_own_party - outparty_dislike)

#join this back in with the cses data
cses <- left_join(cses, likes, by = "id") 


#create summary dataframe for outparty dislike
election_list <- c("AUS_1996", "AUS_2004", "AUS_2007", "AUS_2013", 
                   "AUT_2008","AUT_2013", 
                   "CAN_1997","CAN_2004","CAN_2008","CAN_2011","CAN_2015",
                   #note that CAN_2015 is not in A/G/H
                   "CHE_1999","CHE_2003","CHE_2007","CHE_2011",
                   "DEU12002","DEU22002","DEU_1998",  "DEU_2005","DEU_2009","DEU_2013",
                   "DNK_1998","DNK_2001","DNK_2007",
                   "ESP_1996", "ESP_2000","ESP_2004","ESP_2008",
                   "FIN_2003","FIN_2007","FIN_2011","FIN_2015",
                   "FRA_2002","FRA_2007","FRA_2012",
                   "GBR_1997","GBR_2005","GBR_2015",
                   "GRC_2009", "GRC_2012","GRC_2015",
                   #note that GRC_2015 is not in A/G/H
                   "IRL_2002","IRL_2007","IRL_2011",
                   "ISL_1999","ISL_2003", "ISL_2007","ISL_2009","ISL_2013",
                   "ISR_1996","ISR_2003","ISR_2006","ISR_2013",
                   "ITA_2006", 
                   #NOTE THAT A/G/H do not include Italy
                   "NLD_1998","NLD_2002","NLD_2006","NLD_2010",
                   "NOR_1997","NOR_2001","NOR_2005","NOR_2009","NOR_2013",
                   "NZL_1996","NZL_2002","NZL_2008","NZL_2011","NZL_2014",
                   "PRT_2002","PRT_2005","PRT_2009","PRT_2015",
                   "SWE_1998","SWE_2002","SWE_2006","SWE_2014",
                   "USA_1996","USA_2004","USA_2008","USA_2012"
)

#make plot 
# I think I am not calculating the polarization scores correctlly
cses %>% 
  filter(election %in% election_list,
         !is.na(party_id_letter),
         !(country %in% c("FRA", "CHE", "ITA"))) %>% 
  group_by(election) %>% 
  summarize(aff_polar = mean(outparty_dislike, na.rm = TRUE)) %>% 
  mutate(country = substr(election, 1, 3)) %>% 
  group_by(country) %>% 
  summarize(mean_polar = mean(aff_polar, na.rm = TRUE),
            min_polar = min(aff_polar, na.rm = TRUE),
            max_polar = max(aff_polar, na.rm = TRUE),
            n = n()) %>% 
  arrange(mean_polar) %>% 
  ggplot() +
  geom_point(aes(x = reorder(country, mean_polar), y = mean_polar)) + 
  geom_linerange(aes(x = country, y = mean_polar,
                     ymin = min_polar, ymax = max_polar)) + 
  coord_flip() + 
  theme_minimal()

