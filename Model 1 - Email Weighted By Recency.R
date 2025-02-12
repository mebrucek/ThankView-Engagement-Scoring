
library(tidyverse)
library(odbc)
library(DBI)
library(dplyr)
library(dbplyr)


options(scipen = 999)
setwd("C:\\Users\\MichaelBrucek\\Desktop\\Analytics\\ThankView Engagement Models")
`%notin%` <- Negate(`%in%`)


myconn <- DBI::dbConnect(odbc::odbc(), 
                         "BlueWhale", 
                         uid=rstudioapi::askForPassword("Database User"), 
                         pwd=rstudioapi::askForPassword("Database Password"))



# Collect Data ----

# Davidson College
email_data = dbGetQuery(myconn, "SELECT * 
                                 FROM BW.SANDBOX_BI.MIKE_THANKVIEW_INDIVIDUAL_EMAIL_STATS E
                                 WHERE '602' IN (E.ET_OIDS)
                        "
                        )


data_transformed = email_data %>%
  mutate(TIME_SINCE_SENT = as.numeric(Sys.Date() - SENT_DATE),
         TIME_TO_OPEN = as.numeric(OPENED_DATE - SENT_DATE),
         TIME_TO_READ = as.numeric(READ_EMAIL_DATE - SENT_DATE),
         TIME_TO_START_VIDEO = as.numeric(STARTED_VIDEO_DATE - SENT_DATE),
         TIME_TO_CTA_CLICK = as.numeric(CTA_CLICKED_DATE - SENT_DATE),
  ) %>%
  select(TV_DELIVERY_EMAIL,
         BOUNCED,
         UNSUBSCRIBED,
         TIME_SINCE_SENT,
         TIME_TO_OPEN,
         TIME_TO_READ,
         TIME_TO_CTA_CLICK,
         CLICKS,
         VIDEO_VIEWS,
         STARTED_VIDEO,
         VIDEO_POS_1,
         VIDEO_POS_2,
         VIDEO_POS_3,
         FINISHED_VIDEO,
         VIDEO_SHARES_TO_FACEBOOK,
         VIDEO_DOWNLOADS
         )




# Create Features
prospect_features = data_transformed %>%
  group_by(TV_DELIVERY_EMAIL) %>%
  summarise(TOTAL_RECEIVED = n(),
            PERCENT_BOUNCED = sum(BOUNCED) / TOTAL_RECEIVED,
            PERCENT_UNSUBSCRIBE = sum(UNSUBSCRIBED)  / TOTAL_RECEIVED,
            DAYS_SINCE_FIRST = max(TIME_SINCE_SENT),
            DAYS_SINCE_LAST = min(TIME_SINCE_SENT),
            DELIVERY_RANGE = max(DAYS_SINCE_FIRST) - max(DAYS_SINCE_LAST),
            AVG_CLICKS = sum(CLICKS) / TOTAL_RECEIVED,
            AVG_VIEWS = sum(VIDEO_VIEWS) / TOTAL_RECEIVED,
            PERCENT_STARTED = sum(STARTED_VIDEO) / TOTAL_RECEIVED,
            PERCENT_25 = sum(VIDEO_POS_1) / TOTAL_RECEIVED,
            PERCENT_50 = sum(VIDEO_POS_2) / TOTAL_RECEIVED,
            PERCENT_75 = sum(VIDEO_POS_3) / TOTAL_RECEIVED,
            PERCENT_FINISHED = sum(FINISHED_VIDEO) / TOTAL_RECEIVED,
            PERCENT_SHARED = sum(VIDEO_SHARES_TO_FACEBOOK) / TOTAL_RECEIVED,
            PERCENT_DOWNLOADED = sum(VIDEO_DOWNLOADS) / TOTAL_RECEIVED
  )
            




# Convert to scaled numeric format
prospect_features_normalized <- prospect_features %>%
  mutate(across(where(is.numeric), ~scale(.) %>% as.numeric()) 
         )  %>% # Standardize all numeric columns
  select(-c(DAYS_SINCE_FIRST, DELIVERY_RANGE))




# Create Score
engagement_score_sum <- prospect_features_normalized %>%
  mutate(TV_ENGAGEMENT_SCORE_SUM = 
           (1 * TOTAL_RECEIVED) +
           (-3 * PERCENT_BOUNCED) + 
           (-3 * PERCENT_UNSUBSCRIBE) +
           (1 * DAYS_SINCE_LAST) +
           (1 * AVG_CLICKS) + 
           (1 * AVG_VIEWS) + 
           (1 * PERCENT_STARTED) +
           (1 * PERCENT_25) +
           (2 * PERCENT_50) +
           (2 * PERCENT_75) + 
           (4 * PERCENT_FINISHED) +
           (1 * PERCENT_SHARED) +
           (1 * PERCENT_DOWNLOADED)
  ) %>%
  select(TV_ENGAGEMENT_SCORE_SUM, everything()) %>%
  filter(is.na(TV_ENGAGEMENT_SCORE_SUM) == FALSE)  




# Method 1
# engagement_score_cume <- engagement_score %>%
#   mutate(centile = ceiling(100 * cume_dist(TV_ENGAGEMENT_SCORE)))
# 
# # The function cume_dist() computes the cumulative distribution, which for a given value, is the fraction of observations with a score less than or equal to that value. 
# # This naturally uses the maximum rank when encountering ties, so this works too.
# 
# ggplot(engagement_score_cume, aes(x = centile)) +
#   geom_histogram(fill = "#995cd3", bins = 60) +
#   labs(title = "Distribution of Engagement Scores",
#        subtitle = "Cumulative Distribution Method",
#        x = "Engagement Score",
#        y = "Count") +
#   theme_minimal()




# Method 2  (seems easier for non-tech audience to interpret)
engagement_score_rank <- engagement_score_sum %>%
  mutate(centile = ceiling(100 * rank(TV_ENGAGEMENT_SCORE_SUM, ties.method = "max") / nrow(.)))

# The rank() function assigns each value its rank in the data. Using ties.method = "max" ensures that if several observations have the same TV_ENGAGEMENT_SCORE, they 
# all receive the highest rank among them. In other words, if a tie spans what would have been two centile boundaries, every tied observation is placed in the higher centile.
# 
# Dividing the rank by the total number of rows (nrow(.)) gives the relative position (from 0 to 1) of each score in the dataset, multiplying by 100 scales this to a 0-100 percentage. 
# 
# Taking the ceiling makes sure that even a score that is just a tiny fraction above a centile boundary is placed in the higher centile.

ggplot(engagement_score_rank , aes(x = centile)) +
  geom_histogram(fill = "#995cd3", bins = 60) +
  labs(title = "Distribution of Engagement Scores",
       subtitle = "Ranking Method",
       x = "Engagement Score",
       y = "Count") +
  theme_minimal()





# Add the score back to the original data sets for review:
prospect_features_scored = engagement_score_rank %>%
  select(TV_DELIVERY_EMAIL, centile) %>%
  left_join(prospect_features, ., by = "TV_DELIVERY_EMAIL") %>%
  select(TV_DELIVERY_EMAIL, centile, everything()) %>%
  rename(TV_ENGAGEMENT_SCORE = centile) %>%
  left_join(., (engagement_score_sum %>% 
                  select(TV_DELIVERY_EMAIL, TV_ENGAGEMENT_SCORE_SUM)), by = "TV_DELIVERY_EMAIL") %>%
  select(TV_DELIVERY_EMAIL, TV_ENGAGEMENT_SCORE, TV_ENGAGEMENT_SCORE_SUM, everything())



