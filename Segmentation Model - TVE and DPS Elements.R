library(fmsb)
library(GGally)

library(gridGraphics)
library(gridExtra)

# Run engagement model first



tve_elements = prospect_features_scored %>%
  select(-c(TV_ENGAGEMENT_SCORE_SUM))

et_contact_elements = dbGetQuery(myconn, "

          WITH
          
          
          CONSTITUENT_LIST AS (
              SELECT
                ID AS CONTACT_ID,
                ZEROIFNULL(DDS_SCORE) AS DPS_SCORE,
                ZEROIFNULL(DDS_RECENCY) AS DPS_RECENCY,
                ZEROIFNULL(DDS_MONETARY) AS DPS_MONETARY,
                ZEROIFNULL(DDS_FREQUENCY) AS DPS_FREQUENCY 
              FROM
                BW.CONTACTS.ICE_CONTACT C
                LEFT JOIN BW.CONTACTS.ET_SCORES E ON C.ID = E.CONTACT_ID
              WHERE
                C.OID = 602
          ),
          
          
          EMAILS AS (
              SELECT
                E.CONTACT_ID,
                E.EMAIL,
                E.EMAIL_PRIMARY
              FROM
                BW.CONTACTS.ICE_EMAIL E
              WHERE
                E.CONTACT_ID IN (SELECT DISTINCT CONTACT_ID FROM CONSTITUENT_LIST)
          )  
          
          
          SELECT
            C.CONTACT_ID,
            E.EMAIL,
            E.EMAIL_PRIMARY AS PRIMARY_EMAIL,
            C.DPS_SCORE,
            C.DPS_RECENCY,
            C.DPS_MONETARY,
            C.DPS_FREQUENCY
          FROM
            EMAILS E
            LEFT JOIN CONSTITUENT_LIST C ON E.CONTACT_ID = C.CONTACT_ID
          ORDER BY
            C.CONTACT_ID
                                    "
                          )


combined_elements = left_join(et_contact_elements, tve_elements, by = c("EMAIL" = "TV_DELIVERY_EMAIL")) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))



# Combine at the contact level
contact_id_aggregated_data <- combined_elements %>%
  group_by(CONTACT_ID) %>%
  arrange(desc(TV_ENGAGEMENT_SCORE), desc(PRIMARY_EMAIL)) %>%
  slice(1) %>%   # Select the first row from the ordered group
  ungroup()




# Prep for clustering

# Model 1 - DPS Elements Separate
clustering_vars <- contact_id_aggregated_data %>%
  select(#DPS Elements
         DPS_RECENCY, DPS_MONETARY, DPS_FREQUENCY, 
         # ThankView Elements
         TOTAL_RECEIVED, PERCENT_BOUNCED, PERCENT_UNSUBSCRIBE, DAYS_SINCE_FIRST,
         DAYS_SINCE_LAST, DELIVERY_RANGE, AVG_CLICKS, AVG_VIEWS,
         PERCENT_STARTED, PERCENT_25, PERCENT_50, PERCENT_75,
         PERCENT_FINISHED, PERCENT_SHARED, PERCENT_DOWNLOADED)


# Model 2 - DPS Element as one score
clustering_vars <- contact_id_aggregated_data %>%
  select(#DPS Elements
    DPS_SCORE,
    # ThankView Elements
    TOTAL_RECEIVED, PERCENT_BOUNCED, PERCENT_UNSUBSCRIBE, DAYS_SINCE_FIRST,
    DAYS_SINCE_LAST, DELIVERY_RANGE, AVG_CLICKS, AVG_VIEWS,
    PERCENT_STARTED, PERCENT_25, PERCENT_50, PERCENT_75,
    PERCENT_FINISHED, PERCENT_SHARED, PERCENT_DOWNLOADED)


# Model 3 - All DPS elements and Overall Score
clustering_vars <- contact_id_aggregated_data %>%
  select(#DPS Elements
    DPS_SCORE, DPS_RECENCY, DPS_MONETARY, DPS_FREQUENCY,
    # ThankView Elements
    TOTAL_RECEIVED, PERCENT_BOUNCED, PERCENT_UNSUBSCRIBE, DAYS_SINCE_FIRST,
    DAYS_SINCE_LAST, DELIVERY_RANGE, AVG_CLICKS, AVG_VIEWS,
    PERCENT_STARTED, PERCENT_25, PERCENT_50, PERCENT_75,
    PERCENT_FINISHED, PERCENT_SHARED, PERCENT_DOWNLOADED)





# Standardize the variables to give them equal weight.
scaled_data <- scale(clustering_vars)



# Determine the optimal number of clusters (elbow)
set.seed(414)  # For reproducibility
wss <- numeric()
max_k <- 12  # Try clusters from 1 to 15
for (k in 1:max_k) {
  km_out <- kmeans(scaled_data, centers = k, nstart = 35, , iter.max = 25)
  wss[k] <- km_out$tot.withinss
}

# Plot the elbow curve
plot(1:max_k, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Clusters Sum of Squares",
     main = "Elbow Method For Optimal k")





# Based on the elbow plot, choose an appropriate number of clusters.
# For illustration, let’s assume the elbow appears at k = 4.
optimal_k <- 8

# Run the final k-means clustering with the chosen number of clusters.
set.seed(414)
final_kmeans <- kmeans(scaled_data, centers = optimal_k, nstart = 35, iter.max = 25)



# Attach the cluster assignments back to the aggregated data.
contact_id_aggregated_data$SEGMENT <- final_kmeans$cluster



# (Optional) Visualize the Clusters Using PCA.
pca_out <- prcomp(scaled_data)

pca_data <- data.frame(
  PC1 = pca_out$x[, "PC1"],
  PC2 = pca_out$x[, "PC2"],
  cluster = factor(final_kmeans$cluster)
)

ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "Cluster Visualization Using PCA") +
  theme_minimal()




# Examine the cluster centers in original, unscaled units.
cluster_centers <- contact_id_aggregated_data %>%
  # Assuming SEGMENT is your cluster identifier (if not, use the correct name)
  select(SEGMENT, DPS_SCORE,
         
         TOTAL_RECEIVED, PERCENT_BOUNCED, PERCENT_UNSUBSCRIBE, DAYS_SINCE_FIRST,
         DAYS_SINCE_LAST, DELIVERY_RANGE, AVG_CLICKS, AVG_VIEWS,
         PERCENT_STARTED, PERCENT_25, PERCENT_50, PERCENT_75,
         PERCENT_FINISHED, PERCENT_SHARED, PERCENT_DOWNLOADED) %>%
  group_by(SEGMENT) %>%
  summarise(records = n(),,
            across(everything(), ~ mean(.x, na.rm = TRUE)))

print(cluster_centers)




# Visualize cluster centers
# Method 1 - Heat mapping
centers_long <- cluster_centers %>%
  select(-c(records, DAYS_SINCE_FIRST, DELIVERY_RANGE)) %>%
  pivot_longer(-SEGMENT, names_to = "feature", values_to = "value")

# Create a heatmap
ggplot(centers_long, aes(x = feature, y = factor(SEGMENT), fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Cluster Centers Heatmap",
       x = "Feature",
       y = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Method 2 - Facet bars
centers_long <- cluster_centers %>%
  select(-c(records, DAYS_SINCE_FIRST, DELIVERY_RANGE)) %>%
  pivot_longer(-SEGMENT, names_to = "feature", values_to = "value")

ggplot(centers_long, aes(x = factor(SEGMENT), y = value, fill = factor(SEGMENT))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ feature, scales = "free_y") +
  labs(title = "Average Feature Values by Cluster",
       x = "Cluster",
       y = "Average Value") +
  theme_minimal()





# Method 3 - Radar Plots

# Choose a subset of features to display 
selected_features <- c("DPS_SCORE", "TOTAL_RECEIVED", "DAYS_SINCE_LAST", 
                       "PERCENT_FINISHED", "AVG_CLICKS", "AVG_VIEWS", 
                       "PERCENT_SHARED", "PERCENT_UNSUBSCRIBE", "PERCENT_BOUNCED")

# Create a data frame with one row per cluster.
radar_data <- cluster_centers %>%
  select(SEGMENT, all_of(selected_features)) %>%
  arrange(SEGMENT) %>%
  as.data.frame()

# Use SEGMENT as row names.
rownames(radar_data) <- paste("Cluster", radar_data$SEGMENT)
radar_data <- radar_data[ , -1]  # remove SEGMENT column

# Determine the max and min for each feature (across clusters).
max_values <- apply(radar_data, 2, max, na.rm = TRUE)
min_values <- apply(radar_data, 2, min, na.rm = TRUE)

# Create the data frame for fmsb: first row = max, second row = min, then the actual data.
radar_data_for_plot <- rbind(max_values, min_values, radar_data)

n_clusters <- nrow(radar_data)
colors_border <- rainbow(n_clusters)
colors_in <- alpha(colors_border, 0.3)

# Plot the radar chart.
fmsb::radarchart(radar_data_for_plot,
                 axistype = 1,
                 pcol = colors_border,
                 pfcol = colors_in,
                 plwd = 2,
                 cglcol = "grey",
                 cglty = 1,
                 axislabcol = "grey",
                 caxislabels = rep("", 5),  # empty labels
                 vlcex = 0.8,
                 title = "Radar Chart for Cluster Centers")


# Add a legend.
legend("right", legend = rownames(radar_data), bty = "n", 
       pch = 20, col = colors_border, text.col = "black", cex = 0.8)








# Prospect count by segment
prospect_data <- cluster_centers %>%
  select(SEGMENT, records) %>%
  arrange(SEGMENT)

ggplot(prospect_data, aes(x = factor(SEGMENT), y = records, fill = factor(SEGMENT))) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Prospects per Segment",
       x = "Segment",
       y = "Number of Records") +
  theme_minimal() +
  theme(legend.position = "none")
