# Relative path to the dataset
data_path <- file.path("data", "NS_cancer.csv")

# Read the dataset
puffer <- read.csv(data_path)

# Confirm successful data loading
if (exists("puffer")) {
  message("Data successfully loaded from ", data_path)
} else {
  stop("Error: Data file not found. Ensure 'project_data.csv' is in the 'data/' folder.")
}



########## Cleaning Data ##########


### 1. Challenges - Pre-processing and Dataset Analysis ###

# Extract current column names
current_names <- colnames(puffer)[1:55]

# Define new column names
new_names <- c(
  "ComID", "location", "Year", "gender", "age_group", "total_cancers",
  "head_neck", "Lip", "Tongue", "Mouth", "pharynx_tonsil", "paranasal_sinuses", "Larynx",
  "gastro_intestinal", "Esophagus", "Stomach", "small_bowel", "colon_rectum",
  "liver_biliary_tract", "Pancreas", "peritoneum_GI_unspecified",
  "Respiratory", "lung_trachea_bronchus", "mediastinum_pleura",
  "Breast", "skin_melanoma", "melanoma_skin", "other_skin",
  "gynecological", "cervix", "body_uterus", "Ovary", "other_female_genital",
  "genito_urinary", "Testis", "penis_male_genital_unspecified", "Bladder", "kidney_ureter_other_urinary",
  "lymphomas", "non_hodgkins_lymphoma", "hodgkins_lymphoma",
  "hematologic", "multiple_myeloma", "Leukemia", "miscellaneous_proliferative",
  "Other", "eye_lacrimal_gland", "Brain", "meninges_spinal_cord_other_CNS",
  "Thyroid", "other_endocrine", "bone_connective_tissue",
  "other_Ill_defined_sites", "unknown_primary", "Prostate"
)

# Assign new column names
colnames(puffer) <- new_names

# Generate a comma-separated string of column names and print it
print(paste(new_names, collapse = ","))

# Convert numeric columns (from column 6 onward) to integer after removing commas
puffer[, 6:ncol(puffer)] <- lapply(puffer[, 6:ncol(puffer)], function(x) {
  as.integer(gsub(",", "", x)) # Remove commas and convert to integer
})

##### Exploratory Data Analysis (Without NS) #####

# Create a copy of the dataset for separation
bowlder <- puffer

# Separate aggregated data (gender or age_group labeled as "Total")
aggregated_data <- bowlder[bowlder$gender == "Total" | bowlder$age_group == "Total", ]

# Retain granular data (excluding aggregated records)
granular_data <- bowlder[!(bowlder$gender == "Total" | bowlder$age_group == "Total"), ]

# Subset data by location
data_NS <- granular_data[granular_data$location == "Nova Scotia", ]  # Only NS
data_seeds <- granular_data[granular_data$location != "Nova Scotia", ]  # Without NS

# Identify and remove columns containing only NA values
na_columns <- colSums(is.na(data_seeds)) == nrow(data_seeds)
sub_seeds <- data_seeds[, !na_columns]  # Retains 16 columns after removal


### Summary ###

# Generate summary statistics
granular_summary <- summary(sub_seeds)

# Convert summary to an xtable object and export as an HTML file
print(xtable(granular_summary), type = "html", file = "fawkes.html")


### 3.1 Visualization - General Analysis (Gender-wise Relations) ###

# Select cancer-related columns (excluding the first six columns)
cancer_data <- sub_seeds[, 7:ncol(sub_seeds)]

# Replace NA values with 0 for summation
cancer_data[is.na(cancer_data)] <- 0

# Add gender column
cancer_data$gender <- sub_seeds$gender

# Summarize total cancer occurrences per gender
gender_cancer_sum <- cancer_data %>%
  group_by(gender) %>%
  summarise(TotalCancerOccurrences = sum(across(everything(), sum, na.rm = TRUE)))

# Calculate total occurrences and occurrence rate
total_occurrences <- sum(gender_cancer_sum$TotalCancerOccurrences)
gender_cancer_sum <- gender_cancer_sum %>%
  mutate(OccurrenceRate = (TotalCancerOccurrences / total_occurrences) * 100)

# Plot cancer occurrence rate by gender
ggplot(gender_cancer_sum, aes(x = gender, y = OccurrenceRate, fill = gender)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_minimal() +
  labs(
    title = "Cancer Occurrence Rate by Gender",
    x = "Gender",
    y = "Occurrence Rate"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )


### Cancer Type-wise Gender Rates ###

# Identify cancer type columns (from column 7 onwards)
cancer_types <- names(sub_seeds)[7:ncol(sub_seeds)]
cancer_data <- sub_seeds[, cancer_types]

# Replace NA values with 0
cancer_data[is.na(cancer_data)] <- 0

# Add gender column
cancer_data$Gender <- sub_seeds$gender

# Convert data to long format for easier analysis
long_cancer_data <- reshape2::melt(cancer_data, id.vars = "Gender", 
                                   variable.name = "CancerType", 
                                   value.name = "Occurrences")

# Summarize total cancer occurrences per gender for each cancer type
cancer_type_gender_sum <- long_cancer_data %>%
  group_by(CancerType, Gender) %>%
  summarise(TotalCancerOccurrences = sum(Occurrences, na.rm = TRUE), .groups = "drop")

# Calculate total occurrences for each cancer type
cancer_type_totals <- cancer_type_gender_sum %>%
  group_by(CancerType) %>%
  summarise(TypeTotal = sum(TotalCancerOccurrences), .groups = "drop")

# Merge totals back into the original dataset
cancer_type_gender_sum <- left_join(cancer_type_gender_sum, cancer_type_totals, by = "CancerType")

# Compute the occurrence rate for each gender within each cancer type
cancer_type_gender_sum <- cancer_type_gender_sum %>%
  mutate(OccurrenceRate = (TotalCancerOccurrences / TypeTotal) * 100)

# Plot cancer occurrence rate by gender for each cancer type
ggplot(cancer_type_gender_sum, aes(x = OccurrenceRate, y = CancerType, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Cancer Occurrence Rates by Gender for Each Cancer Type",
    x = "Percentage of Occurrences within Cancer Type",
    y = "Cancer Type"
  ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +  # Format x-axis as percentage
  scale_fill_manual(values = c("Male" = "blue", "Female" = "orange")) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )


### Gender Frequency of Cancer Incidents for Each Age Group ###

# Add gender and age group columns
cancer_data$Gender <- sub_seeds$gender
cancer_data$AgeGroup <- sub_seeds$age_group

# Summarize total cancer occurrences per gender within each age group
age_group_gender_sum <- cancer_data %>%
  group_by(AgeGroup, Gender) %>%
  summarise(TotalCancerOccurrences = sum(across(everything(), sum, na.rm = TRUE)), .groups = "drop")

# Calculate total occurrences for each age group
age_group_totals <- age_group_gender_sum %>%
  group_by(AgeGroup) %>%
  summarise(AgeGroupTotal = sum(TotalCancerOccurrences), .groups = "drop")

# Merge totals back into the original dataset
age_group_gender_sum <- left_join(age_group_gender_sum, age_group_totals, by = "AgeGroup")

# Compute the occurrence rate within each age group
age_group_gender_sum <- age_group_gender_sum %>%
  mutate(OccurrenceRate = (TotalCancerOccurrences / AgeGroupTotal) * 100)

# Plot cancer occurrence rate by gender in each age group using a stacked bar plot
ggplot(age_group_gender_sum, aes(x = AgeGroup, y = OccurrenceRate, fill = Gender)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Cancer Occurrence Rates by Gender within Each Age Group",
    x = "Age Group",
    y = "Percentage of Occurrences within Age Group"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Format y-axis as percentage
  theme(
    plot.title = element_text(family = "Helvetica", size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "bottom"
  )

### 3.2 Visualization - General Analysis (Age Group-wise Relations) ###

# Pivot the dataset to a longer format for visualization
long_sub_seeds <- sub_seeds %>%
  pivot_longer(
    cols = 7:ncol(sub_seeds), 
    names_to = "CancerType", 
    values_to = "Count"
  )

## Age Group and Their Percentages Within Total Cancer ##

# Extract cancer-related data (columns from 7 onward)
cancer_data <- sub_seeds[, 7:ncol(sub_seeds)]

# Replace NA values with 0
cancer_data[is.na(cancer_data)] <- 0

# Summarize total cancer occurrences per age group
age_group_sum <- sub_seeds %>%
  group_by(age_group) %>%
  summarise(TotalCancerOccurrences = sum(across(where(is.numeric), sum, na.rm = TRUE)), .groups = "drop")

# Compute percentage of total cancer occurrences per age group
age_group_sum <- age_group_sum %>%
  mutate(Percentage = (TotalCancerOccurrences / sum(TotalCancerOccurrences)) * 100)

# Plot a pie chart of cancer occurrence by age group
ggplot(age_group_sum, aes(x = "", y = Percentage, fill = age_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_void() +
  labs(fill = "Age Group", title = "Age Group-wise Frequency Based on Total Cancer Counts") +
  theme(
    plot.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.position = "right"
  )

## Average Cancer Counts by Location and Age Group ##

# Filter for county-level data (excluding authorities)
county_data <- sub_seeds %>%
  filter(grepl("county", location, ignore.case = TRUE) & !grepl("authority", location, ignore.case = TRUE))

# Extract cancer-related data
cancer_data <- county_data[, 7:ncol(county_data)]
cancer_data[is.na(cancer_data)] <- 0  # Replace NA values with 0

# Add necessary columns
cancer_data$AgeGroup <- county_data$age_group
cancer_data$Location <- county_data$location

# Summarize total cancer occurrences per age group and location
county_age_group_sum <- cancer_data %>%
  group_by(Location, AgeGroup) %>%
  summarise(TotalCancerOccurrences = sum(across(everything(), sum, na.rm = TRUE)), .groups = "drop")

# Calculate total occurrences for each county
county_totals <- county_age_group_sum %>%
  group_by(Location) %>%
  summarise(CountyTotal = sum(TotalCancerOccurrences), .groups = "drop")

# Merge totals into the dataset
county_age_group_sum <- left_join(county_age_group_sum, county_totals, by = "Location")

# Compute occurrence rate within each county per age group
county_age_group_sum <- county_age_group_sum %>%
  mutate(OccurrenceRate = (TotalCancerOccurrences / CountyTotal) * 100)

# Plot cancer occurrence rate by age group in each county using a line plot
ggplot(county_age_group_sum, aes(x = Location, y = OccurrenceRate, group = AgeGroup, color = AgeGroup)) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Format y-axis as percentage
  labs(title = "Cancer Occurrence Rates by Age Group within Each County",
       x = "County",
       y = "Percentage of Occurrences within County") +
  theme(
    plot.title = element_text(face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"
  )

## age-group wise cancer for each types
ggplot(long_sub_seeds, aes(x = CancerType, y = Count, fill = age_group)) + 
  geom_bar(stat = "summary", fun = "mean", position = "stack") +
  labs(title = "Average Cancer Counts by Type and Age", 
       x = "Cancer Type", 
       y = "Average Count") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "bottom"
  )


### 4. Visualization - Correlation Analysis ###

## Granular Data Correlation Analysis ##
data_clean <- na.omit(sub_seeds[, 7:ncol(sub_seeds)])  # Remove rows with NA values
correlation_matrix <- cor(data_clean)

# Save high-resolution heatmap
png("Desktop/high_res_heatmap_seed.png", width = 5000, height = 2400, res = 240)
corrplot(correlation_matrix, method = "color", order = "hclust", 
         type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")
dev.off()  # Close the device

## NS Overall Correlation Analysis ##
data_clean_NS <- na.omit(data_NS[, 7:ncol(data_NS)])

# Remove columns with zero standard deviation
constant_columns <- sapply(data_clean_NS, function(x) sd(x, na.rm = TRUE) == 0)
data_clean_NS <- data_clean_NS[, !constant_columns]

# Compute correlation matrix
correlation_matrix_NS <- cor(data_clean_NS, method = "pearson", use = "complete.obs")

# Save high-resolution heatmap for NS dataset
png("Desktop/high_res_heatmap_NS.png", width = 7000, height = 5000, res = 240)
corrplot(correlation_matrix_NS, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")
dev.off()  # Close the device


### 6. Visualization - Heatmap Analysis ###

# Reshape dataset by removing zero-count occurrences
long_stone_subset <- long_sub_seeds %>%
  filter(Count != 0)

## County-wise Cancer Occurrence ##

# Compute cancer occurrence percentages per location
cancer_data_percent <- long_stone_subset %>%
  group_by(location, CancerType) %>%
  summarise(count = sum(Count), .groups = "drop") %>%
  group_by(location) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = (count / total) * 100)

# Plot county-wise heatmap
ggplot(cancer_data_percent, aes(x = CancerType, y = location, fill = percentage)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(
    title = "County-wise Cancer Occurrence",
    x = "Cancer Type",
    y = "County",
    fill = "Percentage"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"
  )

## Cancer Type Occurrence by Age Group ##

# Compute cancer occurrence percentages per age group
cancer_data_percent_age <- long_stone_subset %>%
  group_by(CancerType, age_group) %>%
  summarise(count = sum(Count), .groups = "drop") %>%
  group_by(CancerType) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = (count / total) * 100)

# Plot cancer type-wise heatmap by age group
ggplot(cancer_data_percent_age, aes(x = age_group, y = CancerType, fill = percentage)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Cancer Type and Age Group-wise Occurrence",
    x = "Age Group",
    y = "Cancer Type",
    fill = "Percentage"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"
  )

## Pie Chart: Cancer Type-wise Percentage ##

# Extract cancer type columns and replace NA values
cancer_types <- names(sub_seeds)[7:ncol(sub_seeds)]
cancer_data <- sub_seeds[, cancer_types]
cancer_data[is.na(cancer_data)] <- 0

# Summarize total occurrences per cancer type
cancer_type_sum <- colSums(cancer_data)

# Convert to a data frame
cancer_type_df <- data.frame(CancerType = names(cancer_type_sum), TotalOccurrences = cancer_type_sum)

# Compute percentage for each cancer type
total_occurrences <- sum(cancer_type_df$TotalOccurrences)
cancer_type_df <- cancer_type_df %>%
  mutate(Percentage = (TotalOccurrences / total_occurrences) * 100)

# Plot pie chart of cancer type percentages
ggplot(cancer_type_df, aes(x = "", y = Percentage, fill = CancerType)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_void() +
  labs(
    fill = "Cancer Type",
    title = "Percentage of Each Cancer Type within Total Cancer Incidents"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme(
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    legend.position = "right"
  )


### 7. Visualization - Time Series Analysis ###

# Function to calculate the midpoint of a year range
calculate_midpoint <- function(year_range) {
  years <- as.numeric(unlist(strsplit(year_range, "-")))
  mean(years, na.rm = TRUE)
}

# Apply the function to compute midpoint years
cancer_years <- sub_seeds %>%
  mutate(MidpointYear = sapply(Year, calculate_midpoint))

# Extract cancer-related columns (excluding non-numeric ones)
cancer_data <- cancer_years[, 7:ncol(cancer_years)]

# Replace NA values with 0
cancer_data[is.na(cancer_data)] <- 0

# Add gender and midpoint year columns
cancer_data <- cancer_data %>%
  mutate(Gender = cancer_years$gender, MidpointYear = cancer_years$MidpointYear)

# Summarize total cancer occurrences per gender per midpoint year
gender_year_sum <- cancer_data %>%
  group_by(Gender, MidpointYear) %>%
  summarise(TotalCancerOccurrences = sum(across(where(is.numeric), sum, na.rm = TRUE)), .groups = "drop")

# Compute total occurrences per midpoint year
yearly_totals <- gender_year_sum %>%
  group_by(MidpointYear) %>%
  summarise(YearlyTotal = sum(TotalCancerOccurrences), .groups = "drop")

# Merge yearly totals into gender-wise data
gender_year_rate <- left_join(gender_year_sum, yearly_totals, by = "MidpointYear") %>%
  mutate(OccurrenceRate = (TotalCancerOccurrences / YearlyTotal) * 100)

# Plot gender-wise cancer occurrence rate over time
ggplot(gender_year_rate, aes(x = MidpointYear, y = OccurrenceRate, color = Gender, group = Gender)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  scale_x_continuous(breaks = unique(gender_year_rate$MidpointYear)) +  # Ensure all midpoint years are displayed
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Gender-Wise Cancer Occurrence Rate Relative to Yearly Total",
    x = "Midpoint Year",
    y = "Occurrence Rate (%)"
  ) +
  theme(
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"
  )
