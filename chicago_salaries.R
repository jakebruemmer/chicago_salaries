# Analyzing City of Chicago publicly available data
# Source: https://data.cityofchicago.org
# Authors: Jake Bruemmer

library(dplyr)
library(ggplot2)
library(scales)

options("scipen" = 10)
# Download the data and rename the columns to snake case
salary_data <- read.csv("data/chicago_employee_salary_data.csv", head = TRUE, sep = ",", quote = "\"")
colnames(salary_data) <- c("name", "position_title", "department", "employee_annual_salary")
salary_data$employee_annual_salary <- as.numeric(sub("\\$", "", salary_data$employee_annual_salary))

department_information <- salary_data %>%
  group_by(department) %>%
  summarise(num_records = n(), 
            avg_salary = mean(employee_annual_salary),
            median_salary = median(employee_annual_salary),
            max_salary = max(employee_annual_salary),
            min_salary = min(employee_annual_salary)) %>%
  ungroup() %>%
  select(department, num_records, avg_salary, median_salary, max_salary, min_salary) %>%
  filter(!is.na(department))

# Summary statistics about the departments
department_information <- transform(department_information, 
                                    department = reorder(department, num_records))

department_bar_chart <- ggplot(department_information, aes(x = department, y = num_records)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Chicago Public Salary Records by Department") + 
  xlab("Department") +
  ylab("Number of Records in Deparment") +
  coord_flip()

# Average salary by department
department_information <- transform(department_information,
                                    department = reorder(department, avg_salary))

department_avg_salary_bar_chart <- ggplot(department_information, aes(x = department, y = avg_salary)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Chicago Public Salary Records by Department") + 
  xlab("Department") +
  ylab("Average Salary by Department") +
  scale_y_continuous(labels = comma) +
  coord_flip()

# Maximum salary by department
department_information <- transform(department_information,
                                    department = reorder(department, max_salary)) 

department_max_salary_bar_chart <- ggplot(department_information, aes(x = department, y = max_salary)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Chicago Public Salary Records by Department") + 
  xlab("Department") +
  ylab("Maximum Salary by Department") +
  scale_y_continuous(labels = comma) + 
  coord_flip()

# Minimum salary by department
department_information <- transform(department_information,
                                    department = reorder(department, max_salary)) 

department_min_salary_bar_chart <- ggplot(department_information, aes(x = department, y = min_salary)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Chicago Public Salary Records by Department") + 
  xlab("Department") +
  ylab("Minimum Salary by Department") +
  scale_y_continuous(labels = comma) +
  coord_flip()

max_salary <- sort(salary_data$employee_annual_salary, decreasing = TRUE)[[1]]
