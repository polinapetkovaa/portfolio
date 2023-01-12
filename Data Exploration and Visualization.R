#Loading libraries
library(readxl)
library(ggplot2)
library(readr)

# Open the boxofficemojo dataset:
boxofficemojo_com <- read_excel("boxofficemojo.com.xlsx")
View(boxofficemojo_com)


# Quick summary of the data
summary(boxofficemojo_com)


# Open imdb.com.csv and get a summary
imdb_com <- read_csv("imdb.com.csv")
View(imdb_com)
summary(imdb_com)


# Recode string variable into numeric
imdb_com$imdb.com_budget_num <- as.numeric(imdb_com$imdb.com_budget)


# Merge the two dataset
movies <- merge(boxofficemojo_com, imdb_com, by.x = "boxofficemojo.com_imdb.com_id", by.y = "imdb.com_id", all.x = TRUE) 


# Save the dataset:
save.image("Data.RData")


# Visualize the opening revenues of the movies using a boxplot:
boxplot(movies$boxofficemojo.com_openinggross)


#Descriptives and bar plot:
# Simple frequencies:
table(movies$boxofficemojo.com_MPAArating)
# barplot of frequencies:
barplot(table(movies$boxofficemojo.com_MPAArating))
# barplot of percentages instead of frequencies:
barplot(table(movies$boxofficemojo.com_MPAArating)/sum(table(movies$boxofficemojo.com_MPAArating))*100)


#Better visualization
ggplot(movies, aes(boxofficemojo.com_MPAArating))+ geom_bar()
ggplot(movies, aes(boxofficemojo.com_MPAArating)) + geom_bar(aes(y = (..count..)/sum(..count..)*100)) + ylab("percentage")


#Bivariate visualization 
# Create dummy variable (for R-rated movies):
movies$boxofficemojo.com_MPAArating_R <- ifelse(movies$boxofficemojo.com_MPAArating == 'R', 1, 0)
movies$boxofficemojo.com_MPAArating_R[is.na(movies$boxofficemojo.com_MPAArating_R)] <- 0
# Movies that are based on a book vs not based on a book
ggplot(movies, aes(x=as.factor(imdb.com_basedonbook), y=boxofficemojo.com_openinggross)) + geom_boxplot()


##Datasets provided by Arjen van Lin, professor in Tilburg University, Netherlands.