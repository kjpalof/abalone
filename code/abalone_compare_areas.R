#K.Palof 
#3-20-17

# comparing length frequency distributions for Sheppard and others - Gravina and M

# is it valid to use age-length key from Sheppard on these other areas?

#load data from 'Abalone_length_R_KJP.R'

#Sheppard legnth distribution
head(shpab)
shpab %>% mutate(area = rep("shp", nrow(shpab))) -> shpab
#save this data for R markdown

#histogram of legnths (len)
ggplot(shpab, aes(x=len)) + 
  geom_histogram(binwidth = 1.0, alpha = 0.5, position = "identity") 

ggplot(shpab, aes(x=len)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=1,
                 colour="black", fill="white") +
  geom_density(alpha=.3, fill="#FF6666")  # Overlay with transparent density plot
  
#MearesPass legnth distribution
head(mrsab)

mrsab %>% mutate(area = rep("mrs", nrow(mrsab))) -> mrsab

                 
#histogram of legnths (len)
ggplot(mrsab, aes(x=len)) + 
  geom_histogram(binwidth = 1.0, alpha = 0.5, position = "identity") 

ggplot(mrsab, aes(x=len)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=1,
                 colour="black", fill="white") +
  geom_density(alpha=.3, fill="#FF6666")  # Overlay with transparent density plot

#Gravina legnth distribution
head(grvab)

grvab %>% mutate(area = rep("grv", nrow(grvab))) -> grvab


#histogram of legnths (len)
ggplot(grvab, aes(x=len)) + 
  geom_histogram(binwidth = 1.0, alpha = 0.5, position = "identity") 

ggplot(grvab, aes(x=len)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=1,
                 colour="black", fill="white") +
  geom_density(alpha=.3, fill="#FF6666")  # Overlay with transparent density plot

#KS test between Sheppard and Gravina
ks.test(shpab$len, grvab$len)
ks.test(shpab$len, mrsab$len)

shpab %>% bind_rows(grvab) ->allab
allab %>% bind_rows(mrsab) -> allab2
head(allab)

ggplot(allab, aes(x=len, fill = area)) + 
  geom_histogram(binwidth = 1.0, alpha = 0.5, position = "identity") 

ggplot(allab, aes(x=len, colour = area)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=1,
                 colour="black", fill="white") +
  geom_density(alpha=.3, fill = "white")  # Overlay with transparent density plot


ggplot(allab2, aes(x=len, fill = area)) + 
  geom_histogram(binwidth = 1.0, alpha = 0.5, position = "identity") 

ggplot(allab2, aes(x=len, colour = area)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=1, 
                 colour="black", fill="white") +
  geom_density(alpha=.3, fill = "white")  # Overlay with transparent density plot

ggplot(allab2, aes(x=len, fill= area)) + geom_density(alpha =.3)



####  comparison of age - length relationship  ----------------
