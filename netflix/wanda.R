
## Loading the Libraries

library(pacman)
p_load(tidyverse,lubridate,showtext)
showtext_auto()
font_add_google("Bebas Neue", "Bebas Neue")


## Loading the Dataset
netflix<- read.csv("D:/Downloads/NetflixOriginals.csv")
View(netflix)
head(netflix)
as_tibble(sapply(netflix, class))
netflix <- netflix %>% mutate(Released = mdy(Premiere))
netflix <- netflix %>%
  mutate(Year = year(Released)) %>%
  mutate(Month = month(Released, label=TRUE)) %>%
  mutate(Date = day(Released)) %>%
  mutate(Day = wday(Released, label=TRUE, abbr=FALSE))

## Data Visualisation

### Number of Movies Released Each Year
n <- netflix %>% group_by(Year) %>% summarise(total=n())
n_graph <- ggplot(data=n)+
  geom_col(mapping=aes(
    x=Year,
    y=total,
    fill=ifelse(total==max(total),"red","grey"))
  )+
  labs(title="Netflix Movies released each year")+
  theme_minimal()+
  scale_fill_manual(values=c("#2d2d2d","#E50914"))+
  theme(
    legend.position="none",
    plot.title=element_text(
      family="Bebas Neue",
      size=25,
      color="#E50914"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

n_graph

### Lowest Rated Movies
n8 <- netflix %>% arrange(desc(`IMDB.Score`)) %>% head(5)

n8_graph <- ggplot(data=n8)+
  geom_col(mapping=aes(
    x=reorder(`Title`, -`IMDB.Score`),
    y=`IMDB.Score`,
    fill=ifelse(
      `IMDB.Score`==min(`IMDB.Score`),
      "red","black")))+
  labs(title="Lowest Rated Movies")+
  theme_minimal()+
  scale_fill_manual(values = c("#2d2d2d","#E50914"))+
  coord_flip()+
  theme(
    legend.position="none",
    plot.title = element_text(
      family="Bebas Neue",
      size=25,
      color="#E50914"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major.x=element_blank()
  )

n8_graph


### Movie Runtime
n9_graph <- ggplot(data=netflix)+
  geom_dotplot(
    mapping=aes(x=Runtime),
    binwidth=2.25,
    fill="#2d2d2d",
    color="#e9ecef")+
  labs(title="Movie Runtime")+
  theme_minimal()+
  theme(
    legend.position="none",
    plot.title=element_text(
      family="Bebas Neue",
      size=25,
      color="#E50914"),
    axis.title.x = element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major.x=element_blank()
  )

n9_graph


### Longest Movies
n10 <- netflix %>% arrange(desc(Runtime)) %>% head(5)

n10_graph <- ggplot(data=n10)+
  geom_col(mapping=aes(
    x=reorder(`Title`,`Runtime`),
    y=`Runtime`,
    fill=ifelse(Runtime==max(`Runtime`),"red","black")))+
  labs(title="Longest Movies")+
  theme_minimal()+
  scale_fill_manual(values=c("#2d2d2d","#E50914"))+
  coord_flip()+
  theme(
    legend.position="none",
    plot.title = element_text(
      family="Bebas Neue",
      size=25,
      color="#E50914"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major.x=element_blank()
  )

n10_graph

### Shortest Movies
n11 <- netflix %>% arrange(desc(-Runtime)) %>% head(5)

n11_graph <- ggplot(data=n11)+
  geom_col(mapping=aes(
    x = reorder(`Title`,`Runtime`),
    y = `Runtime`,
    fill = ifelse(Runtime==min(`Runtime`),"red","black")))+
  labs(title="Shortest Movies")+
  theme_minimal()+
  scale_fill_manual(values = c("#2d2d2d","#E50914"))+
  coord_flip()+
  theme(
    legend.position="none",
    plot.title = element_text(
      family="Bebas Neue",
      size=25,
      color="#E50914"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major.x=element_blank(),
    text=element_text(size=20)
  )
n11_graph


### Runtime vs IMDB Rating
n12_graph <- ggplot(data=netflix,aes(x = `IMDB.Score`, y = Runtime))+
  geom_point()+
  geom_smooth(method = "lm", color="#E50914")+
  labs(title="Runtime vs IMDB Rating")+
  theme_minimal()+
  scale_fill_manual(values=c("#2d2d2d","#E50914"))+
  theme(
    legend.position = "none",
    plot.title=element_text(
      family="Bebas Neue",
      size=25,
      color="#E50914"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major.x=element_blank()
  )

n12_graph


summary(netflix)


