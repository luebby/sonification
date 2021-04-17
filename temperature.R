# Startup
library(sonify)
library(ggformula)
library(gganimate)
library(tidyverse)
library(zoo)

# Fetch data
url <- "https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/annual/air_temperature_mean/regional_averages_tm_year.txt"
daten <- read.csv2(url, dec = ".", skip=1)

# Data preporcessing, rolling window of 5 years
Temperature <- daten %>%
  select(Jahr, Deutschland) %>%
  rename(year = Jahr, temperatur = Deutschland) %>%
  mutate(avg.temperature = rollmean(temperatur, k=5, fill=NA))

# Duration in sec.
duration <- round(nrow(Temperature)/10)

# Sonify
soni_temp <- sonify(y = Temperature$avg.temperature, 
                    duration = duration, play = FALSE,
                    flim = c(220,880))
writeWave(soni_temp, "temperature.wav")

# Plotting
p_temp <- gf_line(avg.temperature ~ year , data = Temperature, size = 2) %>%
  gf_theme(panel.background=element_blank()) +
  ggtitle("Warming in Germany") +
  labs(caption = "Source: https://opendata.dwd.de/", y="Average temperature °C (5 years)")

# Anomation
anim_temp <- p_temp + transition_reveal(along=year)
anim_save("temp.gif", animate(anim_temp, duration = duration, fps = 10))


# Convert gif to mp4
system("ffmpeg -f gif -i temp.gif tempdummy.mp4")
# Combine video (gif) and sound (wav)
system("ffmpeg -i tempdummy.mp4 -i temperature.wav -c:v copy -c:a aac warming.mp4")
