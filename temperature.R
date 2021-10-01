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
p_temp <- gf_line(avg.temperature ~ year , data = Temperature, size = 2,
                  color = ~ avg.temperature) %>%
  gf_refine(scale_color_distiller(palette = "RdBu")) %>%
  gf_theme(panel.background=element_blank()) +
  theme(legend.position = "none") +
  ggtitle("Warming in Germany") +
  labs(caption = "Source: https://opendata.dwd.de/", y="Average temperature °C (5 years)")

# Animation
anim_temp <- p_temp + transition_reveal(along=year)
anim_save("temp.gif", animate(anim_temp, duration = duration, fps = 10))


# Convert gif to mp4
system("ffmpeg -f gif -i temp.gif tempdummy.mp4")
# Combine video (gif) and sound (wav)
system("ffmpeg -i tempdummy.mp4 -i temperature.wav -c:v copy -c:a aac warming.mp4")

# ffmpeg -i warming.mp4 -vcodec libx264 -vf "pad=ceil(iw/2)*2:ceil(ih/2)*2" -pix_fmt yuv420p -strict experimental -r 30 -t 2:20 -acodec aac -vb 1024k -minrate 1024k -maxrate 1024k -bufsize 1024k -ar 44100 -ac 2 out.mp4
