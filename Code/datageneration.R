# Libraries
library(sonify)
library(ggformula)
library(gganimate)

# Settings
duration <- 5
n <- 10000

# Data generation for different distributions
Density <- data.frame(x = seq(0,1, length.out = n))
Density$norm <- dnorm(seq(-5,5, length.out = n))
Density$unif <- dunif(seq(-0.1,1.1, length.out = n))
Density$right <- dchisq(seq(0,20, length.out = n), df = 5)
Density$left <- dchisq(seq(20,0, length.out = n), df = 5)
Density$bimodal <- 0.5 * dnorm(seq(-12,6, length.out = n), mean = -5, sd = 2) +
  0.5 * dnorm(seq(-12,6, length.out = n), mean = 2, sd = 1) 

# wav generation
soni_norm <- sonify(y = Density$norm, duration = duration, play = FALSE)
writeWave(soni_norm, "wav/norm.wav")

soni_unif <- sonify(y = Density$unif, duration = duration, play = FALSE)
writeWave(soni_unif, "wav/unif.wav")

soni_right <- sonify(y = Density$right, duration = duration, play = FALSE)
writeWave(soni_right, "wav/right.wav")

soni_left <- sonify(y = Density$left, duration = duration, play = FALSE)
writeWave(soni_left, "wav/left.wav")

soni_bimodal <- sonify(y = Density$bimodal, duration = duration, play = FALSE)
writeWave(soni_bimodal, "wav/bimodal.wav")

# gif generation
p_norm<- gf_line(norm ~ x , data = Density) %>%
  gf_theme(axis.ticks=element_blank(), 
           axis.text=element_blank(),
           panel.background=element_blank(),
           axis.title=element_blank()) %>%
  gf_labs(title = "Normal distribution")

anim_norm <- p_norm + transition_reveal(along=x)
anim_save("wav/norm.gif", animate(anim_norm, duration = duration))

p_unif<- gf_line(unif ~ x , data = Density) %>%
  gf_theme(axis.ticks=element_blank(), 
           axis.text=element_blank(),
           panel.background=element_blank(),
           axis.title=element_blank()) %>%
  gf_labs(title = "Uniform distribution")

anim_unif <- p_unif + transition_reveal(along=x)
anim_save("wav/unif.gif", animate(anim_unif, duration = duration))

p_right<- gf_line(right ~ x , data = Density) %>%
  gf_theme(axis.ticks=element_blank(), 
           axis.text=element_blank(),
           panel.background=element_blank(),
           axis.title=element_blank()) %>%
  gf_labs(title = "Right skewed")

anim_right <- p_right + transition_reveal(along=x)
anim_save("wav/right.gif", animate(anim_right, duration = duration))

p_left<- gf_line(left ~ x , data = Density) %>%
  gf_theme(axis.ticks=element_blank(), 
           axis.text=element_blank(),
           panel.background=element_blank(),
           axis.title=element_blank()) %>%
  gf_labs(title = "Left skewed")

anim_left <- p_left + transition_reveal(along=x)
anim_save("wav/left.gif", animate(anim_left, duration = duration))

p_bimodal<- gf_line(bimodal ~ x , data = Density) %>%
  gf_theme(axis.ticks=element_blank(), 
           axis.text=element_blank(),
           panel.background=element_blank(),
           axis.title=element_blank()) %>%
  gf_labs(title = "Bi-Modal distribution")

anim_bimodal <- p_bimodal + transition_reveal(along=x)
anim_save("wav/bimodal.gif", animate(anim_bimodal, duration = duration))


# setwd("wav/")
# system("ffmpeg -f gif -i bimodal.gif bimodalvid.mp4")
# system("ffmpeg -i bimodalvid.mp4 -i bimodal.wav -c:v copy -c:a aac bimodal.mp4")
