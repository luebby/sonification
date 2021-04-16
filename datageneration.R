# Startup
library(sonify)
library(ggformula)
library(gganimate)

# Settings
duration <- 5 # Duration (in sec.) of video
n <- 10000 # Number of points for graph and sound

#############################################
# Data generation for different distributions
Density <- data.frame(x = seq(0,1, length.out = n))
Density$norm <- dnorm(seq(-5,5, length.out = n))
Density$unif <- dunif(seq(-0.1,1.1, length.out = n))
Density$right <- dchisq(seq(0,20, length.out = n), df = 5)
Density$left <- dchisq(seq(20,0, length.out = n), df = 5)
Density$bimodal <- 0.5 * dnorm(seq(-12,6, length.out = n), mean = -5, sd = 2) +
  0.5 * dnorm(seq(-12,6, length.out = n), mean = 2, sd = 1) 

################
# wav generation
soni_norm <- sonify(y = Density$norm, duration = duration, play = FALSE)
writeWave(soni_norm, "norm.wav")

soni_unif <- sonify(y = Density$unif, duration = duration, play = FALSE)
writeWave(soni_unif, "unif.wav")

soni_right <- sonify(y = Density$right, duration = duration, play = FALSE)
writeWave(soni_right, "right.wav")

soni_left <- sonify(y = Density$left, duration = duration, play = FALSE)
writeWave(soni_left, "left.wav")

soni_bimodal <- sonify(y = Density$bimodal, duration = duration, play = FALSE)
writeWave(soni_bimodal, "bimodal.wav")

################
# gif generation
p_norm<- gf_line(norm ~ x , data = Density, size = 2) %>%
  gf_theme(axis.ticks=element_blank(), 
           axis.text=element_blank(),
           panel.background=element_blank(),
           axis.title=element_blank(),
           plot.title = element_text(size=22)) %>%
  gf_labs(title = "Normal distribution")

anim_norm <- p_norm + transition_reveal(along=x)
anim_save("norm.gif", animate(anim_norm, duration = duration))

p_unif<- gf_line(unif ~ x , data = Density, size = 2) %>%
  gf_theme(axis.ticks=element_blank(), 
           axis.text=element_blank(),
           panel.background=element_blank(),
           axis.title=element_blank(),
           plot.title = element_text(size=22)) %>%
  gf_labs(title = "Uniform distribution")

anim_unif <- p_unif + transition_reveal(along=x)
anim_save("unif.gif", animate(anim_unif, duration = duration))

p_right<- gf_line(right ~ x , data = Density, size = 2) %>%
  gf_theme(axis.ticks=element_blank(), 
           axis.text=element_blank(),
           panel.background=element_blank(),
           axis.title=element_blank(),
           plot.title = element_text(size=22)) %>%
  gf_labs(title = "Right skewed")

anim_right <- p_right + transition_reveal(along=x)
anim_save("right.gif", animate(anim_right, duration = duration))

p_left<- gf_line(left ~ x , data = Density, size = 2) %>%
  gf_theme(axis.ticks=element_blank(), 
           axis.text=element_blank(),
           panel.background=element_blank(),
           axis.title=element_blank(),
           plot.title = element_text(size=22)) %>%
  gf_labs(title = "Left skewed")

anim_left <- p_left + transition_reveal(along=x)
anim_save("left.gif", animate(anim_left, duration = duration))

p_bimodal<- gf_line(bimodal ~ x , data = Density, size = 2) %>%
  gf_theme(axis.ticks=element_blank(), 
           axis.text=element_blank(),
           panel.background=element_blank(),
           axis.title=element_blank(),
           plot.title = element_text(size=22)) %>%
  gf_labs(title = "Bi-Modal distribution")

anim_bimodal <- p_bimodal + transition_reveal(along=x)
anim_save("bimodal.gif", animate(anim_bimodal, duration = duration))

##################
# Video generation
## ffmpeg needed https://ffmpeg.org/

# Convert gif to mp4
system("ffmpeg -f gif -i left.gif leftdummy.mp4")
# Combine video (gif) and sound (wav)
system("ffmpeg -i leftdummy.mp4 -i left.wav -c:v copy -c:a aac left.mp4")
