
# setup -------------------------------------------------------------------

# rm(list=ls())

library(tidyverse)
library(lubridate)
library(grid)
PATH <- str_c(getwd(), "/music-visualization")

load(str_c(PATH, "/DATA/", "apple_music_data_20201204.RData"))

# viz ------------------------------- --------------------------------------

sysfonts::font_add_google("Montserrat Bold","mont")
fam_lab <- "mont"
plot_face <- "bold"
replay_type <- theme(text = element_text(family = fam_lab, face = "bold"))
showtext::showtext_auto()

rounding_parameter <- 0.05

plot_color_green <- "#CCF364"
plot_color_pink <- "#E833A1"
plot_color_teal <- "#B9F8F0"
plot_color_red <- "#F97D8C"
plot_color_purple <- "#B09CC7"
plot_color_yellow <- "#F9E94E"

big_number_size <- 64 
title_size <- 18
subtitle_size <- 12
body_size <- 8
caption_size <- 4

margin_controls <- margin(2,1,2,1, unit = "cm")

title_vjust <- 1.5

# plot total minutes ------------------------------------------------------

bg_color <- plot_color_green
text_color <- plot_color_pink

(minutes_2020 <- plays_2020 %>% pull(play_mins) %>% sum() %>% round(0))

plot_total_minutes <- {data.frame(
  x = 1,
  y = 1,
  lab = c("total minutes", scales::comma(minutes_2020), str_c("(",scales::comma(round(minutes_2020/60/52,1)), " hours a week)"))
) %>% 
    ggplot() +
    geom_text(
      aes(x,y, label = lab[2]),
      vjust = -1/6,
      size = big_number_size * 5/14,
      color = text_color,
      family = fam_lab,
      fontface = plot_face
    ) +
    geom_text(
      vjust = 2,
      aes(x,y, label = lab[1]),
      size = title_size * 5/14,
      color = text_color,
      family = fam_lab, 
      fontface = plot_face
    ) +
    geom_text(
      aes(x,y, label = lab[3]),
      vjust = 5,
      size = subtitle_size * 5/14,
      color = text_color,
      family = fam_lab,
      fontface = plot_face
    ) +
    labs(caption = "[2020 wrapped]") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),axis.line = element_blank(),
      axis.text = element_blank(),axis.title = element_blank(),
      
      plot.caption = element_text(color = text_color, size = caption_size, 
                                  family = fam_lab, face = "bold"),
      panel.background = element_rect(colour=bg_color, fill=bg_color,size = 10)
    ) + replay_type}

plot_total_minutes 

{p <- plot_total_minutes +
    theme(plot.background = element_rect(fill = bg_color, color = NA),
          panel.background = element_blank(),
          plot.margin = margin_controls)
  g <- ggplotGrob(p)
  bg <- g$grobs[[1]]
  round_bg <- roundrectGrob(
    x=bg$x, y=bg$y, width=bg$width, height=bg$height,
    r=unit(rounding_parameter, "snpc"),
    just=bg$just, name=bg$name, gp=bg$gp, vp=bg$vp
  )
  g$grobs[[1]] <- round_bg}

ggsave(
  str_c(PATH, "/VIZ/", 'plot_total_minutes','.png'),
  ggplotify::as.ggplot(g),
  height = 4,
  width = 4,
  bg = "transparent"
)

# plot top artists --------------------------------------------------------

top_artists %>% mutate(
  MFD = ifelse(
    str_detect(artist_name, "MF|Vaughn|Madv|Nehru|DANG"),
    1,0
    )
) %>% filter(MFD==1) %>% pull(hours) %>% sum()

c("MF Doom", "NehruvianDoom", "DANGERDOOM", "Victor Vaughn", "Madvillain, Madlib, MF DOOM
", "Mr. Fantastik, MF DOOM")

bg_color <- plot_color_pink
text_color <- plot_color_green

top_artists <- 
  plays_2020 %>% filter(artist_name != "Jack Johnson") %>% 
  mutate(artist_name = ifelse(str_detect(artist_name, "MF|Vaughn|Madv|Nehru|DANG"),
                              "MF Doom", artist_name)) %>% 
  group_by(artist_name) %>% 
  summarise(hours = sum(play_mins, rm.na = T)/60) %>% 
  ungroup() %>% 
  arrange(desc(hours)) %>% 
  head(10)

plot_top_artists <- 
  top_artists %>% 
  mutate(artist_name = fct_relevel(artist_name, top_artists$artist_name %>% rev())) %>% 
  ggplot() +
  geom_segment(
    aes(
      x = artist_name,
      xend = artist_name, 
      y = 0,
      yend = hours
    ),
    stat = 'identity',
    color = text_color
  ) +
  coord_flip() +
  geom_text(
    aes(
      x = artist_name,
      y = hours, 
      label = str_c(format(round(hours,1), nsmall = 1), c(" hrs", rep("", nrow(top_artists)-1)))
    ),
    hjust = -.35,
    size = body_size * 5/14,
    family = fam_lab,
    fontface = plot_face,
    color = text_color
  ) +
  ylim(0,1.4*max(top_artists$hours)) +
  ggtitle("top artists") +
  labs(caption = "[2020 wrapped]") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), axis.line = element_blank(),
    axis.title = element_blank(), axis.text.x = element_blank(),
    
    plot.title = element_text(color = text_color, size = title_size, hjust = -.3, vjust = title_vjust),
    axis.text.y = element_text(color = text_color, size = 8),
    plot.caption = element_text(color = text_color, size = caption_size),
    
    panel.background = element_rect(colour = bg_color, fill = bg_color, size = 8)
  ) + replay_type

plot_top_artists 

{p <- plot_top_artists +
    theme(plot.background = element_rect(fill = bg_color, color = NA),
          panel.background = element_blank(),
          plot.margin = margin_controls)
  g <- ggplotGrob(p) 
  bg <- g$grobs[[1]]
  round_bg <- roundrectGrob(
    x=bg$x, y=bg$y, width=bg$width, height=bg$height, 
    r=unit(rounding_parameter, "snpc"),
    just=bg$just, name=bg$name, gp=bg$gp, vp=bg$vp
  )
  g$grobs[[1]] <- round_bg}

ggsave(
  str_c(PATH, "/VIZ/", 'plot_top_artists','.png'),
  ggplotify::as.ggplot(g),
  height = 4,
  width = 4,
  bg = "transparent"
)


# plot top songs ----------------------------------------------------------

bg_color <- plot_color_purple
text_color <- plot_color_yellow

n_songs <- 6

top_songs <- 
  plays_2020 %>% 
  group_by(artist_name, content_name) %>% 
  summarise(
    minutes = sum(play_mins, rm.na = T), 
    plays = length(play_mins),
    est_complete_plays = (minutes/median(song_length)) %>% round(2)
  ) %>% 
  ungroup() %>% filter(!is.infinite(est_complete_plays)) %>% 
  mutate(
    comb = percent_rank(minutes) + percent_rank(est_complete_plays),
    content_name = ifelse(str_detect(content_name, "Sweepst"), "Sweepstakes", content_name)
    ) %>% 
  arrange(desc(comb)) %>% 
  select(-comb) %>% head(n_songs)

plot_top_songs <-
  top_songs %>% 
  mutate(
    content_name = str_c(1:n_songs,".   ", content_name),
    content_name = fct_relevel(content_name, str_c(1:n_songs,".   ", top_songs$content_name))
  ) %>% 
  ggplot() +
  geom_text(
    aes(
      x = 1,
      y = n_songs:1, 
      label = content_name
    ),
    nudge_y = 0.2,
    hjust = 0,
    size = (body_size+2) * 5/14,
    family = fam_lab,
    fontface = plot_face,
    color = text_color
  ) +
  geom_text(
    aes(
      x = 1,
      y = n_songs:1, 
      label = artist_name
    ),
    nudge_y = -0.2,
    nudge_x = 0.13,
    hjust = 0,
    size = 5 * 5/14,
    family = fam_lab,
    fontface = plot_face,
    color = text_color
  ) +
  xlim(.9,2) +
  ggtitle("top songs") +
  labs(caption = str_c("[2020", 
                       #" \UF8FF",
                       " wrapped]")) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), axis.line = element_blank(),
    axis.title = element_blank(), axis.text = element_blank(),
    
    plot.title = element_text(color = text_color, size = title_size, hjust = .5, vjust = title_vjust),
    plot.caption = element_text(color = text_color, size = caption_size),
    
    panel.background = element_rect(colour = bg_color, fill = bg_color, size = 8)
  ) + replay_type

plot_top_songs 

{p <- plot_top_songs +
    theme(plot.background = element_rect(fill = bg_color, color = NA),
          panel.background = element_blank(),
          plot.margin = margin_controls)
  g <- ggplotGrob(p) 
  bg <- g$grobs[[1]]
  round_bg <- roundrectGrob(
    x=bg$x, y=bg$y, width=bg$width, height=bg$height, 
    r=unit(rounding_parameter, "snpc"),
    just=bg$just, name=bg$name, gp=bg$gp, vp=bg$vp
  )
  g$grobs[[1]] <- round_bg}

ggsave(
  str_c(PATH, "/VIZ/", 'plot_top_songs','.png'),
  ggplotify::as.ggplot(g),
  height = 4,
  width = 4,
  bg = "transparent"
)
 
# top months --------------------------------------------------------------

bg_color <- plot_color_yellow
text_color <- plot_color_purple

mnths <- c("j","f","m","a","m","j","j","a","s","o","n")

hours_by_month <- 
  plays_2020 %>% ungroup() %>% 
  mutate(mo = month(event_start_timestamp),
         wk = week(event_start_timestamp),
         period = mo) %>% 
  group_by(period) %>% 
  summarise(hr = sum(play_mins)/60) %>% 
  ungroup() %>% 
  filter(period != "12") %>% 
  mutate(
    period_char = case_when(
      period == 1 ~ mnths[1],
      period == 2 ~ mnths[2],
      period == 3 ~ mnths[3],
      period == 4 ~ mnths[4],
      period == 5 ~ mnths[5],
      period == 6 ~ mnths[6],
      period == 7 ~ mnths[7],
      period == 8 ~ mnths[8],
      period == 9 ~ mnths[9],
      period == 10 ~ mnths[10],
      period == 11 ~ mnths[11]
    ),
    period = fct_relevel(as_factor(period), as.character(max(period):1))
    
  ) 

plot_months <- 
  hours_by_month %>% 
  ggplot() +
  geom_bar(
    aes(
      x = period,
      y = hr
    ),
    stat = 'identity',
    width = 0.5,
    fill = text_color
  ) +
  geom_text(
    aes(
      x = period,
      y = -5,
      label = period_char
    ),
    size = 5 * 5/14,
    family = fam_lab,
    fontface = plot_face,
    color = text_color
  ) +
  geom_text(
    aes(
      x = period,
      y = hr,
      label = str_c(format(round(hr,0), nsmall = 0), c(" hrs", rep("", nrow(hours_by_month)-1)))
    ),
    hjust = -.3,
    size = 5 * 5/14,
    family = fam_lab,
    fontface = plot_face,
    color = text_color
  ) +
  ylim(-6,115) +
  coord_flip() +
  ggtitle("listening by month") +
  labs(caption = str_c("[2020"," wrapped]")) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), axis.line = element_blank(),
    axis.title = element_blank(), axis.text = element_blank(),
    # axis.text.y = element_text(color = text_color, size = body_size),
    plot.title = element_text(color = text_color, size = title_size, hjust = .5, vjust = title_vjust),
    plot.caption = element_text(color = text_color, size = caption_size),
    
    panel.background = element_rect(colour = bg_color, fill = bg_color, size = 8)
  ) + replay_type
  
plot_months 

{p <- plot_months +
    theme(plot.background = element_rect(fill = bg_color, color = NA),
          panel.background = element_blank(),
          plot.margin = margin_controls)
  g <- ggplotGrob(p) 
  bg <- g$grobs[[1]]
  round_bg <- roundrectGrob(
    x=bg$x, y=bg$y, width=bg$width, height=bg$height, 
    r=unit(rounding_parameter, "snpc"),
    just=bg$just, name=bg$name, gp=bg$gp, vp=bg$vp
  )
  g$grobs[[1]] <- round_bg}

ggsave(
  str_c(PATH, "/VIZ/", 'plot_months','.png'),
  ggplotify::as.ggplot(g),
  height = 4,
  width = 4,
  bg = "transparent"
)




