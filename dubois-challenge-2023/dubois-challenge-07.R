# Load ggplot2 ---------------------------------------------------------------------------------------
library(ggplot2)


# Import and wrangle data ----------------------------------------------------------------------------
data <- readr::read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2023/challenge07/data.csv")

# I used an online protractor (https://www.ginifab.com/feeds/angle_measurement/) to measure the
# angles of the top and bottom wedges and the gaps between them. Each wedge's angle is
# approximately 110° and the gap between them is approximately 70°.
# 110 * 2 + 70 * 2 = 360°

# To convert degrees to radians, we multiply by (pi / 180).
deg_to_rad <- pi / 180

# For `geom_arc_bar`, 0° starts at the "12 o'clock" position and positive angles are measured
# clockwise from there. To have the wedges start at the same angles as the original visualization:
# -55° (90 - (70 / 2)) for the first group and 125° (90 + (70 / 2)) for the second group,
# we need to offset the top wedge by -55° and the bottom one by +125°.
g1_offset <- -55 * deg_to_rad
g2_offset <- 125 * deg_to_rad

df <- data |>
  janitor::clean_names() |>
  dplyr::mutate(occupation = forcats::fct(occupation, levels = c(
    "Agriculture, Fisheries and Mining",
    "Domestic and Personal Service",
    "Manufacturing and Mechanical Industries",
    "Trade and Transportation",
    "Professions"
  ))) |>
  # Create groups to get a cumulative sum for each group later
  dplyr::group_by(group) |>
  dplyr::arrange(occupation, .by_group = TRUE) |>
  dplyr::mutate(
    # Calculate each segment's angle in radians
    # 100% is mapped to 110° so 62%, for example, corresponds to (62 * 110 / 100) degrees
    rad = percentage * 110 / 100 * deg_to_rad,
    # Offset
    offset = dplyr::if_else(group == "Negroes", g1_offset, g2_offset),
    # End angles for each segment
    end = cumsum(rad) + offset,
    # Start angles for each segment
    start = dplyr::lag(end, default = 0) + dplyr::if_else(
      occupation == "Agriculture, Fisheries and Mining",
      offset,
      0
    ),
    # Mid angles for the percentage labels
    mid = (start + end) / 2
  )

rds <- list(
  r = 1, # Outer radius
  rgrp = 1.04, # Radius for the groups' labels (this should be 1.04 * r but I chose r = 1)
  rpct = 0.88 # Radius for the percentage labels
)


# Fonts and colors -----------------------------------------------------------------------------------
f <- list(
  f1 = "Jefferies",
  f2 = "Source Sans Pro"
)

col_bg <- "#DBD0C1"
col_pal1 <- c("#E2324A", "#FEC910", "#5C6EA2", "#D3C4B2", "#BA9578")
# Background colors for the percentage labels
col_pal2 <- c(colorspace::lighten(col_pal1[1:4], 0.5), col_bg)


# Plot -----------------------------------------------------------------------------------------------
plot <- ggplot(df) +
  ggforce::geom_arc_bar(
    aes(x0 = 0, y0 = 0, r0 = 0, r = rds$r, start = start, end = end, fill = occupation),
    size = 0.1,
    key_glyph = "point"
  ) +
  ggrepel::geom_text_repel(
    aes(
      # Remember that `geom_arc_bar` goes in clockwise direction
      # So X-Coordinate = radius * sin(angle) and Y-Coordinate = radius * cos(angle)
      x = rds$rpct * sin(mid),
      y = dplyr::if_else(
        group == "Negroes" & occupation == "Professions",
        rds$rpct * cos(mid) - 0.03,
        rds$rpct * cos(mid)
      ),
      label = stringr::str_c(percentage, "%")
    ),
    family = f$f1,
    size = 12.5 / .pt,
    color = "black",
    bg.color = rep(col_pal2, 2),
    bg.r = 0.2,
    point.size = NA,
    segment.size = NA
  ) +
  geom_text(
    aes(
      x = 0, # rds$rgrp * cos(pi / 2) with cos(pi / 2) = 0
      y = dplyr::if_else(
        group == "Negroes",
        rds$rgrp, # rds$rgrp * sin(pi / 2) with sin(pi / 2) = 1
        -rds$rgrp # rds$rgrp * sin(3 * pi / 2) with sin(3 * pi / 2) = -1
      ),
      label = stringr::str_to_upper(stringr::str_c(group, "."))
    ),
    family = f$f1,
    size = 17 / .pt
  ) +
  scale_fill_manual(
    values = col_pal1,
    labels = ~ dplyr::if_else(
      .x == "Domestic and Personal Service",
      stringr::str_to_upper(stringr::str_wrap(.x, 20)),
      stringr::str_to_upper(stringr::str_wrap(.x, 22))
    )
  ) +
  guides(fill = guide_legend(ncol = 2, override.aes = list(shape = 21, size = 11.5, stroke = 0.2))) +
  labs(
    title = "OCCUPATIONS OF NEGROES AND WHITES IN GEORGIA .",
    caption = "#DuboisChallenge2023 | Graphic: Shad Frigui"
  ) +
  theme_void(base_family = f$f1) +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5, margin = margin(b = 20)),
    plot.caption = element_text(family = f$f2, size = 12, hjust = 0.5, margin = margin(t = 20, b = 10)),
    legend.justification = "left",
    legend.position = c(0, 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 10.5, margin = margin(r = 300)),
    legend.text.align = 0.5,
    legend.spacing.x = unit(c(-0.3, -3.8), "cm"),
    legend.key.height = unit(1.9, "cm"),
    plot.margin = margin(30, 35, 10, 35)
  )

bg_img <- magick::image_read("images/bg-07.jpg") |>
  magick::image_modulate(brightness = 103)

final_plot <- cowplot::ggdraw() +
  cowplot::draw_image(bg_img) +
  cowplot::draw_plot(plot)


# Save plot ------------------------------------------------------------------------------------------
ggsave("dubois-challenge-07.png", final_plot, path = "images", width = 7.5, height = 9.5, dpi = 400)
