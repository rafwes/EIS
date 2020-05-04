rm(list=ls())

library(tidyverse)
library(plotrix)
library(grid)
library(zoo)
#library(reshape2)
#library(ISOweek)
#library(lubridate)
#library(EnvStats)

#base_path <- "/xdisk/agalvao/mig2020/extra/agalvao/eis_nielsen/rafael"
base_path <- "/home/rafael/Sync/IMPA/2020.0/simulations/code"

monthly_file <- file.path(base_path,"csv_output/estimation_data_monthly.csv")
weekly_1w_file <- file.path(base_path,"csv_output/estimation_data_weekly_1w.csv")
weekly_4w_file <- file.path(base_path,"csv_output/estimation_data_weekly_4w.csv")

column_classes <- c("numeric", "Date", rep("numeric", 7))

monthly_data <- read.csv(monthly_file, colClasses = column_classes)
weekly_1w_data <- read.csv(weekly_1w_file, colClasses = column_classes)
weekly_4w_data <- read.csv(weekly_4w_file, colClasses = column_classes)

rm(monthly_file,
   weekly_1w_file,
   weekly_4w_file)

plot_data <- 
  monthly_data %>%
  group_by(DATE) %>%
  summarise_all(mean) %>%
  ungroup() %>%
  transmute(DATE = DATE,
            Y = scale(rollapply(Y,
                                3,
                                mean,
                                partial = TRUE,
                                align = "center")),
            X_TB = scale(rollapply(X_TB,
                                   3,
                                   mean,
                                   partial = TRUE,
                                   align = "center")),
            X_ST = scale(rollapply(X_ST,
                                   3,
                                   mean,
                                   partial = TRUE,
                                   align = "center")))

                  
plot_2005 <- 
  ggplot() + 
  geom_line(data = plot_data %>% 
              select(DATE,Y,X_TB) %>%
              filter(between(DATE,
                             as.Date("2005-06-01"), 
                             as.Date("2007-06-01"))) %>% 
              pivot_longer(-DATE),
            aes(x = DATE, 
                y = value, 
                colour = name)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text())

plot_2007 <- 
  ggplot() + 
  geom_line(data = plot_data %>% 
              select(DATE,Y,X_TB) %>%
              filter(between(DATE,
                             as.Date("2007-06-01"), 
                             as.Date("2009-06-01"))) %>% 
              pivot_longer(-DATE),
            aes(x = DATE, 
                y = value, 
                colour = name)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text())

plot_2009 <- 
  ggplot() + 
  geom_line(data = plot_data %>% 
              select(DATE,Y,X_TB) %>%
              filter(between(DATE,
                             as.Date("2009-06-01"), 
                             as.Date("2011-06-01"))) %>% 
              pivot_longer(-DATE),
            aes(x = DATE, 
                y = value, 
                colour = name)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text())

plot_2011 <- 
  ggplot() + 
  geom_line(data = plot_data %>% 
              select(DATE,Y,X_TB) %>%
              filter(between(DATE,
                             as.Date("2011-06-01"), 
                             as.Date("2013-06-01"))) %>% 
              pivot_longer(-DATE),
            aes(x = DATE, 
                y = value, 
                colour = name)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text())

plot_2013 <- 
  ggplot() + 
  geom_line(data = plot_data %>% 
              select(DATE,Y,X_TB) %>%
              filter(between(DATE,
                             as.Date("2013-06-01"), 
                             as.Date("2015-06-01"))) %>% 
              pivot_longer(-DATE),
            aes(x = DATE, 
                y = value, 
                colour = name)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text())


grid.newpage()
grid.draw(rbind(ggplotGrob(plot_2005), 
                ggplotGrob(plot_2007),
                ggplotGrob(plot_2009), 
                ggplotGrob(plot_2011),
                ggplotGrob(plot_2013),
                size = "last"))


FALSE

if (FALSE) {


ggplot() + 
  geom_line(data = long_curve,
            aes(x = DATE, 
                y = value, 
                colour = variable)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c(RATE_TB_REAL_MW = "gray", 
                                RATE_TB_REAL_MW_SMOOTH = "darkgreen")) + 
  geom_point(data = long_points,
             aes(x = DATE,
                 y = value,
                 color = variable))


long_curve_cut <- long_curve %>% 
  filter(between(DATE,
                 as.Date("2008-01-01"), 
                 as.Date("2010-01-01")))


long_points_cut <- long_points %>% 
  filter(between(DATE,
                 as.Date("2008-01-01"), 
                 as.Date("2010-01-01")))

ggplot() + 
  geom_line(data = long_curve_cut,
            aes(x = DATE, 
                y = value, 
                colour = variable)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c(RATE_TB_REAL_MW = "gray", 
                                RATE_TB_REAL_MW_SMOOTH = "darkgreen")) + 
  geom_point(data = long_points_cut,
             aes(x = DATE,
                 y = value,
                 color = variable))

rm(long_curve,long_curve_cut,long_points,long_points_cut)

}
