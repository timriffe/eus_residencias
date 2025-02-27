
source("R/00_install_load.R")

Baseline <- read_csv("Data/baseline.csv") %>% 
  rename(sex = sexo,
         age = edad,
         date = fecha, 
         week = semana,
         deaths = def,
         exposure = expos)

BaselineCum <- read_csv("Data/cumulative2020+.csv") %>% 
  rename(sex = sexo,
         age = edad,
         date = fecha, 
         week = semana,
         deaths = def,
         exposure = expos,
         excess = exceso,
         excess_cum = exceso_cum) %>% 
  mutate(deaths_cum = baseline_cum + excess_cum)
# baselines w deaths

bfit1 <-
Baseline %>% 
  filter(prov == "01 Araba/Álava") %>% 
  ggplot(aes(x = date, y = deaths, color = sex, shape = sex)) +
  geom_point(size = .3, alpha = .3) +
  geom_line(inherit.aes = FALSE, aes(x = date, y = baseline, color = sex)) +
  facet_grid(rows=vars(rev(age)), scales = "free") + 
  labs(y = "defunciones", x = "")+
  scale_colour_manual(values = c("blue", "red"))

bfit2 <-
Baseline %>% 
  filter(prov == "20 Gipuzkoa") %>% 
  ggplot(aes(x = date, y = deaths, color = sex, shape = sex)) +
  geom_point(size = .3, alpha = .3) +
  geom_line(inherit.aes = FALSE, aes(x = date, y = baseline, color = sex)) +
  facet_grid(rows=vars(rev(age)), scales = "free")+ 
  labs(y = "", x = "")+
  scale_colour_manual(values = c("blue", "red"))

bfit3 <-
Baseline %>% 
  filter(prov == "48 Bizkaia") %>% 
  ggplot(aes(x = date, y = deaths, color = sex, shape = sex)) +
  geom_point(size = .3, alpha = .3) +
  geom_line(inherit.aes = FALSE, aes(x = date, y = baseline, color = sex)) +
  facet_grid(rows=vars(rev(age)), scales = "free") + 
  labs(y = "", x = "") +
  scale_colour_manual(values = c("blue", "red"))
  
bfit_panel <-
  plot_grid(plotlist=list(bfit1+ theme(legend.position="none"), 
                          bfit2+ theme(legend.position="none"), 
                          bfit3+ theme(legend.position="none")), ncol = 3,
            labels = c("Araba","Gipuzkoa","Bizkaia"))
bfit_legend <- get_legend(
  # create some space to the left of the legend
  bfit1 + theme(legend.box.margin = margin(0, 0, 0, 12))
)
bfit_panel_out <- plot_grid(bfit_panel, bfit_legend, rel_widths = c(3, .4))
ggsave(filename = "Figs/baselines_def.png",
       bfit_panel_out,
       width = 30, height = 20, units = "cm")


# same, but as rates:
# baselines w deaths

cumul1 <-
BaselineCum %>% 
  filter(prov == "01 Araba/Álava") %>% 
  ggplot(aes(x = date,y = deaths_cum, color = sex)) + 
  geom_line() +
  geom_line(inherit.aes = FALSE, mapping = aes(x = date, y = baseline_cum, color = sex), lty = 4) +
  facet_grid(rows = vars(age), scales = "free")+ 
  #theme(legend.position = "none") +
  labs(y = "defunciones cumulativas", x = "") +
  scale_colour_manual(values = c("blue", "red"))
cumul2<-
BaselineCum %>% 
  filter(prov == "20 Gipuzkoa") %>% 
  ggplot(aes(x = date,y = deaths_cum, color = sex)) + 
  geom_line() +
  geom_line(inherit.aes = FALSE, mapping = aes(x = date, y = baseline_cum, color = sex), lty = 4) +
  facet_grid(rows = vars(age), scales = "free")+ 
  labs(y = "", x = "") +
 #theme(legend.position = "none") +
  scale_colour_manual(values = c("blue", "red"))
cumul3<-
BaselineCum %>% 
  filter(prov == "48 Bizkaia") %>% 
  ggplot(aes(x = date,y = deaths_cum, color = sex)) + 
  geom_line() +
  geom_line(inherit.aes = FALSE, mapping = aes(x = date, y = baseline_cum, color = sex), lty = 4) +
  facet_grid(rows = vars(age), scales = "free")+ 
  labs(y = "", x = "") +
  scale_colour_manual(values = c("blue", "red"))

cumul_panel <-
  plot_grid(plotlist=list(cumul1+ theme(legend.position="none"), 
                          cumul2+ theme(legend.position="none"), 
                          cumul3+ theme(legend.position="none")), ncol = 3,
            labels = c("Araba","Gipuzkoa","Bizkaia"))
cumul_legend <- get_legend(
  # create some space to the left of the legend
  cumul1 + theme(legend.box.margin = margin(0, 0, 0, 12))
)
cumul_panel_out <- plot_grid(cumul_panel, cumul_legend, rel_widths = c(3, .4))
ggsave(filename = "Figs/cumulativas.png",
       cumul_panel_out,
       width = 30, height = 20, units = "cm")

# cumulative excess
cum_ex1 <- 
BaselineCum %>% 
  filter(prov == "01 Araba/Álava") %>% 
  ggplot(aes(x = date,y = excess_cum, color = sex)) + 
  geom_line() +
  geom_hline(yintercept = 0, size = .5,alpha = .5) +
  facet_grid(rows = vars(age), scales = "free")  + 
  labs(y = "exceso cumulativo", x = "") +
  scale_colour_manual(values = c("blue", "red"))
cum_ex2 <- 
BaselineCum %>% 
  filter(prov == "20 Gipuzkoa") %>% 
  ggplot(aes(x = date,y = excess_cum, color = sex)) + 
  geom_line() +
  geom_hline(yintercept = 0, size = .5,alpha = .5) +
  facet_grid(rows = vars(age), scales = "free") + 
  labs(y = "", x = "") +
  scale_colour_manual(values = c("blue", "red"))
cum_ex3 <- 
BaselineCum %>% 
  filter(prov == "48 Bizkaia") %>% 
  ggplot(aes(x = date,y = excess_cum, color = sex)) + 
  geom_line() +
  geom_hline(yintercept = 0, size = .5,alpha = .5) +
  facet_grid(rows = vars(age), scales = "free") + 
  labs(y = "", x = "") +
  scale_colour_manual(values = c("blue", "red"))

cum_ex_panel <-
  plot_grid(plotlist=list(cum_ex1+ theme(legend.position="none"), 
                          cum_ex2+ theme(legend.position="none"), 
                          cum_ex3+ theme(legend.position="none")), ncol = 3,
            labels = c("Araba","Gipuzkoa","Bizkaia"))
cum_ex_legend <- get_legend(
  # create some space to the left of the legend
  cum_ex1 + theme(legend.box.margin = margin(0, 0, 0, 12))
)
cum_ex_panel_out <- plot_grid(cum_ex_panel, cum_ex_legend, rel_widths = c(3, .4))
ggsave(filename = "Figs/exceso_cumulativo.png",
       cum_ex_panel_out,
       width = 30, height = 20, units = "cm")




# excess, very noisy
ex1 <- 
BaselineCum %>% 
  filter(prov == "01 Araba/Álava") %>% 
  ggplot(aes(x = date,y = excess, color = sex)) + 
  geom_line() +
  geom_hline(yintercept = 0, size = .5,alpha = .5) +
  facet_grid(rows = vars(age), scales = "free") + 
  labs(y = "exceso defunciones", x = "") +
  scale_colour_manual(values = c("blue", "red"))
ex2 <- 
BaselineCum %>% 
  filter(prov == "20 Gipuzkoa") %>% 
  ggplot(aes(x = date,y = excess, color = sex)) + 
  geom_line() +
  geom_hline(yintercept = 0, size = .5,alpha = .5) +
  facet_grid(rows = vars(age), scales = "free") + 
  labs(y = "", x = "") +
  scale_colour_manual(values = c("blue", "red"))
ex3 <- 
BaselineCum %>% 
  filter(prov == "48 Bizkaia") %>% 
  ggplot(aes(x = date,y = excess, color = sex)) + 
  geom_line() +
  geom_hline(yintercept = 0, size = .5,alpha = .5) +
  facet_grid(rows = vars(age), scales = "free") + 
  labs(y = "", x = "") +
  scale_colour_manual(values = c("blue", "red"))

ex_panel <-
  plot_grid(plotlist=list(ex1 + theme(legend.position="none"), 
                          ex2 + theme(legend.position="none"), 
                          ex3 + theme(legend.position="none")), ncol = 3,
            labels = c("Araba","Gipuzkoa","Bizkaia"))
ex_legend <- get_legend(
  # create some space to the left of the legend
  ex1 + theme(legend.box.margin = margin(0, 0, 0, 12))
)
ex_panel_out <- plot_grid(ex_panel, ex_legend, rel_widths = c(3, .4))
ggsave(filename = "Figs/exceso.png",
       ex_panel_out,
       width = 30, height = 20, units = "cm")


# No age
Totals <-
  BaselineCum %>% 
  group_by(prov, date, sex) %>% 
  summarize(deaths = sum(deaths),
            baseline = sum(baseline),
            excess = sum(excess),
            deaths_cum = sum(deaths_cum),
            baseline_cum = sum(baseline_cum),
            excess_cum = sum(excess_cum),
            .groups = "drop")
Tot_out<-
Totals %>% 
  ggplot(aes(x = date, y = deaths, color = sex)) +
  geom_line() +
  geom_line(inherit.aes = FALSE, mapping = aes(x = date, y = baseline, color = sex), lty = 4) +
  facet_grid(rows = vars(prov), scales = "free_y")+
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = "defunciones totals y baseline",x="")

ggsave("Figs/def_total_2020+.png",Tot_out,
       width = 15, height = 20, units = "cm")
ex_tot_cum_out <-
Totals %>% 
  ggplot(aes(x = date, y = excess_cum, color = sex)) +
  geom_line() +
  facet_grid(rows = vars(prov), scales = "free_y") +
  geom_hline(yintercept = 0, size = .5,alpha = .5) +
  scale_colour_manual(values = c("blue", "red"))+
  labs(y = "exceso total cumulativo",x="")

ggsave("Figs/exceso_total_cumul.png",ex_tot_cum_out,
       width = 15, height = 20, units = "cm")

ex_tot_out <-
  Totals %>% 
  ggplot(aes(x = date, y = excess, color = sex)) +
  geom_line() +
  facet_grid(rows = vars(prov), scales = "free_y") +
  geom_hline(yintercept = 0, size = .5,alpha = .5) +
  scale_colour_manual(values = c("blue", "red"))+
  labs(y = "exceso total",x="")

ggsave("Figs/exceso_total.png",ex_tot_out,
       width = 15, height = 20, units = "cm")

# -------------------------------------- #
