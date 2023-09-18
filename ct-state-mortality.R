#Adith Arun

library(tidyverse)
ct_mort <- "CTmortality_2005_2022.csv" %>% read_csv()

ct_mort_subset <- ct_mort %>% select("DTHYR", "SEX", "AGEYRS_calc", "RACE6_ETH") %>% filter(RACE6_ETH <= 2)

ct_pop <- "A1SRH_state_icen0009_pcen1019.xlsx" %>% readxl::read_excel()

ct_pop_subset <- ct_pop %>% select(year, age, sex_name, race_name, hisp_name, pop) %>% filter(hisp_name == "Non-Hisp") %>% filter(race_name %in% c("White", "Black"))


ct_dths <- ct_mort_subset %>% mutate(dthstatus = 1) %>% group_by(AGEYRS_calc, SEX, RACE6_ETH, DTHYR) %>% summarize(net_deaths = sum(dthstatus)) %>% ungroup() %>% filter(AGEYRS_calc >= 0) %>% filter(DTHYR <= 2019)

#For all of CT: (2005 - 2019) (pop goes from 2000 - 2019 easy and death goes 2005 - 2022) so the intersection is 2005 - 2019

ct_dths <- ct_dths %>% mutate(sex = ifelse(SEX == 1, "M", ifelse(SEX == 2, "F", NA))) %>% mutate(race = ifelse(RACE6_ETH == 1, "White", ifelse(RACE6_ETH == 2, "Black", NA)))  %>% filter(!is.na(sex)) %>% filter(!is.na(race)) %>% mutate(age = AGEYRS_calc) %>% select(-c(AGEYRS_calc, SEX, RACE6_ETH)) %>% filter(age < 200)

ct_denom <- ct_pop_subset %>% mutate(sex = sex_name, race = race_name) %>% select(year, age, sex, pop, race) %>% filter(year <= 2022)


age_bucket <- function(age) {
  if (age < 85) {
    lower <- 5 * (age %/% 5) # Integer division
    upper <- lower + 4
    return(paste0(lower, "-", upper))
  } else {
    return("85+")
  }
}

ct_dths <- ct_dths %>% mutate(age_bkt = sapply(age, age_bucket)) %>% select(-age)

ct_denom <- ct_denom %>% mutate(age_bkt = sapply(age, age_bucket)) %>% select(-age)

ct_dths <- ct_dths %>% group_by(DTHYR, sex, race, age_bkt) %>% summarize(net_deaths = sum(net_deaths)) %>% ungroup()

ct_denom <- ct_denom %>% group_by(year, sex, race, age_bkt) %>% summarize(pop = sum(pop)) %>% ungroup()

master <- expand.grid(ages = ct_denom %>% pull(age_bkt) %>% unique(), years = ct_dths$DTHYR %>% unique(), sex = c("M", "F"), race = c("White", "Black")) %>% as_tibble() %>% mutate(sex = as.character(sex), ages = as.character(ages), race = as.character(race))

df <- master %>% left_join(ct_dths, by=c("ages"="age_bkt", "years"="DTHYR", "sex"="sex", "race"="race")) %>% left_join(ct_denom, by=c("ages"="age_bkt", "sex"="sex", "race"="race", "years"="year"))

df <- df %>% mutate(net_deaths = ifelse(is.na(net_deaths), 0, net_deaths)) 

age_fracs <- ct_denom %>% filter(year == 2000) %>% group_by(year, sex) %>% mutate(n = sum(pop)) %>% ungroup() %>% mutate(frac = pop / n) %>% select(sex, age_bkt, frac)



df <- df %>% mutate(crude_rate = net_deaths / pop)


df <- df %>% left_join(age_fracs, by=c("ages"="age_bkt", "sex"="sex"), relationship = "many-to-many")

#aadr by year 
aadr <- df %>% mutate(tmp = crude_rate * frac *10^5) %>% group_by(years, sex, race) %>% summarize(aadr = sum(tmp)) %>% ungroup()


#plot aadr 

cbb <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

panel_theme <- theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor=element_blank())

sizing_theme <- theme(axis.text = element_text(size=16), axis.title=element_text(size=20), legend.text=element_text(size=15), legend.title=element_text(size=0), plot.title=element_text(size=22, hjust=0.5)) 


(aadr %>% mutate(sex = ifelse(sex == "F", "Female", "Male")) %>% ggplot(aes(x=years, y=aadr, color=race, shape=sex, linetype=sex)) + geom_line(size=0.75) + geom_point(size=2.25) + scale_y_continuous(breaks=scales::pretty_breaks(n=8)) + panel_theme + sizing_theme +  ylab("Age-Adjusted Mortality Rate per 100K Individuals") + ggtitle("CT State Age-Adjusted Mortality Rates") + scale_color_manual(values=cbb[1:2]) + scale_x_continuous(breaks=aadr$years %>% unique(), labels= function(x) ifelse(x %% 2 == 1, x, "")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("") ) %>% ggsave("~/Downloads/CT-mortality/aadr-by-race.pdf", plot = ., device = cairo_pdf, units = "in", height = 9, width = 12)




# excess aadr by year and MR ratio
excess_aadr <- aadr %>% group_by(years, sex) %>% summarize(excess_aadr = aadr[race == "Black"] - aadr[race == "White"], ratio_aadr = aadr[race == "Black"] / aadr[race == "White"]) %>% ungroup()

(excess_aadr %>% mutate(sex = ifelse(sex == "F", "Female", "Male")) %>% ggplot(aes(x=years, y=excess_aadr, color=sex)) + geom_line(size=0.75) + geom_point(size=2.25) + scale_y_continuous(breaks=scales::pretty_breaks(n=8)) + panel_theme + sizing_theme +  ylab("Excess Age-Adjusted Mortality Rate per 100K Individuals") + ggtitle("CT State Excess Age-Adjusted Mortality Rates") + scale_color_manual(values=cbb[1:2]) + scale_x_continuous(breaks=excess_aadr$years %>% unique(), labels= function(x) ifelse(x %% 2 == 1, x, "")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("") ) %>% ggsave("~/Downloads/CT-mortality/excess-aadr-by-year.pdf", plot = ., device = cairo_pdf, units = "in", height = 11, width = 12)

(excess_aadr %>% mutate(sex = ifelse(sex == "F", "Female", "Male")) %>% ggplot(aes(x=years, y=ratio_aadr, color=sex)) + geom_line(size=0.75) + geom_point(size=2.25) + scale_y_continuous(breaks=scales::pretty_breaks(n=8)) + panel_theme + sizing_theme +  ylab("Age-Adjusted Mortality Rate Ratio") + ggtitle("CT State Age-Adjusted Mortality Rate Ratio") + scale_color_manual(values=cbb[1:2]) + scale_x_continuous(breaks=excess_aadr$years %>% unique(), labels= function(x) ifelse(x %% 2 == 1, x, "")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("") ) %>% ggsave("~/Downloads/CT-mortality/aadr-ratio-by-year.pdf", plot = ., device = cairo_pdf, units = "in", height = 11, width = 12)


#Mortality rate by age 
mr <- df %>% group_by(ages, sex, race) %>% summarize(nd = sum(net_deaths), np = sum(pop)) %>% mutate(cr = (nd / np)*10^5) %>% ungroup()


#and excess MR by age
excess_age_mr <- mr %>% group_by(ages, sex) %>% summarize(mrr = cr[race=="Black"] / cr[race == "White"], excess_mr = cr[race == "Black"] - cr[race == 'White']) %>% ungroup()


agevector <- excess_age_mr %>% select(ages) %>% distinct() %>% mutate(cat = sub("[-+].*", "", ages)) %>% mutate(cat = as.numeric(cat)) %>% arrange(cat) %>% filter(ages != "85+")


(excess_age_mr %>% mutate(cat = as.numeric(sub("[-+].*", "", ages))) %>% filter(ages != "85+") %>% mutate(sex = ifelse(sex == "F", "Female", "Male")) %>% ggplot(aes(x=cat, y=excess_mr, group=sex, color=sex)) + geom_line(size=1.25) + geom_hline(yintercept=1, linetype="dashed") + scale_color_manual(values=c("maroon", "navy")) + panel_theme + sizing_theme + scale_x_continuous(limits=c(min(agevector$cat),max(agevector$cat)), breaks=agevector$cat, labels=agevector$ages) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(aes(color=sex), size=2.75) + scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +  theme(plot.title = element_text(hjust = 0.5)) + ylab("Excess Mortality Rate per 100K Individuals") + ggtitle("CT State Excess Mortality Rate By Age (2005-2019)") ) %>% ggsave("~/Downloads/CT-mortality/excess-mortality-by-age.pdf", plot = ., device = cairo_pdf, units = "in", height = 11, width = 12)


#Mortality rate by age and year and excess MR by age and year
mr_yr <- df %>% mutate(yr_bkt = ifelse(years <= 2009, "2005-2009", ifelse(years <= 2014, "2010-2014", "2015-2019"))) %>% group_by(ages, sex, race, yr_bkt) %>% summarize(nd = sum(net_deaths), np = sum(pop)) %>% mutate(cr = (nd / np)*10^5)

excess_mr_yr <- mr_yr %>% ungroup() %>% group_by(ages, sex, yr_bkt) %>% summarize(excess_mr = cr[race == "Black"] - cr[race == "White"])


(excess_mr_yr %>% ungroup() %>% mutate(cat = as.numeric(sub("[-+].*", "", ages))) %>% filter(ages != "85+") %>% mutate(sex = ifelse(sex == "F", "Female", "Male")) %>% ggplot(aes(x=cat, y=excess_mr, group=yr_bkt, color=yr_bkt)) + facet_wrap(~sex, scales = "free", nrow = 1) + geom_line(size=1.25) + geom_hline(yintercept=1, linetype="dashed") + scale_color_manual(values=cbb[1:3]) + panel_theme + sizing_theme + scale_x_continuous(limits=c(min(agevector$cat),max(agevector$cat)), breaks=agevector$cat, labels=agevector$ages) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(aes(color=yr_bkt), size=2.75) + scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +  theme(plot.title = element_text(hjust = 0.5)) + ylab("Excess Mortality Rate per 100K Individuals") + ggtitle("CT State Excess Mortality Rate By Age and Year") + theme(strip.text = element_text(size = 16))) %>% ggsave("~/Downloads/CT-mortality/excess-mortality-by-age-year.pdf", plot = ., device = cairo_pdf, units = "in", height = 11, width = 15)



#Life expectancy file
#Not split for race and gender for CT state, but we can substitute in National data for this
lifeexp <- "CT_B.XLSX" %>% readxl::read_excel() %>% select(1, 5, 7, 11) %>% magrittr::set_colnames(c("id", "age_group", "pop", "life_exp")) %>% mutate(weight = pop * life_exp) %>% group_by(age_group) %>% summarize(nw = sum(weight) / sum(pop))

lifeexp$age_bkt <- c("1-4", "15-24", "25-34", "35-44", "45-54", "5-14", "55-64", "65-74", "75-84", "85+", "0")

lifeexp <- lifeexp %>% slice(1:10) %>% mutate(age_bkt =  ifelse(age_bkt == "1-4", "0-4", age_bkt)) %>% mutate(nw = ifelse(age_bkt == "0-4", 79.9, nw)) %>% select(2,3)



oldagegroups <- c("0-4", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "5-9","50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
newagegroups <- c("0-4", "5-14", "15-24", "15-24", "25-34", "25-34", "35-44", "35-44", "45-54", "5-14", "45-54", "55-64", "55-64", "65-74", "65-74", "75-84", "75-84", "85+")
remap_age <- data.frame(old = oldagegroups, new = newagegroups)


lly <- df %>% left_join(remap_age, by=c("ages"="old")) %>% left_join(lifeexp, by=c("new"="age_bkt")) %>% mutate(yll = crude_rate * 10^5 * nw)

#excess lly per year
excess_lly_year <- lly %>% group_by(years, sex, race) %>% summarize(yll = mean(yll)) %>% ungroup() %>% group_by(years, sex) %>% summarize(excess_yll = yll[race == "Black"] - yll[race == "White"])

(excess_lly_year %>% mutate(sex = ifelse(sex == "F", "Female", "Male")) %>% ggplot(aes(x=years, y=excess_yll, color=sex)) + geom_line(size=1) + geom_point(size=2.5) + scale_y_continuous(breaks=scales::pretty_breaks(n=8)) + panel_theme + sizing_theme +  ylab("Excess Potential Years of Life Lost") + ggtitle("CT State Excess Years of Life Lost") + scale_color_manual(values=cbb[1:2]) + scale_x_continuous(breaks=excess_aadr$years %>% unique(), labels= function(x) ifelse(x %% 2 == 1, x, "")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("") ) %>% ggsave("~/Downloads/CT-mortality/excess-ypll-year.pdf", plot = ., device = cairo_pdf, units = "in", height = 11, width = 12)


#excess lly per age
excess_lly_age <- df %>% left_join(remap_age, by=c("ages"="old")) %>% left_join(lifeexp, by=c("new"="age_bkt")) %>% group_by(ages, sex, race) %>% summarize(cr = mean(crude_rate), lifeexp = unique(nw)) %>% mutate(yll = cr * 10^5 * lifeexp) %>% ungroup() %>% group_by(ages, sex) %>% summarize(excess_yll = yll[race == "Black"] - yll[race == "White"]) 

(excess_lly_age %>% ungroup() %>% mutate(cat = as.numeric(sub("[-+].*", "", ages))) %>% filter(ages != "85+") %>% mutate(sex = ifelse(sex == "F", "Female", "Male")) %>% ggplot(aes(x=cat, y=excess_yll, group=sex, color=sex)) + geom_line(size=1.25) + geom_hline(yintercept=1, linetype="dashed") + scale_color_manual(values=c("maroon", "navy")) + panel_theme + sizing_theme + scale_x_continuous(limits=c(min(agevector$cat),max(agevector$cat)), breaks=agevector$cat, labels=agevector$ages) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(aes(color=sex), size=2.75) + scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +  theme(plot.title = element_text(hjust = 0.5)) + ylab("Excess Potential Years of Life Lost\nper 100K Individuals") + ggtitle("CT State Excess Years of Life Lost By Age (2005-2019)") ) %>% ggsave("~/Downloads/CT-mortality/excess-ypll-by-age.pdf", plot = ., device = cairo_pdf, units = "in", height = 11, width = 14)


#excess lly per age per 5 year block
excess_lly_age_yr <- df %>% mutate(yr_bkt = ifelse(years <= 2009, "2005-2009", ifelse(years <= 2014, "2010-2014", "2015-2019"))) %>% left_join(remap_age, by=c("ages"="old")) %>% left_join(lifeexp, by=c("new"="age_bkt")) %>% group_by(ages, sex, yr_bkt, race) %>% summarize(cr = mean(crude_rate), lifeexp = unique(nw)) %>% mutate(yll = cr * 10^5 * lifeexp) %>% ungroup() %>% group_by(ages, sex, yr_bkt) %>% summarize(excess_yll = yll[race == "Black"] - yll[race == "White"]) 

(excess_lly_age_yr %>% ungroup() %>% mutate(cat = as.numeric(sub("[-+].*", "", ages))) %>% filter(ages != "85+") %>% mutate(sex = ifelse(sex == "F", "Female", "Male")) %>% ggplot(aes(x=cat, y=excess_yll, group=yr_bkt, color=yr_bkt)) + facet_wrap(~sex, scales = "free", nrow = 1) + geom_line(size=1.25) + geom_hline(yintercept=1, linetype="dashed") + scale_color_manual(values=cbb[1:3]) + panel_theme + sizing_theme + scale_x_continuous(limits=c(min(agevector$cat),max(agevector$cat)), breaks=agevector$cat, labels=agevector$ages) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(aes(color=yr_bkt), size=2.75) + scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +  theme(plot.title = element_text(hjust = 0.5)) + ylab("Excess Mortality Rate per 100K Individuals") + ggtitle("CT State Excess Years of Life Lost By Age and Year") + theme(strip.text = element_text(size = 16))) %>% ggsave("~/Downloads/CT-mortality/excess-ypll-by-age-year.pdf", plot = ., device = cairo_pdf, units = "in", height = 11, width = 15)












































































