```R
 $ SUBUNIT         : Factor w/ 10 levels "OTHER MINING",..: 9 8 8 8 8 8 8 8 9 9 ...
 $ ACCIDENT_DT     : POSIXct, format: "2012-03-14" "2007-01-08" "2009-07-04" "2000-05-26" ...
 $ ACCIDENT_TIME   : num  945 1105 1000 1100 1430 ...
 $ DEGREE_INJURY   : Factor w/ 12 levels "INJURIES INVOLVNG NONEMPLOYEES",..: 10 11 12 10 10 10 12 10 10 8 ...
 $ FIPS_STATE      : Factor w/ 52 levels "U.S. Virgin Islands",..: 46 46 46 46 46 46 46 46 46 46 ...
 $ UG_LOCATION     : Factor w/ 10 levels "UNDERGROUND SHOP/OFFICE",..: 10 10 10 10 10 10 10 10 10 10 ...
 $ UG_MINING_METHOD: Factor w/ 8 levels "Shortwall","Caving",..: 8 8 8 8 8 8 8 8 8 8 ...
 $ MINING_EQUIP    : Factor w/ 70 levels "Rotary dump, Dump rail ",..: 66 69 70 70 70 70 69 66 70 67 ...
 $ EQUIP_MFR_NAME  : Factor w/ 190 levels "Advance Mining Aerodyne",..: 188 189 190 190 190 190 189 188 190 187 ...
 $ SHIFT_BEGIN_TIME: num  600 700 600 700 700 700 2300 700 700 1800 ...
 $ CLASSIFICATION  : Factor w/ 29 levels "IMPOUNDMENT",..: 24 26 28 29 29 23 26 28 29 29 ...
 $ ACCIDENT_TYPE   : Factor w/ 45 levels "CONTCT W/ COLD OBJS OR SUBSTNC",..: 39 44 45 41 31 23 24 45 45 45 ...
 $ NO_INJURIES     : num  1 1 1 1 1 1 1 1 1 1 ...
 $ TOT_EXPER       : num  4.35 0.02 10 NA 0.87 ...
 $ MINE_EXPER      : num  4.35 0.02 2.15 0.23 0.87 ...
 $ JOB_EXPER       : num  0.67 0.02 2.15 0.23 0.38 ...
 $ ACTIVITY        : Factor w/ 99 levels "BRUSH FLOOR-INCREASE OPENING SIZE",..: 99 96 80 99 46 97 96 94 99 97 ...
 $ INJURY_SOURCE   : Factor w/ 127 levels "RDIOACT ORE-INJ FM RDIATN",..: 119 112 125 100 102 89 112 119 107 124 ...
 $ NATURE_INJURY   : Factor w/ 38 levels "LASER BURN","CONTAGIOUS,INFECT DISEASE",..: 34 37 38 38 28 38 38 38 33 27 ...
 $ INJ_BODY_PART   : Factor w/ 47 levels "LOWER EXTREMITIES,NEC",..: 43 34 39 45 40 45 42 45 45 36 ...
 $ SCHEDULE_CHARGE : num  0 0 0 NA 0 0 0 0 NA 0 ...
 $ DAYS_RESTRICT   : num  8 0 0 5 5 3 0 21 10 19 ...
 $ DAYS_LOST       : num  0 0 9 0 0 0 0 0 0 13 ...
 $ TRANS_TERM      : Factor w/ 3 levels "NO VALUE FOUND",..: 3 3 3 3 3 3 1 3 3 3 ...
 $ IMMED_NOTIFY    : Factor w/ 14 levels "IMPOUNDING DAM",..: 14 14 14 13 14 14 14 14 13 14 ...
 $ COAL_METAL_IND  : Factor w/ 2 levels "Coal","Metal": 2 2 2 2 2 2 2 2 2 2 ...
```

```R
###  Bar Chart - Underground Location accidents
We need to remove not underground mining data

tmp <- accidents[which(accidents$SUBUNIT == 'UNDERGROUND'),]
length(tmp[which(tmp$UG_LOCATION == 'NO VALUE FOUND'),]$UG_LOCATION) # 1 value contain NO VALUE FOUND
tmp <- tmp[-which(tmp$UG_LOCATION == 'NO VALUE FOUND'),] # remove it
tmp$UG_LOCATION      <- factor(tmp$UG_LOCATION      , levels=names(sort(table(tmp$UG_LOCATION), decreasing=FALSE)))
ggplot(tmp, aes(x=UG_LOCATION))               + 
     geom_bar(fill = "#003f5c")                     +
     scale_y_continuous(labels = comma)             +
     ggtitle("Accidents by Underground Location")   +
     xlab("Underground Location")                   +
     coord_flip()

###  Bar Chart - Underground Mining Method accidents

tmp$UG_MINING_METHOD      <- factor(tmp$UG_MINING_METHOD      , levels=names(sort(table(tmp$UG_MINING_METHOD), decreasing=FALSE)))
ggplot(tmp, aes(x=UG_MINING_METHOD))                   + 
     geom_bar(fill = "#003f5c")                        +
     scale_y_continuous(labels = comma)                +
     ggtitle("Accidents by Underground Mining Method") +
     xlab("Underground Mining Method")                 +
     coord_flip()

We can see that most accident occure at underground is continous mining, 

We can see there's lost of `NO VALUE FOUND ` data

ggplot(tmp[which(tmp$UG_MINING_METHOD == 'NO VALUE FOUND' ),], aes(x=UG_MINING_METHOD, fill=UG_LOCATION)) + 
     geom_bar(position="dodge")
```

```R
virginIslands <- map_data("world", "virgin islands")
puertoRico <- map_data("world", "puerto rico")
Alaska <- subset( map_data("world"), subregion == "Alaska")

accidents.state <- accidents %>%
     group_by(FIPS_STATE) %>%
     summarise(TOTAL = n()) %>%
     mutate(FIPS_STATE = tolower(FIPS_STATE)) %>%
     mutate(FIPS_STATE = ifelse(FIPS_STATE == "u.s. virgin islands", "Virgin Islands", FIPS_STATE)) 

accidents.state.usa <- merge(accidents.state, state, by.x = "FIPS_STATE", by.y="region")
accidents.state.usa <- accidents.state.usa[order(accidents.state.usa$order),]

accidents.state.VI <- merge(accidents.state, virginIslands, by.x = "FIPS_STATE", by.y="region")%>%
     subset(group > 3)
accidents.state.VI <- accidents.state.VI[order(accidents.state.VI$order),]

accidents.state.PR <- merge(accidents.state, mutate(puertoRico, region=tolower(region)), by.x = "FIPS_STATE", by.y="region")
accidents.state.PR <- accidents.state.PR[order(accidents.state.PR$order),]

accidents.state.alaska <- merge(accidents.state, mutate(Alaska, region='alaska'), by.x = "FIPS_STATE", by.y="region")

```
```R
accidents.state.lab <- accidents.state.usa %>%
    group_by(FIPS_STATE)  %>%
    summarise(
        long = kmeans(long, centers=1)$centers,
        lat = kmeans(lat, centers=1)$centers
    )
accidents.state.lab.VI <- accidents.state.VI %>%
    group_by(FIPS_STATE)  %>%
    summarise(
        long = kmeans(long -40, centers=1)$centers,
        lat = kmeans(lat, centers=1)$centers+1
    )
accidents.state.lab.PR <- accidents.state.PR %>%
    group_by(FIPS_STATE)  %>%
    summarise(
        long = kmeans(long, centers=1)$centers,
        lat = kmeans(lat, centers=1)$centers+1
    )
accidents.state.lab.alaska <- accidents.state.alaska %>%
    group_by(FIPS_STATE)  %>%
    summarise(
        long = kmeans(long/2.3, centers=1)$centers,
        lat = kmeans(lat/2.3, centers=1)$centers
    )
```R

```R
theme <- theme(panel.grid = element_blank(),
     panel.background = element_rect(fill = '#458BD0'),
     panel.border = element_blank(),
     axis.title.x=element_blank(),
     axis.text.x=element_blank(),
     axis.ticks.x=element_blank(),
     axis.title.y=element_blank(),
     axis.text.y=element_blank(),
     axis.ticks.y=element_blank()
)
ggplot(accidents.state.usa) +
    geom_polygon(data=accidents.state.usa, aes(x = long, y = lat, fill = TOTAL, group = group)) + 
    geom_polygon(data=accidents.state.PR, aes(x = long, y = lat, fill = TOTAL, group = group)) +
    geom_polygon(data=accidents.state.alaska, aes(x = long/2.3, y = lat/2.3, fill = TOTAL, group = group)) +
    geom_polygon(data=accidents.state.VI, aes(x = long-40, y = lat, fill = TOTAL, group = group)) +
    geom_text(aes(x = long, y = lat, label = FIPS_STATE), data = accidents.state.lab,  size = 2) +
    geom_text(aes(x = long, y = lat, label = FIPS_STATE), data = accidents.state.lab.PR,  size = 3) +
    geom_text(aes(x = long, y = lat, label = FIPS_STATE), data = accidents.state.lab.alaska,  size = 3) +
    geom_text(aes(x = long, y = lat, label = FIPS_STATE), data = accidents.state.lab.VI,  size = 3) +
    scale_fill_gradient(low="#F1FFFC", high="#003f5c") + xlim(-130,-50) + ylim(17, 50) + 
    ggtitle("Total accident by FIPS State") +
    theme
```

```R
accident.death <- accidents.date[which(accidents$IMMED_NOTIFY == 'DEATH'),] %>%
     subset(year < 2015)
p1 <- ggplot(accident.death, aes(x= year,)) +
    geom_bar(fill="#003f5c") + 
    ggtitle("fatalities in mining") +
    theme(
        legend.position="bottom", legend.direction = 'horizontal',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    )

p2 <- ggplot(subset(accident.death, COAL_METAL_IND =="Coal"), aes(x= year,)) +
    geom_bar(fill="#003f5c") + 
    ggtitle("fatalities in core mining") +
    theme(
        legend.position="bottom", legend.direction = 'horizontal',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    )

p3 <- ggplot(subset(accident.death, COAL_METAL_IND !="Coal"), aes(x= year,)) +
    geom_bar(fill="#003f5c") + 
    ggtitle("fatalities in metal mining") +
    theme(
        legend.position="bottom", legend.direction = 'horizontal',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    )
grid.arrange(p1, p2, p3， nrow=3,ncol=1)
```

We see that Death did not decrease over time. In 2001, 2006, 2010, the death toll slightly higher than other years in Coal mining. And in metal mining, number of deaths increased in 2000, 2007, 2014.

To check why, we can plot with classification 

```R
accident.death$CLASSIFICATION <- factor(accident.death$CLASSIFICATION)

accident.death.year <-  accident.death%>%
     subset(COAL_METAL_IND =="Coal" ) %>%
     group_by(year, CLASSIFICATION) %>%
     summarise( TOTAL = n()) %>%
     subset(CLASSIFICATION !="OTHER" )

p1 <- ggplot(subset(accident.death.year, TOTAL>1), aes(y=TOTAL, x= year, fill=CLASSIFICATION)) +
    geom_col(position = "dodge") + 
    geom_text(aes(label = TOTAL), position = position_dodge(width=1), vjust = -0.5)+
    ggtitle("fatalities in core mining") +
    theme(
        legend.position="bottom", legend.direction = 'horizontal',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    )

accident.death.year <-  accident.death %>% 
     subset(COAL_METAL_IND !="Coal" ) %>%
     group_by(year, CLASSIFICATION) %>%
     summarise( TOTAL = n()) %>%
     subset(CLASSIFICATION !="OTHER" )

p2 <- ggplot(subset(accident.death.year, TOTAL>1), aes(y=TOTAL, x= year, fill=CLASSIFICATION)) +
    geom_col(position = "dodge") + 
    geom_text(aes(label = TOTAL), position = position_dodge(width=1), vjust = -0.5)+
    ggtitle("fatalities in metal mining") +
    theme(
        legend.position="bottom", legend.direction = 'horizontal',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    )
grid.arrange(p1,p2, nrow=2,ncol=1)
```
The above graph shows powered haulage and machinery were responsible for a significant proportion of mining deaths.

Infrequent  Major accident - coal dust explosion caused many death in one accident, and is the reason why 2001, 2006, 2010  deaths increased in coal mining.

there's a slight drop in powered haulage and machinery accidents each year, but there's also a increase trend in falling accidents.