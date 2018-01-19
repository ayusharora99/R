install.packages("ggplot2")
library(ggplot2)
# batters: http://www.fangraphs.com/leaders.aspx?pos=p&stats=bat&lg=all&qual=0&type=8&season=2009&month=0&season1=2000&ind=1&team=0&rost=0&age=0&filter=&players=0
batting = read.csv('batting2010-2016.csv')
sfbatting = read.csv('sfbatting.csv')
firstsfbatting2016 = read.csv('SFBatting2016-1.csv')
secondsfbatting2016 = read.csv('SFBattin2016-2.csv')
# pitchers: http://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=8&season=2009&month=0&season1=2000&ind=1&team=0&rost=0&age=0&filter=&players=0
pitching = read.csv('SP 2010-2016.csv')
sfpitching = read.csv('Giants SP 2010-2017.csv')
firstsfsp2016 = read.csv('SFSP2016-1.csv')
secondsfsp2016 = read.csv('SFSP2016-2.csv')
firstsfrp2016 = read.csv('SFRP2016-1.csv')
secondsfrp2016 = read.csv('SFRP2016-2.csv')
# subset help
?subset

# narrow by innings and PA
sffiftypa = subset(sfbatting,PA>=100)
sfhundredip = subset(sfpitching,IP>=100)
secondsfsp2016thirtyip = subset(secondsfsp2016, IP >= 30)
# ggplot help
?ggplot

# plot K/9 vs. ERA as pitchers
pPlot = ggplot(firstsfsp2016,aes(ERA,FIP))

ppPlot = ggplot(secondsfsp2016thirtyip,aes(ERA,FIP))

aPlot = ggplot(firstsfsp2016, aes(AVG,BABIP))

aaPlot =  ggplot(secondsfsp2016thirtyip, aes(AVG,BABIP))

# error - must add more layers
pPlot

ppPlot

aPlot

aaPlot

# add points in red
pPlot = pPlot + geom_point(color = 'firebrick3')
pPlot

ppPlot = ppPlot + geom_point(color = 'firebrick3')
ppPlot

aPlot = aPlot +  geom_point(color = 'firebrick3')
aPlot

aaPlot = aaPlot + geom_point(color = 'firebrick3')
aaPlot


# add titles
pPlot = pPlot + ggtitle('ERA vs. FIP for Giants Starting Pitching 2016 1st Half (Min 30 IP)            R^2 = 0.743')
pPlot = pPlot + xlab('Earned Run Average')
pPlot = pPlot + ylab('Fielding Independent Pitching')
pPlot

ppPlot = ppPlot + ggtitle('ERA vs. FIP for Giants Starting Pitching 2016 2nd Half (Min 30 IP)          R^2 = 0.307 ')
ppPlot

aPlot = aPlot + ggtitle('AVG vs. BABIP for Giants Starting Pitching 2016 1st Half (Min 30 IP           R^2 =  0.8346)')
aPlot

aaPlot = aaPlot + ggtitle('AVG vs. BABIP for Giants Starting Pitching 2016 2nd Half (Min 30 IP         R^2 = 0.7275) ')
aaPlot

# add regression line
pPlot = pPlot + stat_smooth(method = 'lm',formula = y~x,color = 'black',size = 1)
pPlot
summary(lm(firstsfsp2016$ERA ~ firstsfsp2016$FIP))

ppPlot = ppPlot + stat_smooth(method = 'lm',formula = y~x,color = 'black',size = 1)
ppPlot
summary(lm(secondsfsp2016thirtyip$ERA ~ secondsfsp2016thirtyip$FIP))

aPlot = aPlot + stat_smooth(method = 'lm',formula = y~x,color = 'black',size = 1)
aPlot
summary(lm(firstsfsp2016$AVG ~ firstsfsp2016$BABIP))

aaPlot = aaPlot + stat_smooth(method = 'lm',formula = y~x,color = 'black',size = 1)
aaPlot
summary(lm(secondsfsp2016thirtyip$AVG ~ secondsfsp2016thirtyip$BABIP))



# merge help
?merge

# merge pitching and hitting
pitHit = merge(x = sfhundredip,y = sffiftypa,
               by.x = c('Season','playerid','Name'),by.y = c('Season','playerid','Name'))

# create OPS column
pitHit$OPS = pitHit$OBP + pitHit$SLG

# which best hitter?
pitHit$Name[which.max(pitHit$OPS)]

# top hitters 
pitHit = pitHit[order(-pitHit$OPS),]
head(subset(pitHit,subset = TRUE,select = c(Name,Season,OPS,ERA), n=10))

# how many > .700 OPS, < 4.00 ERA?
subset(pitHit,subset = OPS > .7 & ERA < 4,select = c(Name,Season,OPS,ERA))

# best pitcher with at least a .600 OPS?
bestRow = which.min(subset(pitHit,subset = OPS > .6)$ERA)
df = subset(pitHit,subset = OPS > .6,select = c(Name,Season,OPS,ERA))
df[bestRow,]

