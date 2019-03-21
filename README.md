# starcraft-user-engagement
## Predicting user engagement for Star Craft II - predicting total hours played by the user

## Elena Gillis (emg3sc)
## Kanika Dawar (kd4hr)
## Karan Gadiya (khg8mh)
## Varshini Sriram (vs4vx)

#### December 6, 2018
#### University of Virginia, Data Science Institute

## Summary
The objective of this project is to analyze player on screen behaviour for a game called
StarCraft 2. After a gaming company releases a beta version of a newly launched game,
they wish to increase the users' playing time by analyzing their behaviour while playing the
game. Our primary hypothesis is that a players game statistics can have an influence on
the number of hours spent on playing the game. We claim that the total number of hours
that each player spends playing the game can be predicted by their on-screen behaviour. We
expect that the variables we have will explain a good amount of variation in our response
variable. If this proves to be true, the gaming company can identify the most important or
challenging portions of their product, improve on product usability and eventually use these
ndings to improve user-experience, for promotion, and marketing.
3 Introduction
StarCraft 2 (SC2) is a Real-Time Strategy (RTS) video game that is played at extraordinary
speeds. All matches start with each player choosing one of three species and controlling one
base and 6 worker units. Each of the three species control dierent buildings and units with
unique abilities and upgrades, creating a complex interaction within the games environment.
Basically, the three species units play out a dynamic game of rock-paper-scissors. Players
must command their units, prepare defences, and build base expansions to gather more re-
sources while building bigger armies to outmaneuver their enemies in what amounts to a
high speed game of Go.
To perform this analysis, we use SkillCraft data, which was collected from on-screen move-
ment patterns for players of StarCraft II real-time strategy game. The data was downloaded
from UCI Machine Learning repository and was collected in September of 2013 by the De-
4
partment of Psychology at Simon Fraser University. The objective of the game is to develop
a base, battleships, and defeat the opponents by destroying their bases. The bases are cre-
ated by gathering artifacts and resources. The data published is numbers of dierent types
of the on-screen movements made by individual players in one real-time second.
The nal model makes use of 9 variables - LeagueIndex, Age, HoursPerWeek, ActionLatency,
AssignToHotkeys, UniqueHotkeys, ActionsInPAC, UniqueUnitsMade, and SelectByHotkeys.
LeagueIndex species which skill level the player plays at - Bronze, Silver, Gold, Platinum,
Diamond, Master, GrandMaster, and Professional leagues (which are ordinally coded as 1-
8). Age and HoursPerWeek give the age of the player and the number of hours the player
played per week on average. HoursPerWeek is dierent from TotalHours in a way that some
players might play for only a few weeks but have higher average hours per week as compared
to other players who have been playing from a long time but play for comparatively fewer
hours every week. SelectByHotkeys is the number of unit or building selections made using
hotkeys. AssignHotKeys is a continuous value associating the number of units or buildings
assigned to hotkeys per timestamp. UniqueHotkeys denotes the number of unique hotkeys
used per timestamp while UniqueUnitsMade species the number of unique units made per
timestamp. ActionLatency denotes the mean latency from the onset of PACs to their rst
action in milliseconds. And the last variable ActionsInPAC is the number of PACs per
timestamp.
4 Approach
4.1 Data Preparation and Initial Exploration
Our original data has 3395 observations and 20 variables (submitted electronically). When
performing the exploratory data analysis we noticed that there were missing values in the
5
Total Hours variable, Hours Per Week, and Age. Since Total Hours is the variable that we
are trying to predict, we have removed the rows with missing observations. In addition, we
observed that some of the variables in the Total Hours column were lled with a question
mark - these rows were also removed from the dataset. This reduced the length of our data
to 3338 rows. We converted the variables to their intended types and prepared the data for
model building.
To visualize the dataset on a plot and reduce the variables to two dimensions we used the
Principle Component Analysis. We plotted the rst principle component against the re-
sponse variable, however no signicant patterns were seen in the data due to the wide range
of the response. In order to reduce the range, we log-transformed the response variable and
were able to observe that there is potentially a linear relationship in the data. This also
indicated that we will most likely need to log-transform the response variable when tting
our model.
Figure
