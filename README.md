# USFCA-Dons-Baseball-Pitching-RShiny-Dashboard
Designed to create a 'one stop shop' for USFCA Baseball coaches in managing and analyzing pitching performance data. Fully equipped with data visualizations, rankings and coaching suggestions to maximize pitch movement, consistency and overall pitch effectiveness. Originally created in undergrad at USFCA. Revised and published in 2026.


**How to navigate the dashboard**
First, select a pitcher from the dropdown menu on the left-hand side. 
Next, select a pitch type you want to view for the Boxplot and Scatterplot (FB = 4-seam fastball, 2FB = 2-seam fastball, CH = changeup, CV = curveball, SL = slider). The pitch type drop down is also used for the Summary tab, so the user will be able to see coaching notes and suggestions on training for the given pitch and pitcher.
Finally, select a variable to view for the Boxplot. (The scatterplot as titled, solely shows the horizontal/vertical break of the given pitch of the selected pitcher.

The leaderboard tab shows the full pitcher roster rankings based off several different pitch type/variable combinations. The combinations were decided on based on the level of importance for a given pitch type. For example, the velocity of a FB is a more significant variable to analyze compared to the CV, whose pitch movement (i.e. h/v break) and spin efficiency are more statistically significant.

The summary page--as previously mentioned above--is meant to aid coaches in their training direction during practices. As we know, each pitcher is unique in delivering their pitches, so it's important to note that these are just suggestions, rather than an expected training route. 

Overall, this dashboard portrays its potential if we were to update the dataframe after each practice using the Rapsodo technology.

**What is Rapsodo?**

Rapsodo is a sports technology company and brand that makes portable launch monitors and data-tracking devices for golf, baseball, and softball.
The systems combine radar and camera technology to capture real-time performance metrics, video, and 3D data so athletes and coaches can analyze mechanics and improve play.

All data in the dashboard was gathered using Rapsodo technology over the course of a couple months of practice.
