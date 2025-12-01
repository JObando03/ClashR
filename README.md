# ClashR
A package dedicated to fetching and analyzing Clash Royale statistics that are publicly available from RoyaleAPI. It grants anyone who wants to get more detailed insights the ability to look at factors such as their most used cards, win rates, progression, and more.
This package uses publicly available data provided by the RoyaleAPI
    (https://royaleapi.com). It is an independent project and is not
    affiliated with, endorsed, sponsored, or specifically approved by
    Supercell or RoyaleAPI. Users must comply with RoyaleAPI's API Terms of
    Service when using this package.
In order to use this package follow the steps below:
1) Go to this website https://developer.clashroyale.com/
2) Create a free account and generate your own api key
3) Once you have your token, Load ClashR into your environment and run the "set_cr_token("your_token")" command to save your token to your environment
4) Once token is saved, restart R and you will be able to use RoyaleAPI and ClashR functions

To download the package, use the code below
install.packages("devtools")
devtools::install_github("JObando03/ClashR")
library(ClashR)
