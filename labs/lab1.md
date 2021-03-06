Lab 1: Data Formats for Networks
================
Tiernan Cahill
09/06/2021

# Loading Data and Required Packages

Let’s begin by importing the raw data from Brandwatch (stored in an
Excel file), as well as loading all of the packages we will need for
this lab:

-   `readxl` for reading Excel data into an R data frame
-   `dplyr` for wrangling data
-   `stringr` for parsing some messy text in the original dataset
-   `tidyr` for restructuring data
-   `igraph` for modelling and visualizing our network
-   `Matrix` for handling sparse matrices

Note that `dplyr`, `stringr`, and `tidyr` are all included as part of
the `tidyverse` package.

``` r
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(igraph)
library(Matrix)

lab <- read_excel("../data/lab.xlsx") %>% 
  rename_all(~str_replace_all(., "\\s+", "")) # Remove whitespace from variable names

# Let's take a look!
glimpse(lab)
```

    ## Rows: 5,000
    ## Columns: 80
    ## $ ...1                       <chr> "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "1~
    ## $ Date                       <chr> "2020-11-02 18:51:36.0", "2020-10-16 11:20:02.0", "2020-09-14 16:02:29.0", "2020-10-24 02:28:22.0", "~
    ## $ Title                      <chr> "please go vote or turn in mail ballot! #Election2020", "RT @TexasGOP Report any irregularities to th~
    ## $ Url                        <chr> "http://twitter.com/jackiezgonzalez/statuses/1323336977547030529", "http://twitter.com/janesmi4154244~
    ## $ Domain                     <chr> "twitter.com", "twitter.com", "twitter.com", "twitter.com", "twitter.com", "twitter.com", "twitter.co~
    ## $ Sentiment                  <chr> "positive", "neutral", "neutral", "neutral", "positive", "neutral", "neutral", "neutral", "neutral", ~
    ## $ PageType                   <chr> "twitter", "twitter", "twitter", "twitter", "twitter", "twitter", "twitter", "twitter", "twitter", "t~
    ## $ Language                   <chr> "en", "en", "en", "en", "en", "en", "en", "en", "en", "en", "en", "en", "en", "en", "en", "en", "en",~
    ## $ CountryCode                <chr> "USA", "USA", "USA", "USA", "USA", "USA", "USA", "USA", "USA", "USA", "USA", "USA", "USA", "USA", "US~
    ## $ ContinentCode              <chr> "NORTH AMERICA", "NORTH AMERICA", "NORTH AMERICA", "NORTH AMERICA", "NORTH AMERICA", "NORTH AMERICA",~
    ## $ Continent                  <chr> "North America", "North America", "North America", "North America", "North America", "North America",~
    ## $ Country                    <chr> "United States of America", "United States of America", "United States of America", "United States of~
    ## $ CityCode                   <chr> NA, NA, "USA.NY.New York", NA, "USA.NM.Albuquerque", "USA.NY.New York", NA, NA, NA, NA, NA, "USA.NY.N~
    ## $ AccountType                <chr> "individual", "individual", "individual", "individual", "individual", "individual", "individual", "in~
    ## $ Added                      <chr> "2020-11-02T18:57:17.232+0000", "2020-10-24T20:41:41.167+0000", "2020-09-14T16:05:15.976+0000", "2020~
    ## $ Assignment                 <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ Author                     <chr> "jackiezgonzalez", "janesmi41542442", "mmcthemonitor", "CJWarner1", "NMSecOfState", "Cindy_ocean", "d~
    ## $ Avatar                     <chr> "https://audiences.brandwatch.com/api/audiences/v1/avatars/twitter/2170837448?token=3ca60bde9cdafe897~
    ## $ CategoryDetails            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ Checked                    <chr> "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "f~
    ## $ City                       <chr> NA, NA, "New York", NA, "Albuquerque", "New York", NA, NA, NA, NA, NA, "New York", "Buffalo", "Washin~
    ## $ DisplayURLs                <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ ExpandedURLs               <chr> NA, NA, NA, "https://www.washingtonexaminer.com/opinion/joe-biden-is-lying-his-ukraine-policy-created~
    ## $ FacebookAuthorID           <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ FacebookComments           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ FacebookLikes              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ FacebookRole               <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ FacebookShares             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ FacebookSubtype            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ FullName                   <chr> "jackiezgonzalez (jackie g <U+200E><U+2735>)", "janesmi41542442 (jaynesmith)", "mmcthemonitor (The Monitor)", "CJWar~
    ## $ FullText                   <chr> "please go vote or turn in mail ballot! #Election2020", "RT @TexasGOP Report any irregularities to th~
    ## $ Gender                     <chr> "female", "unknown", "unknown", "unknown", "unknown", "unknown", "unknown", "female", NA, "unknown", ~
    ## $ Hashtags                   <chr> "#election2020", "#earlyvoting, #vote, #election2020", "#mmcthemonitor, #marymountmahattan, #nyc, #po~
    ## $ Impact                     <dbl> 4.9, 0.0, 0.0, 12.9, 51.9, 0.0, 0.0, 0.0, 61.4, 6.2, 29.7, 27.3, 0.0, 3.4, 38.1, 81.6, 0.0, 3.1, 37.5~
    ## $ Impressions                <dbl> 952, 882, 384, 3807, 31827, 30, 573, 589, 341429, 2995, 8113, 6627, 14, 2233, 18346, 2049264, 961, 21~
    ## $ InstagramComments          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ InstagramFollowers         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ InstagramFollowing         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ InstagramInteractionsCount <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ InstagramLikes             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ InstagramPosts             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ Interest                   <chr> NA, "Family & Parenting", NA, "Politics", NA, NA, "Sports", NA, "Business, Politics", "Beauty/Health ~
    ## $ LastAssignmentDate         <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ Latitude                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ LocationName               <chr> "CA, USA", "USA", "New York, NY, USA", "USA", "Albuquerque, NM, USA", "New York, NY, USA", "USA", "TX~
    ## $ Longitude                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ MediaFilter                <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ MediaURLs                  <chr> NA, "http://pbs.twimg.com/media/EkX-XyfWsAAvQCo.jpg", "http://pbs.twimg.com/media/Eh4zE0TWsAAxhZK.png~
    ## $ MentionedAuthors           <chr> NA, "@texasgop", "@seamusfallon3", "@derekdob", NA, "@warroompandemic, @realdonaldtrump<U+2069>, @mariabartir~
    ## $ OriginalUrl                <chr> "http://twitter.com/jackiezgonzalez/statuses/1323336977547030529", "http://twitter.com/TexasGOP/statu~
    ## $ Priority                   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ Professions                <chr> NA, NA, NA, NA, "{profession=Politician, jobTitle=Secretary of State}", NA, NA, NA, NA, NA, NA, NA, N~
    ## $ ResourceId                 <chr> "15b33814193546ecc6bee3a1b6b6cbcb", "299671a225b603e6b52cd7d2fe373ee0", "fc95d6f51b65bcb94d5ef191a434~
    ## $ ShortURLs                  <chr> NA, "https://t.co/KLfMAFKBIZ", "https://t.co/yShRSWemUh", "https://t.co/x5ss0ERijJ", "https://t.co/dR~
    ## $ Starred                    <chr> "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false", "f~
    ## $ Status                     <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ Subtype                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ ThreadAuthor               <chr> NA, "TexasGOP", NA, "derekdob", NA, "WarRoomPandemic", "SammiG0203", "WarRoomPandemic", "MMassalas", ~
    ## $ ThreadCreatedDate          <chr> NA, "2020-10-15T13:50:58.000+0000", NA, "2020-10-23T12:20:37.000+0000", NA, "2020-10-27T11:57:01.000+~
    ## $ ThreadEntryType            <chr> "post", "share", "post", "share", "post", "share", "share", "share", "share", "share", "share", "shar~
    ## $ ThreadId                   <chr> "0", "1316738339278663686", "0", "1319614706240335873", "0", "1321058315489710086", "1317534971331006~
    ## $ ThreadURL                  <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ TotalMonthlyVisitors       <dbl> 6e+09, 6e+09, 6e+09, 6e+09, 6e+09, 6e+09, 6e+09, 6e+09, 6e+09, 6e+09, 6e+09, 6e+09, 6e+09, 6e+09, 6e+~
    ## $ TwitterAuthorID            <chr> "2170837448", "827502320162201600", "217612902", "542071766", "806695193873358852", "1255996367820017~
    ## $ TwitterChannelRole         <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ TwitterFollowers           <dbl> 697, 882, 384, 3807, 2768, 30, 573, 589, 341429, 2995, 8113, 6627, 14, 2233, 18346, 17762, 961, 2168,~
    ## $ TwitterFollowing           <dbl> 199, 371, 294, 4058, 283, 96, 1125, 275, 206082, 3968, 6459, 2765, 208, 5001, 12877, 10904, 298, 3666~
    ## $ TwitterReplyCount          <dbl> 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ TwitterReplyto             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "http://twitter.com/Keen_James/statuses/132023860~
    ## $ TwitterRetweetof           <chr> NA, "http://twitter.com/TexasGOP/statuses/1316738339278663686", NA, "http://twitter.com/derekdob/stat~
    ## $ TwitterRetweets            <dbl> 1, 0, 0, 0, 14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 110, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ TwitterTweets              <dbl> 20172, 93031, 1227, 136367, 1275, 1277, 5421, 95096, 399832, 39140, 117772, 476947, 410, 173773, 8595~
    ## $ TwitterVerified            <chr> "false", "false", "false", "false", "true", "false", "false", "false", "false", "false", "false", "fa~
    ## $ Updated                    <chr> "2020-11-02T21:22:10.883+0000", "2020-10-24T20:41:41.167+0000", "2020-09-14T16:05:15.976+0000", "2020~
    ## $ `Reach(new)`               <dbl> 358, 0, 0, 2423, 10037, 0, 0, 0, 27532, 1927, 4312, 3754, 0, 1376, 7043, 47419, 92, 1324, 6705, 201, ~
    ## $ Copyright                  <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ PageTypeName               <chr> "Twitter", "Twitter", "Twitter", "Twitter", "Twitter", "Twitter", "Twitter", "Twitter", "Twitter", "T~
    ## $ RedditScore                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ Region                     <chr> "California", NA, "New York", NA, "New Mexico", "New York", NA, "Texas", "Texas", "Arizona", "Florida~
    ## $ RegionCode                 <chr> "USA.CA", NA, "USA.NY", NA, "USA.NM", "USA.NY", NA, "USA.TX", "USA.TX", "USA.AZ", "USA.FL", "USA.NY",~

Looking at the raw data, we can see that there are `80` variables
included from Brandwatch, many of which are not relevant to our
analysis, or even to this particular context (e.g.,
`InstagramFollowers`). As we restructure this dataset into various
formats for network analysis in the following sections, we will have to
pick and choose which variables are necessary in each case.

## Keyword Matching

Especially when dealing with large social media datasets, you may wish
to narrow down the list of observations that you use for constructing
your network. By combining the `filter` and `str_detect` functions, we
can use regular expressions to check the text of tweets in the dataset
for pattern matches and extract a subset of documents to use in the
subsequent network analysis.

``` r
covid_tweets <- lab %>% 
  filter(str_detect(FullText, "(?i:(#?covid-?(19)?)|(corona(virus)?)|(pandemic))"))

# Let's take a look!
covid_tweets %>% 
  select(FullText) %>% 
  head()
```

    ## # A tibble: 6 x 1
    ##   FullText                                                                                                                                
    ##   <chr>                                                                                                                                   
    ## 1 RT @WarRoomPandemic #SteveBannon predicts a <U+2066>@realDonaldTrump<U+2069> victory in #Election2020 Bannon tells @MariaBartiromo: “President Trump is~
    ## 2 RT @WarRoomPandemic Bannon: @JoeBiden's campaign has no scalability. You can't just 'COVID' this and fire people up digitally. The Dem ~
    ## 3 Just like that...there was no more Flu ??...#COVID19 #coronavirus #plandemic #MediaWatch #Election2020 @DonaldJTrumpJr https://t.co/ahp~
    ## 4 RT @JohnVetsResist1 Have had to do. As I sit here in a puddle of tears I'm frozen in thought but thinking I did the right thing. I feel~
    ## 5 RT @AprilDRyan Who would have ever thought that @realDonaldTrump & @MelaniaTrump testing positive for #COVID19 would be the October Sur~
    ## 6 RT @DrThomasPaul #COVID19 officially ends on November 4, 2020. #Election2020

# Formats for Representing Networks

## Edge List

The format that most closely resembles the structure of the data we
already have is probably an *edge list*. In this case, we have a list of
tweets, which we can model as *edges*, with the Twitter users who are
either posting or mentioned in each tweet being represented as *nodes*.

``` r
edges <- lab %>%
  select(V1 = Author, V2 = MentionedAuthors) %>%
  separate_rows(V2, sep = ", ") %>%                   # Possibly >1 edge per tweet
  mutate(V2 = str_replace_all(V2, "@", ""))           # Consistent formatting for Node IDs

# Let's take a look!
head(edges)
```

    ## # A tibble: 6 x 2
    ##   V1              V2             
    ##   <chr>           <chr>          
    ## 1 jackiezgonzalez <NA>           
    ## 2 janesmi41542442 texasgop       
    ## 3 mmcthemonitor   seamusfallon3  
    ## 4 CJWarner1       derekdob       
    ## 5 NMSecOfState    <NA>           
    ## 6 Cindy_ocean     warroompandemic

### Weighted Edges

Note that some of these tweets don’t mention any other users, and so we
have incomplete edges. We can choose to retain these in our network
model as *isolates* or to remove them from the dataset. Also, remember
that these edges represent a *directed* network. In some cases, there
may be more than one tweet between the same pair of users, which we can
represent by giving the edge a *weight*.

``` r
weighted_edges <- edges %>% 
  count(V1, V2, name = "weight")

# Let's take a look!
head(weighted_edges)
```

    ## # A tibble: 6 x 3
    ##   V1             V2              weight
    ##   <chr>          <chr>            <int>
    ## 1 ___Dustin__    jorgensen4potus      1
    ## 2 __Ladderman421 danscavino           1
    ## 3 __MrSwift__    hdrewgalloway        1
    ## 4 __Princess__Q  joebiden             1
    ## 5 __Princess__Q  leandroshan          1
    ## 6 __ruthlesss__  bbcjonsopel          1

There will now be no duplicate edges in the list, but user pairs who had
multiple interactions in the sample will have a higher weight associated
with that edge.

### Adding Node Attributes

In some situations, we may want a graph object to include attributes
associated with the nodes in a network that are not directly related to
the network structure (e.g., the number of followers a particular
Twitter user has, or the location they have added to their profile). To
add these variables to the `igraph` object, we first need to extract
them from the existing dataset and make sure that they are appropriately
formatted.

``` r
nodes <- lab %>%
  select(id = Author, verified = TwitterVerified, 
         followers = TwitterFollowers, region = Region) %>%
  mutate(verified = as.logical(verified),
         region = as.factor(region))

# Also need to make sure that mentioned users are included, 
# even though we do not have data for them
mentioned_nodes <- lab %>%
  select(id = MentionedAuthors) %>%
  mutate(id = str_replace_all(id, "@", "")) %>%
  separate_rows(id, sep = ", ")

nodes <- nodes %>% 
  bind_rows(mentioned_nodes) %>%
  distinct(id, .keep_all = T) %>%
  arrange(id)   
```

### Converting to a Network

To conduct later network analysis on the network model we’ve created, we
will need to convert it into a *graph object* that the tools from the
`igraph` package can understand. (Also, this is a good place to point
out that `igraph` functions refer to nodes as *vertices*.)

``` r
# To avoid misassigning vertex attributes due to inconsistent ordering of node
# lists, we can instead create an entirely new graph object that incorporates
# both our existing edge list and our new node list (with attributes)

mention_graph <- graph_from_data_frame(weighted_edges, directed = T, vertices = nodes) %>%
  delete_vertices("NA") # igraph interprets missing values as a string

# Let's take a look!
E(mention_graph) %>% head()
```

    ## + 6/8038 edges from 525d99a (vertex names):
    ## [1] ___Dustin__   ->jorgensen4potus __Ladderman421->danscavino      __MrSwift__   ->hdrewgalloway   __Princess__Q ->joebiden       
    ## [5] __Princess__Q ->leandroshan     __ruthlesss__ ->bbcjonsopel

``` r
V(mention_graph) %>% head()
```

    ## + 6/7620 vertices, named, from 525d99a:
    ## [1]                ___Dustin__    __Ladderman421 __MrSwift__    __Princess__Q  __ruthlesss__

``` r
vertex.attributes(mention_graph) %>% glimpse()
```

    ## List of 4
    ##  $ name     : chr [1:7620] "" "___Dustin__" "__Ladderman421" "__MrSwift__" ...
    ##  $ verified : logi [1:7620] NA FALSE FALSE FALSE FALSE FALSE ...
    ##  $ followers: num [1:7620] NA 67 186 1000 2032 ...
    ##  $ region   : chr [1:7620] NA "Texas" "California" "Texas" ...

## Adjacacency Matrix

Graph models can also be represented as an *adjacency matrix*. This
format can be especially useful for representing small, densely
connected networks, but can get large and unwieldy with large, sparse
networks (such as the Twitter mentions we’ve been looking at).

It’s worth noting that `igraph` can convert back and forth between most
major network formats, allowing us to easily produce an adjacency matrix
representation from our existing graph object.

``` r
mention_matrix <- as_adjacency_matrix(mention_graph)

# Let's take a look!
mention_matrix[0:10, 0:10]
```

    ## 10 x 10 sparse Matrix of class "dgCMatrix"

    ##    [[ suppressing 10 column names '', '___Dustin__', '__Ladderman421' ... ]]

    ##                                   
    ##                . . . . . . . . . .
    ## ___Dustin__    . . . . . . . . . .
    ## __Ladderman421 . . . . . . . . . .
    ## __MrSwift__    . . . . . . . . . .
    ## __Princess__Q  . . . . . . . . . .
    ## __ruthlesss__  . . . . . . . . . .
    ## _ACHP          . . . . . . . . . .
    ## _alex_myers_   . . . . . . . . . .
    ## _ashdashlee_   . . . . . . . . . .
    ## _asyan         . . . . . . . . . .

In addition to modelling interactions between users, we can also look at
co-incidence between hashtags as a network. Below, we have some code
that generates an adjacency matrix of hashtags based on the Brandwatch
data. Unlike the earlier network of mentions, this one will be
*undirected*, since it only indicates that two hashtags appeared
together in a tweet and the number of times this occurred in the dataset
(represented as the *weight* of that edge).

``` r
htags <- lapply(lab$Hashtags, str_split, pattern = ", ")
htags <- lapply(htags, unlist, recursive = F)

# Credit to Martin Stefan for the matrix generation code below 
# [https://tinyurl.com/7sd76m8p]

# First create a list of unique tags in the dataset to set the dimensions of the matrix
utags <- unique(unlist(htags))

htag_matrix <- matrix(0, length(utags), length(utags))
rownames(htag_matrix) <- utags
colnames(htag_matrix) <- utags

# Then, iterate through the list of tweets and add to the adjacency matrix
for(t in 1:length(htags)) {
  tags <- htags[[t]]
  
  if(length(tags) == 1) next()
  
  htag_matrix[tags, tags] <- htag_matrix[tags, tags] + 1
}

# Set the diagonal of the matrix to 0 (hashtags are not adjacent to themselves)
diag(htag_matrix) <- 0

# Create graph object
htag_graph <- graph_from_adjacency_matrix(htag_matrix, mode = "undirected", 
                                          weighted = TRUE)

# Let's take a look!
htag_graph[1:10, 1:10]
```

    ## 10 x 10 sparse Matrix of class "dgCMatrix"

    ##    [[ suppressing 10 column names '#election2020', '#earlyvoting', '#vote' ... ]]

    ##                                                     
    ## #election2020              . 97 812 1 1 8 79 6 54 51
    ## #earlyvoting              97  .  56 . . 1  . .  .  .
    ## #vote                    812 56   . . . 2  9 .  5 10
    ## #mmcthemonitor             1  .   . . 1 1  1 .  .  .
    ## #marymountmahattan         1  .   . 1 . 1  1 .  .  .
    ## #nyc                       8  1   2 1 1 .  1 .  .  .
    ## #politics                 79  .   9 1 1 1  . .  2  3
    ## #bidencrimefamilyexposed   6  .   . . . .  . .  1  1
    ## #debate2020               54  .   5 . . .  2 1  . 18
    ## #presidentialdebate       51  .  10 . . .  3 1 18  .

## Adjacency List

*Adjacency lists* are an alternative format to adjacency matrices,
consisting of a list of nodes and each node’s connected neighbours. For
sparse networks, this format can be more efficient than an equivalent
adjacency matrix, since the length of the list is equal to the number of
nodes, whereas the size of the matrix is the square of the number of
nodes.

``` r
htag_list <- as_adj_list(htag_graph)

# Let's take a look!
htag_list %>% head(3)
```

    ## $`#election2020`
    ## + 3158/3181 vertices, named, from 53d961b:
    ##    [1] #earlyvoting                             #vote                                    #mmcthemonitor                          
    ##    [4] #marymountmahattan                       #nyc                                     #politics                               
    ##    [7] #bidencrimefamilyexposed                 #debate2020                              #presidentialdebate                     
    ##   [10] #joebideniscorrupt                       #walkaway                                #biden2020                              
    ##   [13] #joebidenlies                            #stopvotingfordemocrats                  #demexit                                
    ##   [16] #nmpol                                   #stevebannon                             #warroompandemic                        
    ##   [19] #bidenharris2020                         #trumppence2020                          #voteearly                              
    ##   [22] #evildems                                #trumpnation2020                         #maga                                   
    ##   [25] #facebook                                #trump                                   #larrysharpe                            
    ##   [28] #libertarian                             #liberty                                 #democrat                               
    ## + ... omitted several vertices
    ## 
    ## $`#earlyvoting`
    ## + 112/3181 vertices, named, from 53d961b:
    ##   [1] #election2020             #vote                     #nyc                      #bidenharris2020          #voteearly               
    ##   [6] #maga                     #trump                    #liberty                  #republicans              #leadright               
    ##  [11] #covid19                  #biden                    #trump2020                #elections                #florida                 
    ##  [16] #pennsylvania             #vegas                    #clarkcounty              #dontloseyourvoice        #voteblue                
    ##  [21] #democrats                #gotv                     #arkansas                 #justice                  #opportunity             
    ##  [26] #bluewave2020             #arlegis                  #arlege                   #arpol                    #2020elections           
    ##  [31] #retweeet                 #president                #electionday              #votebymail               #vote2020                
    ##  [36] #holdtexas                #keeptexasred             #wearethestorm            #election                 #nevada                  
    ##  [41] #demvoice1                #mailinballots            #lasvegas                 #voting                   #2020election            
    ##  [46] #ivoted                   #halloween                #votebiden                #votebidenharris2020      #votebluedownballot      
    ## + ... omitted several vertices
    ## 
    ## $`#vote`
    ## + 861/3181 vertices, named, from 53d961b:
    ##   [1] #election2020                   #earlyvoting                    #nyc                            #politics                      
    ##   [5] #debate2020                     #presidentialdebate             #walkaway                       #biden2020                     
    ##   [9] #bidenharris2020                #trumppence2020                 #voteearly                      #maga                          
    ##  [13] #trump                          #larrysharpe                    #libertarian                    #liberty                       
    ##  [17] #democrat                       #republicans                    #debates2020                    #blackvoicesfortrump           
    ##  [21] #blackvote                      #platinumplan                   #gop                            #promisesmadepromiseskept      
    ##  [25] #iwillvote                      #leadright                      #chinaownsbiden                 #chinaownsalldemocrats         
    ##  [29] #votered2020                    #removeeverydemocrat            #americafirst                   #covid19                       
    ##  [33] #coronavirus                    #biden                          #qanon2018                      #qanon2020                     
    ##  [37] #trump2020                      #votetrumpout                   #usa                            #elections                     
    ## + ... omitted several vertices

## Affiliation Matrix

We may sometimes need to model an *affiliation network*, showing
connections between two different *classes* of nodes and, in turn, the
indirect connections between nodes that share an affiliation. For
example, we might create a model in which nodes represent both users
**and** hashtags. In the resulting *bipartite graph*, users will never
be directly connected to other users, but will be indirectly connected
through the hashtags they have in common.

``` r
# Combine hashtag lists from multiple tweets by the same user
htag_uses <- lab %>%
  select(user = Author, htags = Hashtags) %>%
  group_by(user) %>% mutate(htags = paste(htags, collapse = ", "),
                            htags = str_split(htags, ", ")) %>%
  rowwise() %>% mutate(htags = list(unique(htags)),
                       htags = list(sort(htags))) %>%
  distinct()

# Convert the nested list to a matrix
htag_uses_matrix <- htag_uses %>%
  unnest(htags) %>%
  table() %>% unclass() %>% Matrix()

# Convert into a graph object
htag_uses_graph <- graph_from_incidence_matrix(htag_uses_matrix, mode = "out")

# Let's take a look!
E(htag_uses_graph) %>% head(20)
```

    ## + 20/16051 edges from 55123de (vertex names):
    ##  [1] beckya57       --#<U+03C9>         GrassrootsDNC  --#<U+03C9>         snowstormyou   --#<U+03C9>        
    ##  [4] eafinct        --#<U+6295><U+7968> NIHAustin      --#<U+6295><U+7968> oceanfootbaII  --#<U+6295><U+7968>
    ##  [7] MANMWA2        --#101nights        RealClelland   --#10milliondreams  JMelaniaKC     --#12moreyears     
    ## [10] SamChen220     --#13minutes        GlenSmi70330487--#1stamendment     GuiseElizabeth --#1u              
    ## [13] laborradionet  --#1u               Sheridan543    --#1u               ProChoiceCA    --#2020census      
    ## [16] WePoll_TheGame --#2020census       1andonlycarol  --#2020debate       CaseyAdamF     --#2020debate      
    ## [19] Chgogirl3      --#2020debate       Deplorable4US  --#2020debate

# Network Visualization

Once we have our networks stored as graph objects that `igraph` can
read, we can use the built-in plotting functions to produce some basic
visualizations. (*Note*: Given that it can be very difficult to
effectively visualize large-scale networks of the sort that we have
produced from Brandwatch data, for the purposes of this workshop, we
will use smaller, algorithmically-generated graphs.)

``` r
# Generate an Erdos-Renyi random graph with 40 nodes and a density of 0.05
# (By default; feel free to change the parameters below)
er_density <- 0.05
er_vcount <- 40
er_ecount <- (er_vcount * (er_vcount-1) / 2) * er_density
er_graph <- sample_gnm(n = er_vcount, m = er_ecount)

# Produce a plot with default settings
plot(er_graph)
```

![](lab1_files/figure-gfm/viz-demo-1.png)<!-- -->

``` r
# Adjust layout (using Fruchterman-Reingold algorithm and basic aesthetics)
plot(er_graph, 
     layout = layout_with_fr,
     vertex.label = NA,
     vertex.size = 8,
     vertex.color = "forestgreen")
```

![](lab1_files/figure-gfm/viz-demo-2.png)<!-- -->

``` r
# Set aesthetics based on graph attributes
V(er_graph)$size <- degree(er_graph)*4
E(er_graph)$width <- edge.betweenness(er_graph, directed = F) * 0.05

plot(er_graph, 
     layout = layout_with_fr,
     vertex.label = NA,
     vertex.color = "lightblue", 
     edge.curved = 0.2)
```

![](lab1_files/figure-gfm/viz-demo-3.png)<!-- --> Now that we have a
sense of how to use the `igraph` plotting functions, we can try to apply
it to a subset of our real-world dataset. For instance, if we were
interested in visualizing how people relate issues with the United
States Postal Service to other election topics, we might try to
visualize the ego network of the hashtag `#usps`:

``` r
# This function returns a list of graphs by default
usps_ego_graph <- make_ego_graph(htag_graph, nodes = "#usps")[[1]]

# Scale labels to eigenvector centrality and edges to weight
V(usps_ego_graph)$label.cex <- eigen_centrality(usps_ego_graph)$vector^0.2
E(usps_ego_graph)$width <- E(usps_ego_graph)$weight*0.01

# Create a layout with custom parameters
l <- layout_with_lgl(usps_ego_graph, root = "#usps")
l <- norm_coords(l, ymin = -1, ymax = 1, xmin = -1, xmax = 1)


plot(usps_ego_graph, 
     layout = l,
     rescale = F,
     vertex.shape = "none",
     vertex.label.color = "gray10",
     vertex.label.font = 2,
     edge.color = "gray85")
```

![](lab1_files/figure-gfm/viz-demo2-1.png)<!-- -->

# Serializing Graph Objects

To avoid repeating ourselves in subsequent labs and having to recreate
the graph objects from scratch, we’ll serialize them as RDS files that
can be loaded directly into other R scripts.

``` r
saveRDS(mention_graph, file = "../data/mention_graph.RDS")
saveRDS(htag_graph, file = "../data/htag_graph.RDS")
saveRDS(htag_uses_graph, file = "../data/htag_uses_graph.RDS")
```
