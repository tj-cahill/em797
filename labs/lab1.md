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
    ## $ ...1                       <chr> "1", "2", "3", "4", "5", "6", "7", "8", "9"~
    ## $ Date                       <chr> "2020-11-02 18:51:36.0", "2020-10-16 11:20:~
    ## $ Title                      <chr> "please go vote or turn in mail ballot! #El~
    ## $ Url                        <chr> "http://twitter.com/jackiezgonzalez/statuse~
    ## $ Domain                     <chr> "twitter.com", "twitter.com", "twitter.com"~
    ## $ Sentiment                  <chr> "positive", "neutral", "neutral", "neutral"~
    ## $ PageType                   <chr> "twitter", "twitter", "twitter", "twitter",~
    ## $ Language                   <chr> "en", "en", "en", "en", "en", "en", "en", "~
    ## $ CountryCode                <chr> "USA", "USA", "USA", "USA", "USA", "USA", "~
    ## $ ContinentCode              <chr> "NORTH AMERICA", "NORTH AMERICA", "NORTH AM~
    ## $ Continent                  <chr> "North America", "North America", "North Am~
    ## $ Country                    <chr> "United States of America", "United States ~
    ## $ CityCode                   <chr> NA, NA, "USA.NY.New York", NA, "USA.NM.Albu~
    ## $ AccountType                <chr> "individual", "individual", "individual", "~
    ## $ Added                      <chr> "2020-11-02T18:57:17.232+0000", "2020-10-24~
    ## $ Assignment                 <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ Author                     <chr> "jackiezgonzalez", "janesmi41542442", "mmct~
    ## $ Avatar                     <chr> "https://audiences.brandwatch.com/api/audie~
    ## $ CategoryDetails            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ Checked                    <chr> "false", "false", "false", "false", "false"~
    ## $ City                       <chr> NA, NA, "New York", NA, "Albuquerque", "New~
    ## $ DisplayURLs                <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ ExpandedURLs               <chr> NA, NA, NA, "https://www.washingtonexaminer~
    ## $ FacebookAuthorID           <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ FacebookComments           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ FacebookLikes              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ FacebookRole               <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ FacebookShares             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ FacebookSubtype            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ FullName                   <chr> "jackiezgonzalez (jackie g <U+200E><U+2735>)", "janesmi415~
    ## $ FullText                   <chr> "please go vote or turn in mail ballot! #El~
    ## $ Gender                     <chr> "female", "unknown", "unknown", "unknown", ~
    ## $ Hashtags                   <chr> "#election2020", "#earlyvoting, #vote, #ele~
    ## $ Impact                     <dbl> 4.9, 0.0, 0.0, 12.9, 51.9, 0.0, 0.0, 0.0, 6~
    ## $ Impressions                <dbl> 952, 882, 384, 3807, 31827, 30, 573, 589, 3~
    ## $ InstagramComments          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ InstagramFollowers         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ InstagramFollowing         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ InstagramInteractionsCount <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ InstagramLikes             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ InstagramPosts             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Interest                   <chr> NA, "Family & Parenting", NA, "Politics", N~
    ## $ LastAssignmentDate         <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ Latitude                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ LocationName               <chr> "CA, USA", "USA", "New York, NY, USA", "USA~
    ## $ Longitude                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ MediaFilter                <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ MediaURLs                  <chr> NA, "http://pbs.twimg.com/media/EkX-XyfWsAA~
    ## $ MentionedAuthors           <chr> NA, "@texasgop", "@seamusfallon3", "@derekd~
    ## $ OriginalUrl                <chr> "http://twitter.com/jackiezgonzalez/statuse~
    ## $ Priority                   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ Professions                <chr> NA, NA, NA, NA, "{profession=Politician, jo~
    ## $ ResourceId                 <chr> "15b33814193546ecc6bee3a1b6b6cbcb", "299671~
    ## $ ShortURLs                  <chr> NA, "https://t.co/KLfMAFKBIZ", "https://t.c~
    ## $ Starred                    <chr> "false", "false", "false", "false", "false"~
    ## $ Status                     <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ Subtype                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ ThreadAuthor               <chr> NA, "TexasGOP", NA, "derekdob", NA, "WarRoo~
    ## $ ThreadCreatedDate          <chr> NA, "2020-10-15T13:50:58.000+0000", NA, "20~
    ## $ ThreadEntryType            <chr> "post", "share", "post", "share", "post", "~
    ## $ ThreadId                   <chr> "0", "1316738339278663686", "0", "131961470~
    ## $ ThreadURL                  <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ TotalMonthlyVisitors       <dbl> 6e+09, 6e+09, 6e+09, 6e+09, 6e+09, 6e+09, 6~
    ## $ TwitterAuthorID            <chr> "2170837448", "827502320162201600", "217612~
    ## $ TwitterChannelRole         <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ TwitterFollowers           <dbl> 697, 882, 384, 3807, 2768, 30, 573, 589, 34~
    ## $ TwitterFollowing           <dbl> 199, 371, 294, 4058, 283, 96, 1125, 275, 20~
    ## $ TwitterReplyCount          <dbl> 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ TwitterReplyto             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ TwitterRetweetof           <chr> NA, "http://twitter.com/TexasGOP/statuses/1~
    ## $ TwitterRetweets            <dbl> 1, 0, 0, 0, 14, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
    ## $ TwitterTweets              <dbl> 20172, 93031, 1227, 136367, 1275, 1277, 542~
    ## $ TwitterVerified            <chr> "false", "false", "false", "false", "true",~
    ## $ Updated                    <chr> "2020-11-02T21:22:10.883+0000", "2020-10-24~
    ## $ `Reach(new)`               <dbl> 358, 0, 0, 2423, 10037, 0, 0, 0, 27532, 192~
    ## $ Copyright                  <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ PageTypeName               <chr> "Twitter", "Twitter", "Twitter", "Twitter",~
    ## $ RedditScore                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ Region                     <chr> "California", NA, "New York", NA, "New Mexi~
    ## $ RegionCode                 <chr> "USA.CA", NA, "USA.NY", NA, "USA.NM", "USA.~

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
    ## 1 RT @WarRoomPandemic #SteveBannon predicts a <U+2066>@realDonaldTrump<U+2069> victory in #Elec~
    ## 2 RT @WarRoomPandemic Bannon: @JoeBiden's campaign has no scalability. You can'~
    ## 3 Just like that...there was no more Flu ??...#COVID19 #coronavirus #plandemic ~
    ## 4 RT @JohnVetsResist1 Have had to do. As I sit here in a puddle of tears I'm fr~
    ## 5 RT @AprilDRyan Who would have ever thought that @realDonaldTrump & @MelaniaTr~
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

### Converting to a Network

To conduct later network analysis on the network model we’ve created, we
will need to convert it into a *graph object* that the tools from the
`igraph` package can understand. (Also, this is a good place to point
out that `igraph` functions refer to nodes as *vertices*.)

``` r
mention_graph <- graph_from_data_frame(weighted_edges, directed = TRUE) %>%
  delete_vertices("NA")  # igraph interprets missing values as a string

# Let's take a look!
E(mention_graph) %>% head()
```

    ## + 6/8038 edges from dc9d435 (vertex names):
    ## [1] ___Dustin__   ->jorgensen4potus __Ladderman421->danscavino     
    ## [3] __MrSwift__   ->hdrewgalloway   __Princess__Q ->joebiden       
    ## [5] __Princess__Q ->leandroshan     __ruthlesss__ ->bbcjonsopel

### Adding Node Attributes

In some situations, we may want a graph object to include attributes
associated with the nodes in a network that are not directly related to
the network structure (e.g., the number of followers a particular
Twitter user has, or the location they have added to their profile). To
add these variables to the `igraph` object, we first need to extract
them from the existing dataset and make sure that they are appropriately
formatted (i.e., in alphabetical order of node ID and without
duplicates).

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
  arrange(id) %>%
  .[-1,]          # Remove empty row created by isolates

# Before adding variables to the graph object, check and make sure that the lists line up
cbind(names(V(mention_graph)), nodes$id) %>% head()
```

    ##      [,1]             [,2]            
    ## [1,] "___Dustin__"    "___Dustin__"   
    ## [2,] "__Ladderman421" "__Ladderman421"
    ## [3,] "__MrSwift__"    "__MrSwift__"   
    ## [4,] "__Princess__Q"  "__Princess__Q" 
    ## [5,] "__ruthlesss__"  "__ruthlesss__" 
    ## [6,] "_ACHP"          "_ACHP"

``` r
vcount(mention_graph) == nrow(nodes)
```

    ## [1] TRUE

``` r
# If the above checks out, set vertex attributes from the node list
mention_graph <- mention_graph %>%
  set_vertex_attr("verified", index = V(mention_graph), nodes$verified) %>%
  set_vertex_attr("followers", index = V(mention_graph), nodes$followers) %>%
  set_vertex_attr("region", index = V(mention_graph), nodes$region)

# Let's take a look!
vertex.attributes(mention_graph) %>% glimpse()
```

    ## List of 4
    ##  $ name     : chr [1:7620] "___Dustin__" "__Ladderman421" "__MrSwift__" "__Princess__Q" ...
    ##  $ verified : logi [1:7620] FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ followers: num [1:7620] 67 186 1000 2032 305 ...
    ##  $ region   : int [1:7620] 44 5 44 29 5 9 NA NA 11 NA ...

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

    ##    [[ suppressing 10 column names '___Dustin__', '__Ladderman421', '__MrSwift__' ... ]]

    ##                                    
    ## ___Dustin__     . . . . . . . . . .
    ## __Ladderman421  . . . . . . . . . .
    ## __MrSwift__     . . . . . . . . . .
    ## __Princess__Q   . . . . . . . . . .
    ## __ruthlesss__   . . . . . . . . . .
    ## _ACHP           . . . . . . . . . .
    ## _asyan          . . . . . . . . . .
    ## _BayleeRobinson . . . . . . . . . .
    ## _Bec_Smith      . . . . . . . . . .
    ## _Burky          . . . . . . . . . .

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
    ## + 3158/3181 vertices, named, from de4ec33:
    ##    [1] #earlyvoting                             
    ##    [2] #vote                                    
    ##    [3] #mmcthemonitor                           
    ##    [4] #marymountmahattan                       
    ##    [5] #nyc                                     
    ##    [6] #politics                                
    ##    [7] #bidencrimefamilyexposed                 
    ##    [8] #debate2020                              
    ##    [9] #presidentialdebate                      
    ##   [10] #joebideniscorrupt                       
    ## + ... omitted several vertices
    ## 
    ## $`#earlyvoting`
    ## + 112/3181 vertices, named, from de4ec33:
    ##   [1] #election2020             #vote                    
    ##   [3] #nyc                      #bidenharris2020         
    ##   [5] #voteearly                #maga                    
    ##   [7] #trump                    #liberty                 
    ##   [9] #republicans              #leadright               
    ##  [11] #covid19                  #biden                   
    ##  [13] #trump2020                #elections               
    ##  [15] #florida                  #pennsylvania            
    ##  [17] #vegas                    #clarkcounty             
    ##  [19] #dontloseyourvoice        #voteblue                
    ## + ... omitted several vertices
    ## 
    ## $`#vote`
    ## + 861/3181 vertices, named, from de4ec33:
    ##   [1] #election2020                   #earlyvoting                   
    ##   [3] #nyc                            #politics                      
    ##   [5] #debate2020                     #presidentialdebate            
    ##   [7] #walkaway                       #biden2020                     
    ##   [9] #bidenharris2020                #trumppence2020                
    ##  [11] #voteearly                      #maga                          
    ##  [13] #trump                          #larrysharpe                   
    ##  [15] #libertarian                    #liberty                       
    ##  [17] #democrat                       #republicans                   
    ##  [19] #debates2020                    #blackvoicesfortrump           
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

    ## + 20/16051 edges from dfc401d (vertex names):
    ##  [1] beckya57       --#<U+03C9>         GrassrootsDNC  --#<U+03C9>        
    ##  [3] snowstormyou   --#<U+03C9>         eafinct        --#<U+6295><U+7968>
    ##  [5] NIHAustin      --#<U+6295><U+7968> oceanfootbaII  --#<U+6295><U+7968>
    ##  [7] MANMWA2        --#101nights        RealClelland   --#10milliondreams 
    ##  [9] JMelaniaKC     --#12moreyears      SamChen220     --#13minutes       
    ## [11] GlenSmi70330487--#1stamendment     GuiseElizabeth --#1u              
    ## [13] laborradionet  --#1u               Sheridan543    --#1u              
    ## [15] ProChoiceCA    --#2020census       WePoll_TheGame --#2020census      
    ## [17] 1andonlycarol  --#2020debate       CaseyAdamF     --#2020debate      
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
