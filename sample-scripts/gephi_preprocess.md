Brandwatch Data Preprocessing for Gephi
================
Tiernan Cahill
26/05/2021

# Importing Excel Data

We begin by using the `read_excel` function from the `readxl` library to
import the raw Brandwatch data output from the provided XLS data file
into a dataframe. This function has options to selectively import
particular columns, and only two are strictly necessary to import the
network into Gephi, but we will import the entire dataset for now in
case we decide to make use of different sets of variables (e.g.,
including \# of followers as a node-level measurement).

``` r
lab <- read_excel("../data/lab.xlsx")
```

# Creating an Edgelist

In order to import data into Gephi, we need to convert the list of
documents (i.e., Tweet) in the spreadsheet into an *edgelist*. While
there are other import formats available in Gephi (e.g., *adjacency
matrix*), an edgelist is the most structurally similar to the data we
alrady have.

Two common ways of representing Twitter data as network models involve
using edges in the network to operationalize *replies* and *mentions*
respectively. In both cases, the nodes in the network represent *users*.

## Reply Networks

In order to create an edgelist based on *replies*, we need to isolate
two columns from the dataset, containing the username of the *author* of
each tweet, and (where relevant) the username of the author of the reply
thread in which that tweet appeared.

The contents of these columns need to be **cleaned**, to ensure that
usernames are formatted the same way throughout the dataset, and the
columns themselves need to be designated as `Source` and `Target`for
Gephi’s importer.

The edgelist can then be exported as a CSV file. Because Gephi often
struggles to handle large datasets, we will only export a sample (*n* =
1000) of the full edgelist. Note that the result will be a *directed*
network between users based on replies.

``` r
replies <- lab %>% 
  select(Source = Author, Target = "Thread Author", Date) %>%
  mutate(Time = as.numeric(as.POSIXct(Date)) * 1000, .keep = "unused") # Convert date to Gephi's preferred (weird) format

replies_small <- replies %>% 
  sample_n(n) %>% 
  write_csv("../data/gephi/reply_sample_edges.csv", na = "")
```

## Mention Networks

While each tweet can only be marked as a reply to a single thread
author, there might be multiple other users *mentioned* in the tweet,
which can also be used as the basis for a network model.

In this case, in addition to isolating `Source` and `Target` columns, as
before, we will also need to split up rows, since each tweet can
represent multiple connections between users.

``` r
mentions <- lab %>%
  select(Source = Author, Target = "Mentioned Authors", Date) %>%
  separate_rows(Target, sep = ", ") %>%
  mutate(Target = str_replace_all(Target, "@", "")) %>%
  mutate(Time = as.numeric(as.POSIXct(Date)) * 1000, .keep = "unused") 

mentions_small <- mentions %>%
  sample_n(n) %>% 
  write_csv("../data/gephi/mention_sample_edges.csv", na = "")
```

# Creating a Node List

In addition to the edges that connect users in the network (representing
either replies or mentions), we may wish to retain some additional
*node-level* information about the users themselves, such as where they
live, their number of followers, and whether or not they are verified on
Twitter.

``` r
# Retain Region, Followers, and Verified status
users <- lab %>% 
  select(Id = Author, Verified = "Twitter Verified", Followers = "Twitter Followers", Region) %>%
  distinct(Id, .keep_all = T)

# Create nodelists to match the sample edgelists produced above and export
users_replies_small <- users %>%
  filter(Id %in% unique(unlist(replies_small))) %>%
  write_csv("../data/gephi/reply_sample_nodes.csv", na = "")

users_mentions_small <- users %>%
  filter(Id %in% unique(unlist(mentions_small))) %>%
  write_csv("../data/gephi/mention_sample_nodes.csv", na = "")
```

# Creating an Adjacency Matrix

Another way of representing Twitter data as a network is to model the
*co-incidence* of hashtags (i.e., how often different hashtags appear
together in the same tweet). In this case, *nodes* represent hashtags
rather than users and the network is *undirected*. This approach can be
used to understand how concepts and communities represented by hashtags
overlap and intersect.

Since this format will result in a large number of edges generated per
tweet (since every hashtag is connected to every other hashtag in the
same document), we will take this opportunity to try generating an
adjacency matrix, which can also be imported into Gephi.

An adjacency matrix has a row and column for every node in the network,
with the cell *a*<sub>*i*, *j*</sub> containing the weight of the edge
between the nodes *i* and *j*.

``` r
htags <- lapply(lab$Hashtags, str_split, pattern = ", ")
htags <- lapply(htags, unlist, recursive = F)

# Credit to Martin Stefan for the matrix generation code below 
# [https://tinyurl.com/7sd76m8p]

# First create a list of unique tags in the dataset to set the dimensions of the matrix
utags <- unique(unlist(htags))

htag_mat <- matrix(0, length(utags), length(utags))
rownames(htag_mat) <- utags
colnames(htag_mat) <- utags

# Then, iterate through the list of tweets and add one to the adjacency matrix for each co-occurence detected
for(t in 1:length(htags)) {
  tags <- htags[[t]]
  
  if(length(tags) == 1) next()
  
  htag_mat[tags, tags] <- htag_mat[tags, tags] + 1
}

# Set the diagonal of the matrix to 0 (hashtags are not adjacent to themselves)
diag(htag_mat) <- 0

# Sample from the matrix ensuring that the rows continue to match the columns
sample_i <- sample(1:nrow(htag_mat), size = n, replace = F)
htag_mat_small <- htag_mat[sample_i, sample_i]
write.csv(htag_mat_small, "../data/gephi/hashtag_sample_matrix.csv")
```
