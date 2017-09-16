
# coding: utf-8

# In[3]:

import numpy as np
import numpy.ma as ma
import pandas as pd
import warnings
warnings.filterwarnings('ignore')


# In[15]:

import os
data_dir = 'C:/Users/a1381/Desktop/UIUC/Python/Project/Recommender_system/ml-latest-small/'

ratings_file = os.path.join(data_dir, 'ratings.csv')
movies_file = os.path.join(data_dir, 'movies.csv')

ratings = pd.read_csv(ratings_file)
movies = pd.read_csv(movies_file)


# In[16]:

print("movie shape: ", movies.shape)
movies.tail()


# In[17]:

print("rating shape: ", ratings.shape)
ratings.tail()


# In[18]:

# ravel     http://blog.csdn.net/lanchunhui/article/details/50354978
len(pd.unique(ratings['userId'].ravel()))


# In[19]:

mv_lens = pd.merge(movies, ratings)
mv_lens.head()


# In[20]:

print(len(mv_lens))


# In[21]:

# Display the most commonly rated movies
mv_lens.title.value_counts().head()


# In[22]:

mv_lens.rating.isnull().sum()


# In[23]:

# Make a new Data structure that holds the movie, number of ratings, and the average rating.
# agg   https://pandas.pydata.org/pandas-docs/stable/generated/pandas.core.groupby.DataFrameGroupBy.agg.html
mv_stats = mv_lens.groupby('title').agg({'rating': [np.size, np.mean]}) # it can accept a list of functin e.g. [np.size, np.mean]

# Number of ratings to consider top movie
rating_count = 20

# Display most popular movies.
top_movies = mv_stats['rating']['size'] >= rating_count
mv_stats[top_movies].sort_values(by=('rating', 'mean'), ascending=False).head(10)


# In[24]:

mvrs = ratings.groupby(by='movieId').size().sort_values(ascending=False)
print('Type of mvrs: ', type(mvrs))
tmp_ratings = ratings.ix[mvrs[mvrs > 20].index].dropna()
len(tmp_ratings)


# In[25]:

tmp_df = tmp_ratings.pivot(index='userId', columns='movieId', values='rating')


# In[26]:

the_data = tmp_df.applymap(lambda x: 1 if x > 3 else 0).as_matrix()
print(the_data.shape)
the_data[1:10, 1:10]


# In[27]:

def cosine_similarity(u, v):
    return(np.dot(u, v)/np.sqrt((np.dot(u, u) * np.dot(v, v))))


# In[28]:

a = np.array([1, 1, 1, 0, 0])
b = np.array([0, 0, 0, 1, 1])
c = np.array([0, 1, 0, 1, 1])

print('cosine similarity(a, b) = {0:4.3f}'.format(cosine_similarity(a, b)))
print('cosine similarity(a, c) = {0:4.3f}'.format(cosine_similarity(a, c)))
print('cosine similarity(b, c) = {0:4.3f}'.format(cosine_similarity(b, c)))
print('cosine similarity(a, a) = {0:4.3f}'.format(cosine_similarity(a, a)))


# In[30]:

# user-movie matrix
x = the_data

# create a fake user
y = np.zeros(the_data.shape[1], dtype=np.int32)
y[6] = 1 ; y[10] = 1; y[15] = 1; y[64] = 1; y[136] = 1
y[180] = 1; y[230] = 1; y[339] = 1; y[622] = 1; y[703] = 1


# Add a special index column to map the row in the x matrix to the userIds
tmp_df.tmp_idx = np.array(range(x.shape[0]))


# In[31]:

sims = np.apply_along_axis(cosine_similarity, 1, x, y)
# Return the maximum of an array or maximum along an axis, ignoring any NaNs
mx = np.nanmax(sims)

# Find the best matching user
usr_idx = np.where(sims==mx)[0][0]

# Print the first thirty reviews of test user and matched user.
print(y[:30])
print(x[usr_idx, :30])

print('\nCosine Similarity(y, x[{0:d}]) = {1:4.3f}'       .format(usr_idx, cosine_similarity(y, x[usr_idx])), end='\n\n')

# Now we subtract the vectors
# (any negative value is a movie to recommend)
mov_vec = y - x[usr_idx]

# We want a mask aray, so we zero out any recommended movie.
mov_vec[mov_vec >= 0] = 1
mov_vec[mov_vec < 0] = 0

print(mov_vec[:30])


# In[32]:

# Print out the number of movies we will recommend.
print('\n{0} Movie Recommendations for User = {1}'       .format(mov_vec[mov_vec == 0].shape[0], 
              tmp_df[tmp_df.tmp_idx == usr_idx].index[0]))


# In[33]:

# Get the columns (movieIds) for the current user
mov_ids = tmp_df[tmp_df.tmp_idx == usr_idx].columns


# In[34]:


# Now make a masked array to find movies to recommend
# values are the movie ids, mask is the movies the most
# similar user liked.

ma_mov_idx = ma.array(mov_ids, mask = mov_vec)
mov_idx = ma_mov_idx[~ma_mov_idx.mask]


# In[35]:

# Now make a DataFrame of the moves of interest and display

mv_df = movies.ix[movies.movieId.isin(mov_idx)].dropna()

print(60*'-')

for movie in mv_df.title.values:
    print(movie)

print(60*'-', end='\n\n')


# In[ ]:



