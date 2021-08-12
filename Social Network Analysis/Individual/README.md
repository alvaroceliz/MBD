# Social Network Analysis

## IMBD Movie and Show Insights

Analyzing the IMDB dataset with R library "igraph" (also existing in python) to gain insights about the social network between actors, movies, and genres. The relationship between actors or vertices is marked by whether they acted in the same movie. Insights gained:

- Over 90% of actors have less than 25 movies in common with other actors.
- Mark Davis is the actor with the highest degree centrality, or the most direct connections with other actors, an actor of adult films.
- Ron Jeremy is the actor with the highest betweeness, explained by the combination of amount of movies and genres an actor has in common with others, also an actor of adult films.
- Cameron Diaz has the highest closeness centrality, whihc shows how easily an actor can interact or have access to the rest of actors.
