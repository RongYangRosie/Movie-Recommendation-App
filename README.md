# movie-recommendation-app

This is a final project of EN.601.429/629 Functional Programming in Software Engineering in Fall 2022.

## Overview

An Interactive command-line application built in OCaml for movie recommendation. Use three modes based on the three types of recommendation systems to generate the recommendation list:

- **Demographic Mode** 

  It offers generalized recommendations to every user based on movie popularity, which can be treated as a top list among all movies in our movie library. Users don’t need to input their movie preference list, and every user will get the same ordered list.

- **Content Based Mode** 

  It provides similar movies. First genreate TF-IDF vector for feature `overview`, and counter vector for features `keywords`, `genres`, `casts`, and `director`, and then combine these two vectors to calculate cosine similarity for movie pairs. Users need to input their movie preference list, and will get a similar recommended movie list.

- **Collaborative Mode** 

  It provides recommendations based on the rating data. Use biasSVD to predict rating for movies. Users need to input a rating list which contains the movie and corresponding rates.

## Build and run the project

1. Install python library sklearn 

   ```shell
   $pip3 install -U scikit-learn
   ```

2. Ensure that `csv` , `yojson`, `owl`, `sklearn` , `inquire` , `ppx_deriving`, `ounit2`, `bisect_ppx` and its dependencies are installed by running:

   ```shell
   $opam install csv yojson owl sklearn inquire ppx_deriving ounit2 bisect_ppx
   ```

3. Build project in the project directory, it will generate an executable file `main.exe`

   ```shell
   $dune build
   ```

   Then run the program with the following command

   ```shell
   $./_build/default/src/main.exe
   ```

   Or directly use the following command to build and run the program

   ```shell
   $dune exec ./src/main.exe
   ```

## Modules

- `movie.ml` - The movie data related data type `Movie.t` and functions
- `rating.ml` - The rating data related data type `Rating.t`
- `data_preprocessing.ml` - Parse csv file and load data, call `load_movie_data (credits_filename) (movie_filename)` to load movie dataset and `load_rating_data (rating_filename)` to load rating dataset
- `utils` - Common used functions
- `demographic_mode.ml` - Main logic of demographic mode, call `get_recommendations (movie_list:Movie.t) (n:int)` to get recommended movie list
- `content_based_mode.ml` - Main logic of demographic mode, call `get_recommendations ~(title:string) ~(n:int) ~(movie_list:Movie.t)` to get recommended movie list
- `collaborative_mode.ml` - Main logic of collaborative mode, call `get_recommendations ~(movie_list:Movie.t) ~(rating_list:Rating.t) ~(ur_list:Rating.t) ~(n:int)` to get recommended movie list
- `main.ml` - Main logic for interactive commend line interface

## References

- [Getting Started with a Movie Recommendation System | Kaggle](https://www.kaggle.com/code/ibtesama/getting-started-with-a-movie-recommendation-system)

