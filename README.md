# movie-recommendation-app #
## overview ##   
Interactive command-line application built in OCaml for movie recommendation. Use three modes based on the three types of recommendation systems to generate the recommendation list:
* Demographic Mode
It offers generalized recommendations to every user based on movie popularity, which can be
treated as a top list among all movies in our movie library. Users don’t need to input their movie
preference list, and every user will get the same ordered list.
* Content Based Mode 
It recommends similar items based on certain features, e.g., actors, directors and tags, etc. Users
need to input their movie preference list, and will get a similar recommended movie list.
* Collaborative Mode 
It provides recommendations based the matched persons with similar interests. Users need to
input a list which contains the movie and corresponding rates.

## Authors ##
Meihan Lin, Rong Yang

## Movie and recommendation ##  
Every movie has a unique identifiers for objects(title, cast, director, keywords, genres, overview, popularity, vote_count, vote_average). In mode1, user could input a number to get top N movies. In mode2, we use the following features overview, cast, crew, keywords and genres
to build our recommendation algorithm. In mode3, predict the rating for unwatched movie based on the scores provided by users with similar movie preferences.  

## Python Library: Numpy ##  
$ sudo apt-get install python-numpy

## Usage ##  
To compile the project
$dune build

Then you can run the single executable, ./_build/default/main.exe in 3 different modes    
$dune exec ./_build/default/main.exe 

xxxx

## Testing ##  
To test our libraries, run  
$ dune test

## Dependencies ##  
* ounit2
* Csv
* yojson
* owl
* sklearn
* bisect_ppx
* inquire
* ppx_deriving
