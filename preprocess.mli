type movie = {
    movie_id: int;
    title: string;
    cast: string list;
    director: string list;
    keywords: string list;
    genres: string list;
    overview: string;    
    popularity: int;
    vote_count: int;
    vote_average: float;
}

type rating = {
    userid: int;
    movieid: int;
    rating: float;
}