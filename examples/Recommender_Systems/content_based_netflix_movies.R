
library(data.table)
library(Matrix)

netflix <- fread(paste0(
  examples_path, "/Recommender_Systems/data/netflix_titles.csv"
), encoding = "UTF-8")
netflix <- netflix[
  !is.na(cast) & cast != ""
  & !is.na(director) & director!= ""
]

movies <- netflix[type == "Movie"]
tv_shows <- netflix[type == "TV Show"]

rm(netflix)
gc()

movies[,description := NULL]

cast_attrs <- movies[,.(cast = unlist(strsplit(cast, ", "))), by = .(show_id)]
cast_attrs[,cast := paste0(cast, "_cast")]
cast_attrs[,rank := 1:.N, by = .(show_id)]
# cast_attrs[,x := ifelse(rank == 1, 5, ifelse(rank == 2, 3, 1))]  If we want to give more weights to some attributes
cast_attrs[,x := 1]
cast_attrs[,rank := NULL]

genres_attrs <- movies[,.(genre = unlist(strsplit(listed_in, ", "))), by = .(show_id)]
genres_attrs[,genre := paste0(genre, "_genre")]
genres_attrs [,rank := 1:.N, by = .(show_id)]
# genres_attrs [,x := ifelse(rank == 1, 5, 1)]
genres_attrs [,x := 1]
genres_attrs[,rank := NULL]

director_attrs <- movies[,.(director = unlist(strsplit(director, ", "))), by = .(show_id)]
director_attrs[,director := paste0(director, "_director")]
# director_attrs[,x := 5]
director_attrs[,x := 1]

cast_id <- unique(cast_attrs[,.(cast)])
setnames(cast_id, "cast", 'attr')

genres_id <- unique(genres_attrs[,.(genre)])
setnames(genres_id, "genre", 'attr')

director_id <- unique(director_attrs[,.(director)])
setnames(director_id, "director", 'attr')

all_attrs <- rbind(
  cast_id, genres_id, director_id
)
all_attrs[,attr_id := 1:.N]


cast_attrs <- all_attrs[cast_attrs, on = .(attr  = cast)]
genres_attrs <- all_attrs[genres_attrs, on = .(attr = genre)]
director_attrs <- all_attrs[director_attrs, on = .(attr = director)]

movies_coded <- rbind(cast_attrs, genres_attrs, director_attrs)
movies_id <- unique(movies_coded[,.(show_id)])
movies_id[,movie_id := 1:.N]
movies_coded <- movies_id[movies_coded, on = .(show_id)]

movies_coded <- movies_coded[,.(movie_id, attr_id, x)]
movie_contents <- sparseMatrix(
  i = movies_coded$movie_id,
  j = movies_coded$attr_id,
  x = movies_coded$x
)


movies_similarities <- cosine_similarity(movie_contents)
movies_similarities <- as(movies_similarities, "dgTMatrix")

movies_similarities <- data.table(target_movie = movies_similarities@i+1,
                                  cross_movie = movies_similarities@j+1,
                                  CosSim = movies_similarities@x)
movies_similarities <- movies_similarities[target_movie != cross_movie]
movies_similarities <- movies_similarities[order(target_movie, -CosSim)]


movies_similarities <- movies_id[,.(cross_show = show_id , cross_movie = movie_id)][movies_similarities, on = .(cross_movie)]
movies_similarities <- movies_id[,.(target_show = show_id , target_movie = movie_id)][movies_similarities, on = .(target_movie)]
movies_similarities <- movies[,.(cross_show = show_id, cross_title = title)][movies_similarities, on = .(cross_show)]
movies_similarities <- movies[,.(target_show = show_id, target_title = title)][movies_similarities, on = .(target_show)]
