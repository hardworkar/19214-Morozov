import Data.List
import Data.Maybe
data Track = Track { name_t :: String,
                     duration_t :: Float,
                     author_t :: String,
                     album_t :: String
                     } deriving (Show)
 
data Album = Album { name_a :: String,
                     tracks_a :: [Track],
                     author_a :: String
                    } deriving (Show)

data MusicGroup = MusicGroup { name_g :: String,
                              albums_g :: [Album],
                              tracks_g :: [Track]
                            } deriving (Show)

data User = User { firstName :: String,
                   lastName :: String,
                   email :: String,
                   password :: String,
                   favourites_u :: [Track],
                   albums_u :: [Album],
                   groups_u :: [MusicGroup]
                   } deriving (Show)

findTrackByNameInGroup :: String -> MusicGroup -> Maybe Track
findTrackByNameInGroup name (MusicGroup _ _ groupTracks) = find (\track -> (name_t track) == name) groupTracks



findTrackByName :: String -> [MusicGroup] -> [Track]
findTrackByName name groups = map fromJust (filter (\x -> isJust(x)) (map (\x -> (findTrackByNameInGroup name x)) groups))

addTrack :: User -> Track -> User
addTrack (User fn ln em pw tracks al gr) newTrack = User fn ln em pw (newTrack:tracks) al gr

addGroup :: User -> MusicGroup -> User
addGroup (User fn ln em pw tr al groups) newGroup = User fn ln em pw tr al (newGroup:groups) 

addAlbum :: User -> Album -> User
addAlbum (User fn ln em pw tr albums gr) newAlbum = User fn ln em pw tr (newAlbum:albums) gr 

----------------------------------------------------------

vasya = User "Vasya" "Petrov" "vassssa@g.ru" "123" [track1,track2] [album1] [musicBand01]

track1 = Track "Filosofskoe egg" 50.2 "kora0081" "Harry Povar"
track2 = Track "Kubok borczha" 23 "kora0081" "Harry Povar"

album1 = Album "Harry Povar" [track1,track2] "kora0081"

musicBand01 = MusicGroup "kora0081" [album1] [track1,track2]
musicBand02 = MusicGroup "pupodel" [album1] [track1,track2]

all_groups = [musicBand01,musicBand02]
