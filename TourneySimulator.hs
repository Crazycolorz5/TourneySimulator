
data Match p = Match {p1From :: Result p, p2From :: Result p}
roundOne p1 p2 = Match {p1From = Bye p1, p2From = Bye p2}
data Result p = Winner (Match p) | Loser (Match p) | Bye p

data Won p = Won p
data Lost p = Lost p
data Report p = Report (Won p, Lost p)
instance (Show p) => Show (Report p) where
    show (Report (Won w, Lost l)) = show w ++ " played " ++ show l ++ "; W: " ++ show w ++ " L:" ++ show l
getWinner (Report (Won w, Lost l)) = w
getLoser (Report (Won w, Lost l)) = l

newtype Seed = Seeded Int deriving (Eq, Show)
instance Ord Seed where
    (Seeded a) <= (Seeded b) = a >= b

a1 = roundOne (Seeded 1) (Seeded 4)
a2 = roundOne (Seeded 2) (Seeded 3)
b1 = Match { p1From = Winner a1, p2From = Winner a2 }
l = Match { p1From = Loser a1, p2From = Loser a2 }
b2 = Match { p1From = Winner l, p2From = Loser b1 }
gfs1 = Match { p1From = Winner b1, p2From = Winner b2 }
gfs2 = Match { p1From = Winner b1, p2From = Winner gfs1 }

getResult :: (p -> p -> Report p) -> Match p -> Report p
getResult mu n = let
    evalResult res =  case res n of
        Winner m -> getWinner (getResult mu m)
        Loser m -> getLoser (getResult mu m)
        Bye p -> p
    p1 = evalResult p1From
    p2 = evalResult p2From
    in mu p1 p2

projected :: (Ord p) => p -> p -> Report p
projected p1 p2 = Report (Won $ max p1 p2, Lost $ min p1 p2)



--A bracket tournament is like an upside-down tree.
--So each match is a Match, and the winner goes on to one place, and the loser somewhere else.
{-
data Match p = GrandFinals ((p, p) -> Either (Match p) p) | Match { winnerMatch :: p -> Match p, loserMatch :: p -> Maybe (Match p) } 
--p is the player type.
--The loser might be eliminated.
--The reason they take p as an input is if the match result can depend on the player.

data Tournament p = Tourney [Match p]
--A tournament is the list of r1 matches.
-}

{-
An example -- a standard 4 person double elim bracket.

1 - A1 -v
4 -
       -B1 - GFsWinner

3 - A2 -^
2

A1- B2-v
A2-    --> GFsLoser
    B1-^

-}
{-
a1 = Match { winnerMatch = const $ b1, loserMatch = const $ Just l1}
a2 = Match { winnerMatch = const $ b1, loserMatch = const $ Just l1}
b1 = Match { winnerMatch = const $ gfs1, loserMatch = const $ Just l2}
l1 = Match { winnerMatch = const $ l2, loserMatch = const $ Nothing}
l2 = Match { winnerMatch = const $ gfs1, loserMatch = const $ Nothing}
gfs1 = GrandFinals (\(winners, losers) -> if play winners losers then Right winners else Left gfs2)
gfs2 = GrandFinals $ Right . uncurry winner

play :: p -> p -> Bool
play = undefined
winner :: p -> p -> p
winner = undefined


fourManDoubleElim = Tourney [a1, a2]

logExpectedBracket :: (Show p, Ord p) => Tournament p -> [(p, p)] -> IO ()
logExpectedBracket (Tourney matches) players = let r1 = zip matches players in do
    sequence $ do
        (match, p1, p2) <- r1
        putStrLn $ show p1 ++ " plays " ++ show p2 ++ ": " ++ max p1 p2 ++ " wins."

-}
