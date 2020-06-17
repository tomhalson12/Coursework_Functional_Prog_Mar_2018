
------------------------- 

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <  y    = x : merge    xs (y:ys)
    | x == y    = x : merge    xs    ys
    | otherwise = y : merge (x:xs)   ys

minus :: Ord a => [a] -> [a] -> [a]
minus xs [] = xs
minus [] ys = []
minus (x:xs) (y:ys)
    | x <  y    = x : minus    xs (y:ys)
    | x == y    =     minus    xs    ys
    | otherwise =     minus (x:xs)   ys

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = msort (take n xs) `merge` msort (drop n xs)
  where
    n = length xs `div` 2

type Node = Int
type Map  = [(Node,Node)]

bothWays :: Node -> Map -> [Node]
bothWays _ [] = []
bothWays n ((x,y):xs)
    | n == x    = y : bothWays n xs
    | n == y    = x : bothWays n xs
    | otherwise =     bothWays n xs

type Location  = String
type Character = String

type Party = [Character]


------------------------- PART 1: Events

data Game  = Won | Game Node Party [Party]
  deriving (Eq,Show)


start :: Game
start =  Game 6 [] characters

end :: Game
end = Won

type Event = Game -> Game

applyAt :: Int -> (a -> a) -> [a] -> [a]
applyAt _ _ [] = []
applyAt i f (x:xs)
    | i >= length (x:xs) || i < 0 = error "Index out of bounds"
    | i == 0 = (f x) : xs
    | otherwise = x : (applyAt (i-1) f xs)

updateAt :: Node -> Party -> Party -> Event
updateAt _ _ _ Won = Won
updateAt m x y (Game n p r) = Game n p (applyAt m (merge y) (applyAt m (`minus` x) r))

update :: Party -> Party -> Party -> Event
update _ _ _ Won = Won
update x z y (Game n p r) = Game n (merge z (minus p x)) (applyAt n (merge y) (applyAt n (`minus` x) r))


------------------------- PART 2: Dialogues

data Dialogue = End     String
              | Choice  String  [( String , Dialogue )]
              | Action String Event

exitWords = ["X","x","Q","q","Exit","exit","Quit","quit"]

enumerate :: Int -> [String] -> String
enumerate n xs = unlines [ "  " ++ show i ++ ". " ++ x | (i,x) <- zip [n..] xs ]

dialogue :: Game -> Dialogue -> IO Game
dialogue g (End e) = do
                      putStrLn e
                      return g
dialogue g (Action a e) = do 
                            putStrLn a
                            return (e g)
dialogue g (Choice c sd) = do
                                    putStrLn c 
                                    putStr (enumerate 1 [s | (s,d) <- sd])
                                    choice <- getLine
                                    if (elem choice exitWords)
                                    then do 
                                           return g
                                    else if (read choice) > 0 && (read choice) <= (length sd)
                                    then do
                                           dialogue g (snd (sd !! ((read choice)-1) ))                                           
                                    else
                                     do
                                       return g


findDialogue :: Party -> Dialogue
findDialogue p = getDialogue p dialogues
    where
        getDialogue [] _ = (End "There is nothing we can do")
        getDialogue _ [] = (End "There is nothing we can do")
        getDialogue p ((x,d):ds)
            | p == x = d
            | otherwise = getDialogue p ds

------------------------- PART 3: The game loop

loop :: Game -> IO ()
loop Won = do
            return ()
loop (Game n p ps) = do
                    putStrLn ("You are in " ++ (locations !! n))
                    let ns = (bothWays n theMap)
                    let xs = [ (locations !! n) | n <- ns ]
                    putStr "You can travel to\n"
                    putStr (enumerate 1 xs)
                    putStr "With you are\n"
                    putStr (enumerate ((length ns)+1) p)
                    putStr "You can see\n"
                    putStr (enumerate ((length ns)+(length p)+1) (ps !! n))
                    putStrLn "What will you do?"
                    str <- getLine
                    putStrLn ""
                    if (elem str exitWords)
                    then do 
                           return ()
                    else do
                           let is = [ read x | x <- (words str)]
                           if (length is) == 1 && (is !! 0) > 0 && (is !! 0) <= (length ns)
                           then do -- moves locations
                                  loop (Game (ns !! ((read str)-1) ) p ps)
                           else do -- finds dialogue
                                 dia <- dialogue (Game n p ps) (findDialogue (msort [ (p++(ps!!n)) !! (x-(length ns)-1) | x <- is]))
                                 putStrLn ""
                                 loop dia

game :: IO ()
game = loop start

------------------------- PART 4: Solving the game

{-
  splits dialogue into actions and non actions, returns actions and recurses on non actions and keeps track of the choice number
-}
talk' :: Dialogue -> [(Event,[Int])]
talk' (End s) = []
talk' (Action s e) = [(e,[])]
talk' (Choice x sd) = (actionFound sd []) ++ (otherFound (noneActionList sd))
    where
        actionList sd = [ (e,[y]) | (y, (Action z e))  <- [ (y,d) | (y,(s,d)) <- zip [1..] sd, isAction d ] ]
        noneActionList sd = [ (d,[y]) | (y, d)  <- [ (y,d) | (y,(s,d)) <- zip [1..] sd, isAction d == False ] ]
        otherFound (((Choice x sd),y):yd) = (actionFound sd y) ++ ( attachList y (otherFound (noneActionList sd))) ++ otherFound yd
        otherFound (((End e),y):yd) = otherFound yd
        otherFound [] = []
        actionFound sd y = attachList y (actionList sd)
        attachList _ [] = []
        attachList y ((e,x):ls) = (e,(y++x)) : (attachList y ls)
        isAction (Action _ _) = True
        isAction _ = False

talk :: Dialogue -> [(Event,String)]
talk (End s) = [ (e,"") | (e,i) <- (talk' (End s))]
talk (Action s e) = [ (e,"") | (e,i) <- (talk' (Action s e))]
talk (Choice x sd) = [ (e,"\nIn the dialogue, choose " ++ (unwords (map show i))) | (e,i) <- (talk' (Choice x sd))]    

event :: String -> Event
event s _ = Game 0 ["Event: " ++ s] []

testDialogue :: Dialogue
testDialogue = Choice "Morpheus opens his palms"
 [("Take the blue pill", Action "" (event "You wake up in bed"))
 ,("Take the red pill",  Action "" (event "You are a battery"))]

testTalk' :: [(Game,[Int])]
testTalk' = [ (e Won,xs) | (e,xs) <- talk' testDialogue]

testTalk :: [(Game,String)]
testTalk = [ (e Won,str) | (e,str) <- talk testDialogue]


-------------------------

extend :: Map -> (Node,[Int]) -> [(Node,[Int])]
extend mp (n,path) = attachPath path [ (nd, [i]) | (i,nd) <- zip [1..] (bothWays n mp) ]
    where 
         attachPath _ [] = []
         attachPath y ((e,x):ls) = (e,(x++y)) : (attachPath y ls)

travel' :: Map -> [(Node,[Int])] -> [(Node,[Int])] -> [(Node,[Int])]
travel' _ p [] = p
travel' mp p ((cn, cpath):cs)
    | (length (extend mp (cn, cpath))) > 0 = travel' mp (p ++ [ (x,y)|(x,y)<-[(cn,cpath)], notPrev p (x,y) == False]) (cs ++ [ (n,np) | (n, np) <-(extend mp (cn, cpath)), notPrev p (n,np) == False] )
    | otherwise = p
    where
        notPrev p (n, i) = elem n (prevNodes p)
        prevNodes [] = []
        prevNodes p = [ p | (p,ps) <- p ] 

travel :: Map -> Game -> [(Game,String )]
travel mp (Game n p ps) = createList (Game n p ps) (travel' mp [] [(n,[])])
   where
        createList _ [] = []
        createList (Game nd p ps) ( (n,[]) : xs ) = ( (Game nd p ps), ("Stay in " ++ locations !! n)  ) : createList (Game n p ps) xs
        createList (Game nd p ps) ( (n,i) : xs ) = ( (Game n p ps), ("Travel to " ++ locations !! n) ++ ": "++ (unwords (map show (reverse i))) ) : createList (Game n p ps) xs


-------------------------

act :: Game -> [(Game,String)]
act (Game n p ps) = [ ((eve (Game n p ps)), ("\nTalk to " ++ (unwords (form party)) ++ str)) | (party, (eve, str)) <- (seperate [ (pr, talk d) | (pr,d) <- dialogues, members (allCharacters n p ps) pr]) , suitable (Game n p ps) eve]
    where
      allCharacters n x xs = merge x (xs !! n) 
      members xs    []  = False
      members xs (y:ys) = (member xs y) && (members xs ys || ys == [])
      member   [] _ = False
      member  (x:xs) y = (x == y) || (member xs y)
      seperate [] = []
      seperate ((pt,es):pts) = (sep pt es) ++ seperate pts
      sep _ [] = []
      sep pt (e:es) = (pt, e) : (sep pt es)
      form e
           | (length e) == 1 = e
           | otherwise = take ((length e)*2-1) (addAnd e)
      addAnd [] = []
      addAnd (e:es) = e : "and" : (addAnd es)

suitable :: Game -> Event -> Bool
suitable g e
    | (e g) == Won = True
    | otherwise = check g (e g)
    where -- checks if new members have been added anywhere
      check (Game n p ps) (Game n2 p2 ps2)
          | length(minus p2 p) > 0 = True
          | length (minus (msort(p2 ++ (concat ps2))) (msort(p ++ (concat ps)))) > 0 = checkNew p (minus (p2 ++ (concat ps2)) (p ++ (concat ps)))
          | otherwise = False
      checkNew _ [] = False
      checkNew [] _ = True
      checkNew part (x:xs) = (rev(elem x part)) || (checkNew part xs)
      rev True = False
      rev False = True

solve :: IO ()
solve = putStr ((unwords(form (words(solveLoop (start,"")))))++"\n")
    where
      solveLoop :: (Game,String) -> String
      solveLoop (Won, s) = s
      solveLoop (g, s) =  solveLoop (head (concat (actOn (map (joinStrTravel s) (travel theMap g)) ) ) )
      joinStrTravel str (gm, st) = (gm, str ++ st)
      joinStrAct str (gm, st) = (gm, str ++ st ++ "\n")
      actOn [] = []
      actOn ((gm, st):gs) = (map (joinStrAct st) (act gm)) : (actOn gs)
      form [] = []
      form (str:s) -- formats output
          | str == "Travel" || str == "Stay" = ("\n" ++ str) : (form s)
          | str == "Talk" = "\n  Talk" : (form s)
          | str == "In" = "\n  In" : (form s)
          | otherwise = str : (form s)

------------------------- Game data

characters :: [Party]
characters =
  [ ["Duke"]
  , ["Portal Gun"]
  , ["Priest"]
  , ["Lee"]
  , ["Chell","Cortana","Mario","Master Chief"]
  , ["Team Rocket"]
  , ["Peach","Rochelle"]
  ]

locations :: [Location]
locations =
  [ "You are not supposed to be here" -- 0
  , "Aperture Science" -- 1
  , "Church of Halo"   -- 2
  , "Macon"            -- 3
  , "Nintendo Land"    -- 4
  , "Pallet Town"      -- 5
  , "Princess Castle"  -- 6
  ]

theMap :: Map
theMap = [(1,5), (2,4), (2,6), (3,5), (4,5), (4,6)]

dialogues :: [(Party,Dialogue)]
dialogues =
 [ (["Mario"] , Choice "I need to save the Princess."
     [("Sure." ,          Action "Let's go." (update ["Mario"] ["Mario"] []))
     ,("Not right now." , Action "Ok."       (update ["Mario"] [] ["Mario"]))
     ])
 , (["Mario","Peach"] , Choice "Save me, Mario!"
    [("Sure." , Action "Thank you for bringing me my hero. Now I can conveniently leave this hat behind." (update ["Mario","Peach"] [] ["Baseball Cap"]))
    ,("Not right now." , End "Mario, pls.")])
 , (["Peach"] , End "That's *Princess* Peach to you, please. And where's my Mario?")
 , (["Master Chief"] , Choice "I want to marry Cortana. Can you escort us to the Church of Halo?"
     [("Sure." ,          Action "Let's go." (update ["Master Chief"] ["Master Chief"] []))
     ,("Not right now." , Action "Ok."       (update ["Master Chief"] [] ["Master Chief"]))
     ])
 , (["Cortana"] , Choice "I must go with Master Chief."
     [("Sure." ,          Action "Let's go." (update ["Cortana"] ["Cortana"] []))
     ,("Not right now." , Action "Ok."       (update ["Cortana"] [] ["Cortana"]))
     ])
 , (["Master Chief","Priest"] , End "I can't marry you without your bride-to-be.")
 , (["Cortana","Priest"] , End "I can't marry you without your husband-to-be.")
 , (["Priest"] , Choice "Welcome, my child. Have you accepted Master Chief as your savior?"
     [("Hail Master Chief (Blessed Be His Name)" , End "")])
 , (["Cortana","Master Chief","Priest"] , Choice "Do you, Master Chief, accept Cortana to be your beloved bride?" 
      [("I don't", End "The Wedding is cancelled"),
       ("I do", Choice "And do you, Cortana, take Master Chief to be your beloved Husband?" 
        [("I don't", End "The Wedding is cancelled"),
         ("I do", Action "What a beautiful wedding said the bridesmaid to the waiter. But what a shame, that there's some child lurking nearby." (update ["Cortana","Master Chief","Priest"] [] ["Clementine (hiding)"]) )
   ])])
 , (["Baseball Cap"] , Choice "It's a bit grubby, shall I take it?"
     [("Sure." ,          Action "Let's go." (update ["Baseball Cap"] ["Baseball Cap"] []))
     ,("Not right now." , Action "Ok."       (update ["Baseball Cap"] [] ["Baseball Cap"]))
     ])
 , (["Clementine (hiding)"] , End "I'm scared. Where are my parents?")
 , (["Baseball Cap", "Clementine (hiding)"] , Choice "Give the girl the hat?"
    [("Sure." , Action "I feel safe." (update ["Baseball Cap","Clementine (hiding)"] ["Clementine"] []))
    ,("Not right now." , End "")
    ])
 , (["Duke"] , End "Time to k*** a** and chew bubble gum. And I'm all outta gum.")
 , (["Clementine"] , Choice "Will you help me find my parents?"
      [("What do they look like?", Choice "My father's name is Lee" 
        [("I asked what do they look like!", End "Sorry")
        ,("I know him, Let's go!", Action "Yay!" (update ["Clementine"] ["Clementine"] []))
      ])
      ,("Do you know your address?", Choice "I can't remember, I think it rhymes with Bacon" 
        [("How do you not know your own address?", End "Sorry!"),
         ("Are you thinking of Macon?", Choice "Yes! That's it! Do you know where it is?"
          [("Sure, I can take you there", Action "Yay!" (update ["Clementine"] ["Clementine"] []))
          ,("I don't know how to get there", End "Okay then")
          ])
        ,("Are you hungry?", Choice "Yes! Do you have any chocolate?" 
          [("I don't", End "Okay")
          ,("I can go find some", Action "Thanks, I'll stay here in case my parents come back" (update ["Clementine"] [] ["Clementine"]))
   ])])])
 , (["Clementine","Lee"] , Choice "GIVE ME BACK CLEMENTINE!"
     [("Sure...", Action "" (update ["Clementine","Lee"] ["Zombie Lee"] []))
     ])
 , (["Lee"] , End "Clem? Clem, where are you?!")
 , (["Zombie Lee"] , Choice "Uuurrurrhgghghhghgg."
     [("This way." ,  Action "Urg" (update ["Zombie Lee"] ["Zombie Lee"] []))
     ,("Not today." , Action "Hhuuuurgh" (update ["Zombie Lee"] [] ["Zombie Lee"]))
     ])
 , (["Rochelle"] , End "Girl, you should pray there aren't no Zombies around.")
 , (["Rochelle", "Zombie Lee"] , Action "What?! A zombie? You've left me for dead!" (update ["Rochelle","Zombie Lee"] [] ["Pikachu"]))
 , (["Chell"] , Choice "I've just got a volunteering position at Aperture Science. Can you help me find it? I'm not good with directions."
     [("This way." ,  Action "" (update ["Chell"] ["Chell"] []))
     ,("Not today." , Action "" (update ["Chell"] [] ["Chell"]))
    ])
 , (["Chell","Portal Gun"] , Action "This is your fault. It didn't have to be like this. I'm not kidding, now! Turn back, or I will kill you! I'm going to kill you, and all the cake is gone! You don't even care, do you? This is your last chance! ." (update ["Chell","Portal Gun"] [] [] . updateAt 4 ["Team Rocket"] ["Ash"]))
 , (["Team Rocket"] , End "Oh, prepare for trouble, that's what they should do. And make it double, we're grabbing Pikachu.") 
 , (["Pikachu"] , Choice "Pika-Pika"
     [("*throw pokeball*"  , Action "" (update ["Pikachu"] ["Pikachu"] []))
     ,("Nope." ,             Action "" (update ["Pikachu"] [] ["Pikachu"]))
     ])
 , (["Ash", "Pikachu"] , Action "You win." (\_ -> Won))
 , (["Pikachu","Team Rocket"] , End "Hey, look at this! Get a load! Let's grab- ALL GLORY TO THE HYPNOTOAD")
 , (["Portal Gun"] , End "I am an inanimate object. What did you expect?")
 ]