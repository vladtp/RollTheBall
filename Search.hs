{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = NullNode | GraphNode {
    nodeState :: s,
    prevAction :: Maybe a,
    nodePar :: Node s a,
    nodeDepth :: Int,
    nodeChildren :: [Node s a]
    } deriving (Eq)

instance Show (Node s a) where
    show node = show $ nodeDepth node

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
-- nodeState :: Node s a -> s
-- nodeState (GraphNode state _ _ _ _) = state

nodeParent :: (Eq s) => Node s a -> Maybe (Node s a)
nodeParent (GraphNode _ _ parent _ _) = Just parent
nodeParent NullNode = Nothing

-- nodeDepth :: Node s a -> Int
-- nodeDepth (GraphNode _ _ _ depth _) = depth

nodeAction :: (Eq a, Eq s) => Node s a -> Maybe a
nodeAction  = prevAction

-- nodeChildren :: Node s a -> [Node s a]
-- nodeChildren (GraphNode _ _ _ _ children) = children

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

createNode :: (ProblemState s a, Eq s) => s -> a -> Node s a -> Node s a
createNode state action parent = thisNode
    where
        thisNode = GraphNode state (Just action) parent depth children
        depth = nodeDepth parent + 1
        children = [createNode newState newAction thisNode | suc <- successors state, let newState = snd suc, let newAction = fst suc, newState /= state]

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace state = thisNode
    where
        thisNode = GraphNode state Nothing NullNode 0 children
        children = [createNode newState newAction thisNode | suc <- successors state, let newState = snd suc, let newAction = fst suc, newState /= state]

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

recBfs :: Ord s => [Node s a] -> Node s a -> [([Node s a], [Node s a])]
recBfs nodeList node = (addedNodes, newList) : recBfs newList (head newList)
    where
        addedNodes = nodeChildren node
        newList = tail nodeList ++ addedNodes

bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs initialNode = ([initialNode], [initialNode]) : recBfs [initialNode] initialNode



{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

recBidir :: (Eq s, Eq a) => [([Node s a], [Node s a])] -> [([Node s a], [Node s a])] -> (Node s a, Node s a)
recBidir s1 s2
    | null res = recBidir (tail s1) (tail s2)
    | otherwise = head res
    where
        res = [(n1, n2)| n1 <- f1, n2 <- f2, nodeState n1 == nodeState n2]
        f1 = fst $ head s1
        f2 = snd $ head s2

bidirBFS :: (Eq s, Eq a) => Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS n1 n2 = recBidir (bfs n1) (bfs n2)
    


{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extrPath :: (Eq s, Eq a) => [(Maybe a, s)] -> Node s a -> [(Maybe a, s)]
extrPath list node
    | node == NullNode = reverse list
    | otherwise = extrPath (list ++ [newElem]) parent
    where
        parent = nodePar node
        newElem = (action, state)
        action = prevAction node
        state = nodeState node

extractPath :: (Eq s, Eq a) => Node s a -> [(Maybe a, s)]
extractPath = extrPath [] 



{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}
reverseAnAction :: (Maybe a, s) -> (Maybe a, s)
revreseAnAction (Just a, s) = (Just newA, newS)
    where
        (newA, newS) = reverseAction (a, s)
reverseAnAction = id

reversePath :: [(Maybe a, s)] -> [(Maybe a, s)]
reversePath lst = reverse $ map reverseAnAction lst

solve :: (Eq a, Eq s) => (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve initialState finalState = extractPath (fst commonNodes) ++ reversePath (extractPath (snd commonNodes))
    where
        commonNodes = bidirBFS (createStateSpace initialState) (createStateSpace finalState)
