module Chain where

import Test.QuickCheck

retargetInterval = 4
threshold = ceiling ((fromIntegral retargetInterval)*0.75)

data Signal = NoSignal | Signal
  deriving (Eq, Show)
toInt :: Signal -> Int
toInt NoSignal = 0
toInt Signal = 1

data Block = Block { signal :: Signal
                   , violatesNewRule :: Bool
                   , pow :: Int}
  deriving (Eq, Show)
instance Arbitrary Block where
  arbitrary = elements [ Block NoSignal True 1, Block Signal True 1, Block NoSignal False 1, Block Signal False 1 ]

type Blockchain = [Block]

data Parameters = Parameters { startheight :: Int
     , timeoutheight :: Int
     , lockinontimeout :: Bool }
  deriving (Eq, Show)
instance Arbitrary Parameters where
  arbitrary = Parameters <$> elements [0] <*> elements [retargetInterval] <*> arbitrary


data State = Defined | Started | MustSignal | LockedIn | Active | Failed
  deriving (Eq,Show)

getChainAtHeight :: Blockchain -> Int -> Blockchain
getChainAtHeight chain height = fst (splitAt height chain)

genHeights :: Gen Int
genHeights = choose (0,2*retargetInterval)

prop_getChainAtHeight :: Int -> Blockchain -> Bool
prop_getChainAtHeight height chain =
  let
    newchain = getChainAtHeight chain height
    len = if height > length chain
            then length newchain == length chain
          else length newchain == height
    elem = if length newchain > 0 && length chain > 0
             then (head chain) == (head newchain)
           else
             True
  in
    len && elem

countSignals :: Blockchain -> Int
countSignals chain = sum (map (\block -> (toInt (signal block))) (getChainAtHeight (tail (reverse chain)) retargetInterval))

stateFromPrevState :: State -> Blockchain -> Parameters -> State
stateFromPrevState Defined chain params =
  if (length chain >= (startheight params))
    then Started
  else Defined
stateFromPrevState Started chain params =
  let
    nSignals = countSignals chain
    blockHeight = length chain
  in
    if nSignals >= threshold
      then LockedIn
    else if (lockinontimeout params && blockHeight + retargetInterval >= (timeoutheight params))
      then MustSignal
    else if (blockHeight >= (timeoutheight params))
      then Failed
    else
      Started
stateFromPrevState MustSignal _ _ = LockedIn
stateFromPrevState LockedIn _ _ = Active
stateFromPrevState Active _ _ = Active
stateFromPrevState Failed _ _ = Failed

state :: Blockchain -> Parameters -> State
state chain params =
  let
    blockHeight = length chain
  in
  if blockHeight `mod` retargetInterval /= 0
    then state (getChainAtHeight chain (blockHeight - 1)) params
  else
    let
      prevState =
        -- according to newer PR
        if blockHeight == 0
          then Defined
        else state (getChainAtHeight chain (blockHeight - retargetInterval)) params
    in
      stateFromPrevState prevState chain params

prop_state chain params =
  let
    actualState = state chain params
  in
  if (timeoutheight params) - (startheight params) < 2*retargetInterval
    then True
  else
  if lockinontimeout params
    then
      if (length chain) >= (timeoutheight params)
        then actualState == LockedIn || actualState == Active
      else
        actualState == Defined || actualState == Started || actualState == MustSignal
  else
    if (length chain) >= (timeoutheight params)
      then actualState == LockedIn || actualState == Active || actualState == Failed
    else
      actualState == Defined || actualState == Started || actualState == Failed

signalBlock = Block {signal = Signal, violatesNewRule = False, pow = 1}
noSignalBlock = Block {signal = NoSignal, violatesNewRule = False, pow = 1}
test_state =
  let
    params1 = Parameters {startheight = 0, timeoutheight = 3*retargetInterval, lockinontimeout = False}
    params2 = Parameters {startheight = 0, timeoutheight = 3*retargetInterval, lockinontimeout = True}
    chain1 = take 8 (repeat noSignalBlock)
    chain2 = take 12 (repeat noSignalBlock)
    chain3 = take 8 (repeat noSignalBlock) ++ take 4 (repeat signalBlock)
    chain4 = take 4 (repeat noSignalBlock) ++ take 4 (repeat signalBlock)
    chain5 = take 4 (repeat noSignalBlock) ++ take 4 (repeat signalBlock) ++ take 4 (repeat noSignalBlock)
    chain6 = take 4 (repeat noSignalBlock) ++ take 3 (repeat signalBlock) ++ [noSignalBlock]
    chain7 = take 4 (repeat noSignalBlock) ++ take 4 (repeat noSignalBlock) ++ take 2 (repeat signalBlock) ++ take 2 (repeat noSignalBlock)
  in state chain1 params1 == Started
    && state chain1 params2 == MustSignal

    && state chain2 params1 == Failed
    && state chain2 params2 == LockedIn

    && state chain3 params1 == LockedIn
    && state chain3 params2 == LockedIn

    && state chain4 params1 == LockedIn
    && state chain4 params2 == LockedIn

    && state chain5 params1 == Active
    && state chain5 params2 == Active

    && state chain6 params1 == LockedIn
    && state chain6 params2 == LockedIn

    && state chain7 params1 == Failed

isBlockValid :: State -> Block -> Blockchain -> Bool
isBlockValid Defined block _ = True
isBlockValid Started block _ = True
isBlockValid MustSignal block chain =
  let
    count = (length chain) `mod` retargetInterval + 1
    cutChain = reverse (take count (reverse chain))
    nonsignal = sum (map (\block -> if (signal block == NoSignal) then 1 else 0) cutChain)
  in
    nonsignal + threshold <= retargetInterval

isBlockValid LockedIn block _ = True
isBlockValid Active block _ = not (violatesNewRule block)
isBlockValid Failed block _ = True

isValid :: Blockchain -> Parameters -> Bool
isValid chain params =
  let
    s = state chain params
  in
    if length chain == 0
      then True
    else
      (isBlockValid s (last chain) chain) &&
        isValid (getChainAtHeight chain ((length chain) - 1)) params

prop_IsValid chain =
  let
    mkPair (start, end) = (Parameters start end True, Parameters start end False)
    pairs = map mkPair [ (0, 2*retargetInterval), (0, 3*retargetInterval), (retargetInterval, 3*retargetInterval), (0, 4*retargetInterval)]
    cond (params1, params2) =
      if isValid chain params1
        then isValid chain params2
      else if (isValid chain params2) && ((state chain params2 == LockedIn) || (state chain params2 == Active))
        then isValid chain params1
      else True
  in
    foldl (\acc params -> acc && cond params) True pairs

-- different timouts
prop_IsValid2 chain =
  let
    mkPair (start, end) = (Parameters start end True, Parameters start end False)
    i2 = 2*retargetInterval
    i3 = 3*retargetInterval
    i4 = 4*retargetInterval
    pairs = [
      (Parameters 0 i2 False, Parameters 0 i3 False),
      (Parameters 0 i2 False, Parameters 0 i4 False),
      (Parameters 0 i2 True, Parameters 0 i3 False),
      (Parameters 0 i2 True, Parameters 0 i4 False)]
    cond (params1, params2) =
      if isValid chain params1 && ((state chain params1 == LockedIn) || (state chain params1 == Active))
        then isValid chain params2 && ((state chain params2 == LockedIn) || (state chain params2 == Active))
      else True
  in
    foldl (\acc params -> acc && cond params) True pairs
