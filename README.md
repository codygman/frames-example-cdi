frames example using US chronic disease indicators
===================================================

### 1. Clone this repo

### 2. Follow along

First let's take a look at the function which generates types based on csv files, tableTypes:

```haskell
λ> :t tableTypes
tableTypes
  :: String
     -> FilePath
     -> template-haskell-2.11.0.0:Language.Haskell.TH.Lib.DecsQ
```

It takes a String that will be the name of the type generated, a filepath to the csv, and then returns a template Haskell DecsQ value that is built at compile time.

Let's see what types were generated from `data/U.S._Chronic_Disease_Indicators__CDI_.csv`:

```
λ> :i Row
type Row =
  Record
    '["YearStart" :-> Int, "YearEnd" :-> Int, "LocationAbbr" :-> Text,
      "LocationDesc" :-> Text, "DataSource" :-> Text, "Topic" :-> Text,
      "Question" :-> Text, "Response" :-> Text, "DataValueUnit" :-> Text,
      "DataValueTypeID" :-> Text, "DataValueType" :-> Text,
      "DataValue" :-> Double, "DataValueAlt" :-> Double,
      "DataValueFootnoteSymbol" :-> Text, "DatavalueFootnote" :-> Text,
      "LowConfidenceLimit" :-> Double, "HighConfidenceLimit" :-> Double,
      "StratificationCategory1" :-> Text, "Stratification1" :-> Text,
      "StratificationCategory2" :-> Text, "Stratification2" :-> Text,
      "StratificationCategory3" :-> Text, "Stratification3" :-> Text,
      "GeoLocation" :-> Text, "TopicID" :-> Text, "QuestionID" :-> Text,
      "ResponseID" :-> Text, "LocationID" :-> Int,
      "StratificationCategoryID1" :-> Text, "StratificationID1" :-> Text,
      "StratificationCategoryID2" :-> Text, "StratificationID2" :-> Text,
      "StratificationCategoryID3" :-> Text, "StratificationID3" :-> Text]
  	-- Defined at /home/cody/source/frames-chronic-disease-indicators/src/Main.hs:16:1
```

That all looks sane to me. I always check to see that all my columns are there and that they were inferred as the right type.

Let's move on to previewing the data:

```haskell
λ> pipePreview rows 3 cat -- cat is the identity pipe and gets its name from the unix utility "cat"
{YearStart :-> 2013, YearEnd :-> 2013, LocationAbbr :-> "CT", LocationDesc :-> "Connecticut", DataSource :-> "YRBSS", Topic :-> "Alcohol", Question :-> "Alcohol use among youth", Response :-> "", DataValueUnit :-> "%", DataValueTypeID :-> "CrdPrev", DataValueType :-> "Crude Prevalence", DataValue :-> 36.7, DataValueAlt :-> 36.7, DataValueFootnoteSymbol :-> "", DatavalueFootnote :-> "", LowConfidenceLimit :-> 32.7, HighConfidenceLimit :-> 41.0, StratificationCategory1 :-> "Overall", Stratification1 :-> "Overall", StratificationCategory2 :-> "", Stratification2 :-> "", StratificationCategory3 :-> "", Stratification3 :-> "", GeoLocation :-> "(41.56266102000046, -72.64984095199964)", TopicID :-> "ALC", QuestionID :-> "ALC1_1", ResponseID :-> "", LocationID :-> 9, StratificationCategoryID1 :-> "OVERALL", StratificationID1 :-> "OVR", StratificationCategoryID2 :-> "", StratificationID2 :-> "", StratificationCategoryID3 :-> "", StratificationID3 :-> ""}
{YearStart :-> 2013, YearEnd :-> 2013, LocationAbbr :-> "DC", LocationDesc :-> "District of Columbia", DataSource :-> "YRBSS", Topic :-> "Alcohol", Question :-> "Alcohol use among youth", Response :-> "", DataValueUnit :-> "%", DataValueTypeID :-> "CrdPrev", DataValueType :-> "Crude Prevalence", DataValue :-> 31.4, DataValueAlt :-> 31.4, DataValueFootnoteSymbol :-> "", DatavalueFootnote :-> "", LowConfidenceLimit :-> 30.2, HighConfidenceLimit :-> 32.5, StratificationCategory1 :-> "Overall", Stratification1 :-> "Overall", StratificationCategory2 :-> "", Stratification2 :-> "", StratificationCategory3 :-> "", Stratification3 :-> "", GeoLocation :-> "(38.907192, -77.036871)", TopicID :-> "ALC", QuestionID :-> "ALC1_1", ResponseID :-> "", LocationID :-> 11, StratificationCategoryID1 :-> "OVERALL", StratificationID1 :-> "OVR", StratificationCategoryID2 :-> "", StratificationID2 :-> "", StratificationCategoryID3 :-> "", StratificationID3 :-> ""}
{YearStart :-> 2013, YearEnd :-> 2013, LocationAbbr :-> "DE", LocationDesc :-> "Delaware", DataSource :-> "YRBSS", Topic :-> "Alcohol", Question :-> "Alcohol use among youth", Response :-> "", DataValueUnit :-> "%", DataValueTypeID :-> "CrdPrev", DataValueType :-> "Crude Prevalence", DataValue :-> 36.3, DataValueAlt :-> 36.3, DataValueFootnoteSymbol :-> "", DatavalueFootnote :-> "", LowConfidenceLimit :-> 33.7, HighConfidenceLimit :-> 39.0, StratificationCategory1 :-> "Overall", Stratification1 :-> "Overall", StratificationCategory2 :-> "", Stratification2 :-> "", StratificationCategory3 :-> "", Stratification3 :-> "", GeoLocation :-> "(39.008830667000495, -75.57774116799965)", TopicID :-> "ALC", QuestionID :-> "ALC1_1", ResponseID :-> "", LocationID :-> 10, StratificationCategoryID1 :-> "OVERALL", StratificationID1 :-> "OVR", StratificationCategoryID2 :-> "", StratificationID2 :-> "", StratificationCategoryID3 :-> "", StratificationID3 :-> ""}
```

```haskell
λ> pipePreview rows 3 (P.filter (\row -> rget locationAbbr row == "DC"))
{YearStart :-> 2013, YearEnd :-> 2013, LocationAbbr :-> "DC", LocationDesc :-> "District of Columbia", DataSource :-> "YRBSS", Topic :-> "Alcohol", Question :-> "Alcohol use among youth", Response :-> "", DataValueUnit :-> "%", DataValueTypeID :-> "CrdPrev", DataValueType :-> "Crude Prevalence", DataValue :-> 31.4, DataValueAlt :-> 31.4, DataValueFootnoteSymbol :-> "", DatavalueFootnote :-> "", LowConfidenceLimit :-> 30.2, HighConfidenceLimit :-> 32.5, StratificationCategory1 :-> "Overall", Stratification1 :-> "Overall", StratificationCategory2 :-> "", Stratification2 :-> "", StratificationCategory3 :-> "", Stratification3 :-> "", GeoLocation :-> "(38.907192, -77.036871)", TopicID :-> "ALC", QuestionID :-> "ALC1_1", ResponseID :-> "", LocationID :-> 11, StratificationCategoryID1 :-> "OVERALL", StratificationID1 :-> "OVR", StratificationCategoryID2 :-> "", StratificationID2 :-> "", StratificationCategoryID3 :-> "", StratificationID3 :-> ""}
{YearStart :-> 2013, YearEnd :-> 2013, LocationAbbr :-> "DC", LocationDesc :-> "District of Columbia", DataSource :-> "YRBSS", Topic :-> "Alcohol", Question :-> "Binge drinking prevalence among youth", Response :-> "", DataValueUnit :-> "%", DataValueTypeID :-> "CrdPrev", DataValueType :-> "Crude Prevalence", DataValue :-> 12.3, DataValueAlt :-> 12.3, DataValueFootnoteSymbol :-> "", DatavalueFootnote :-> "", LowConfidenceLimit :-> 11.6, HighConfidenceLimit :-> 13.2, StratificationCategory1 :-> "Overall", Stratification1 :-> "Overall", StratificationCategory2 :-> "", Stratification2 :-> "", StratificationCategory3 :-> "", Stratification3 :-> "", GeoLocation :-> "(38.907192, -77.036871)", TopicID :-> "ALC", QuestionID :-> "ALC2_1", ResponseID :-> "", LocationID :-> 11, StratificationCategoryID1 :-> "OVERALL", StratificationID1 :-> "OVR", StratificationCategoryID2 :-> "", StratificationID2 :-> "", StratificationCategoryID3 :-> "", StratificationID3 :-> ""}
{YearStart :-> 2014, YearEnd :-> 2014, LocationAbbr :-> "DC", LocationDesc :-> "District of Columbia", DataSource :-> "BRFSS", Topic :-> "Alcohol", Question :-> "Binge drinking prevalence among adults aged >= 18 years", Response :-> "", DataValueUnit :-> "%", DataValueTypeID :-> "CrdPrev", DataValueType :-> "Crude Prevalence", DataValue :-> 24.9, DataValueAlt :-> 24.9, DataValueFootnoteSymbol :-> "", DatavalueFootnote :-> "", LowConfidenceLimit :-> 22.4, HighConfidenceLimit :-> 27.7, StratificationCategory1 :-> "Overall", Stratification1 :-> "Overall", StratificationCategory2 :-> "", Stratification2 :-> "", StratificationCategory3 :-> "", Stratification3 :-> "", GeoLocation :-> "(38.89037138500049, -77.03196112699965)", TopicID :-> "ALC", QuestionID :-> "ALC2_2", ResponseID :-> "", LocationID :-> 11, StratificationCategoryID1 :-> "OVERALL", StratificationID1 :-> "OVR", StratificationCategoryID2 :-> "", StratificationID2 :-> "", StratificationCategoryID3 :-> "", StratificationID3 :-> ""}
```

There is a more general form than using `pipePreview`. In it, the above example would look like this:

```haskell
λ> runEffect $ rows >-> P.take 3 >-> P.filter (\row -> rget locationAbbr row == "DC") >-> P.print
{YearStart :-> 2013, YearEnd :-> 2013, LocationAbbr :-> "DC", LocationDesc :-> "District of Columbia", DataSource :-> "YRBSS", Topic :-> "Alcohol", Question :-> "Alcohol use among youth", Response :-> "", DataValueUnit :-> "%", DataValueTypeID :-> "CrdPrev", DataValueType :-> "Crude Prevalence", DataValue :-> 31.4, DataValueAlt :-> 31.4, DataValueFootnoteSymbol :-> "", DatavalueFootnote :-> "", LowConfidenceLimit :-> 30.2, HighConfidenceLimit :-> 32.5, StratificationCategory1 :-> "Overall", Stratification1 :-> "Overall", StratificationCategory2 :-> "", Stratification2 :-> "", StratificationCategory3 :-> "", Stratification3 :-> "", GeoLocation :-> "(38.907192, -77.036871)", TopicID :-> "ALC", QuestionID :-> "ALC1_1", ResponseID :-> "", LocationID :-> 11, StratificationCategoryID1 :-> "OVERALL", StratificationID1 :-> "OVR", StratificationCategoryID2 :-> "", StratificationID2 :-> "", StratificationCategoryID3 :-> "", StratificationID3 :-> ""}
```

A common error I make is forgetting to add on an "effect" at the end such as `P.print` which results in an error that looks like this:

```haskell
λ> runEffect $ rows >-> P.take 3 >-> P.filter (\row -> rget locationAbbr row == "DC")

<interactive>:192:13: error:
    • Couldn't match type ‘vinyl-0.5.3:Data.Vinyl.Core.Rec
                             vinyl-0.5.3:Data.Vinyl.Functor.Identity
                             '["YearStart" :-> Int, "YearEnd" :-> Int, "LocationAbbr" :-> Text,
                               "LocationDesc" :-> Text, "DataSource" :-> Text, "Topic" :-> Text,
                               "Question" :-> Text, "Response" :-> Text, "DataValueUnit" :-> Text,
                               "DataValueTypeID" :-> Text, "DataValueType" :-> Text,
                               "DataValue" :-> Double, "DataValueAlt" :-> Double,
                               "DataValueFootnoteSymbol" :-> Text, "DatavalueFootnote" :-> Text,
                               "LowConfidenceLimit" :-> Double, "HighConfidenceLimit" :-> Double,
                               "StratificationCategory1" :-> Text, "Stratification1" :-> Text,
                               "StratificationCategory2" :-> Text, "Stratification2" :-> Text,
                               "StratificationCategory3" :-> Text, "Stratification3" :-> Text,
                               "GeoLocation" :-> Text, "TopicID" :-> Text, "QuestionID" :-> Text,
                               "ResponseID" :-> Text, "LocationID" :-> Int,
                               "StratificationCategoryID1" :-> Text, "StratificationID1" :-> Text,
                               "StratificationCategoryID2" :-> Text, "StratificationID2" :-> Text,
                               "StratificationCategoryID3" :-> Text,
                               "StratificationID3" :-> Text]’
                     with ‘X’
      Expected type: Effect IO ()
        Actual type: Proxy
                       X
                       ()
                       ()
                       (Record
                          '["YearStart" :-> Int, "YearEnd" :-> Int, "LocationAbbr" :-> Text,
                            "LocationDesc" :-> Text, "DataSource" :-> Text, "Topic" :-> Text,
                            "Question" :-> Text, "Response" :-> Text, "DataValueUnit" :-> Text,
                            "DataValueTypeID" :-> Text, "DataValueType" :-> Text,
                            "DataValue" :-> Double, "DataValueAlt" :-> Double,
                            "DataValueFootnoteSymbol" :-> Text, "DatavalueFootnote" :-> Text,
                            "LowConfidenceLimit" :-> Double, "HighConfidenceLimit" :-> Double,
                            "StratificationCategory1" :-> Text, "Stratification1" :-> Text,
                            "StratificationCategory2" :-> Text, "Stratification2" :-> Text,
                            "StratificationCategory3" :-> Text, "Stratification3" :-> Text,
                            "GeoLocation" :-> Text, "TopicID" :-> Text, "QuestionID" :-> Text,
                            "ResponseID" :-> Text, "LocationID" :-> Int,
                            "StratificationCategoryID1" :-> Text, "StratificationID1" :-> Text,
                            "StratificationCategoryID2" :-> Text, "StratificationID2" :-> Text,
                            "StratificationCategoryID3" :-> Text,
                            "StratificationID3" :-> Text])
                       IO
                       ()
    • In the second argument of ‘($)’, namely
        ‘rows >-> P.take 3
         >-> P.filter (\ row -> rget locationAbbr row == "DC")’
      In the expression:
        runEffect
        $ rows >-> P.take 3
          >-> P.filter (\ row -> rget locationAbbr row == "DC")
      In an equation for ‘it’:
          it
            = runEffect
              $ rows >-> P.take 3
                >-> P.filter (\ row -> rget locationAbbr row == "DC")
```

Usually I don't care about all of the data, so let's see how limiting it looks:

```haskell
λ> runEffect $ rows >-> P.filter (\row -> rget locationAbbr row == "DC") >-> P.map (select [pr|YearStart,YearEnd,LocationAbbr,Topic|]) >-> P.take 5 >-> P.print
{YearStart :-> 2013, YearEnd :-> 2013, LocationAbbr :-> "DC", Topic :-> "Alcohol"}
{YearStart :-> 2013, YearEnd :-> 2013, LocationAbbr :-> "DC", Topic :-> "Alcohol"}
{YearStart :-> 2014, YearEnd :-> 2014, LocationAbbr :-> "DC", Topic :-> "Alcohol"}
{YearStart :-> 2014, YearEnd :-> 2014, LocationAbbr :-> "DC", Topic :-> "Alcohol"}
{YearStart :-> 2014, YearEnd :-> 2014, LocationAbbr :-> "DC", Topic :-> "Alcohol"}
```

What other topics are there besides Alcohol?
```haskell
λ> runEffect $ rows >-> P.filter (\row -> rget locationAbbr row == "DC" && rget topic row /= "Alcohol") >-> P.map (select [pr|YearStart,YearEnd,LocationAbbr,Topic|]) >-> P.take 5 >-> P.print
{YearStart :-> 2014, YearEnd :-> 2014, LocationAbbr :-> "DC", Topic :-> "Arthritis"}
{YearStart :-> 2014, YearEnd :-> 2014, LocationAbbr :-> "DC", Topic :-> "Arthritis"}
{YearStart :-> 2014, YearEnd :-> 2014, LocationAbbr :-> "DC", Topic :-> "Arthritis"}
{YearStart :-> 2014, YearEnd :-> 2014, LocationAbbr :-> "DC", Topic :-> "Arthritis"}
{YearStart :-> 2014, YearEnd :-> 2014, LocationAbbr :-> "DC", Topic :-> "Arthritis"}
```

Below is the implementation of the `distinctTopics` function:

```haskell
distinctTopics rowProducer = do
  -- build a set by iterating over all rows and inserting topic values
  -- topicMap <- P.fold (\m r -> M.insert (view lens1 (r :: Record checkRec)) (0 :: Int8) m) M.empty id checkProducer
  topics <- P.fold
              (\accumSet currentRow -> HS.insert (rget topic currentRow) accumSet) -- what we build up the set with
              HS.empty -- initial value
              id -- we just want to return the value we inserted, nothing more
              rowProducer -- our rowProducer, in this specific case it is just `rows`
  print topics
```

This breaks our pipes paradigm a little, but I don't currently know of a better way to do it so this will suffice:

```haskell
λ> distinctTopics (rows >-> P.filter (\row -> rget locationAbbr row == "DC" && rget topic row /= "Alcohol") >-> P.map (select [pr|YearStart,YearEnd,LocationAbbr,Topic|]))
fromList ["Arthritis","Overarching Conditions","Diabetes","Chronic Obstructive Pulmonary Disease","Immunization","Oral Health","Nutrition, Physical Activity, and Weight Status","Tobacco","Chronic kidney disease","Chronic Kidney Disease","Asthma","Reproductive Health","Cancer","Cardiovascular Disease","Mental Health","Older Adults"]
(2.28 secs, 9,068,133,336 bytes)
```

I turned on the `:set +s` option in ghci which shows the time it took for a function to execute and the number of total bytes allocated throughout the functions execution. You can see this was pretty snappy for a...

```
λ> P.length rows
141382
(1.78 secs, 7,828,251,384 bytes)
```

Yes, a 141,382 row csv file. Pretty respectable I think, or at least seems fast enough for my use cases at the moment.


Let's do something a little more involved by answering a useful question:

How many of each topic occurs in what location? Given:

```
LocationAbbr,Topic
TX,Arthritis
TX,Arthritis
TX,Mental Health
AR,Cancer
AR,Diabetes
```

We should get back:

```
[
 [
  ...
  ( "TX",
    [ ("Arthritis",2)
    , ("Mental Health",1)
    ]
  ),
  ( "AR",
    [ ("Cancer",1)
    , ("Diabetes",1)
    ]
  )
  ...
 ]
]
```

I don't know that a list of maps in a list of maps is the best representation here, but it's the one I immediately came up with so we'll go with that. From experience I can see this will be another fold with a mix of Map.insert and Map.update (or Map.updateWith). Let's play around with maps really quick for readers that might not be so familiar with them. If you are already familiar feel free to skip ahead.


First let's see how we can insert into a single location into a map by following the types:

```haskell
λ> :t M.insert
M.insert :: Ord k => k -> a -> M.Map k a -> M.Map k a
λ> :t M.insert ("TX" :: T.Text)
M.insert ("TX" :: T.Text) :: a -> M.Map Text a -> M.Map Text a
λ> :t M.insert ("TX" :: T.Text) ("xxx" :: T.Text)
M.insert ("TX" :: T.Text) ("xxx" :: T.Text)
  :: M.Map Text Text -> M.Map Text Text
λ> :t M.insert ("TX" :: T.Text) ("xxx" :: T.Text) M.empty
M.insert ("TX" :: T.Text) ("xxx" :: T.Text) M.empty
  :: M.Map Text Text
λ> M.insert ("TX" :: T.Text) ("xxx" :: T.Text) M.empty
fromList [("TX","xxx")]
(0.00 secs, 443,944 bytes)
```

We are building up a value by modifying an initial value over time, in functional languages like Haskell you use a fold to do this. Knowing this would require some previous knowledge, so in this case I'm just going to tell you.

Briefly a fold in Haskell looks like this (feel free to skip ahead if you are well versed in folds):

```haskell
λ> import Data.List
λ> :t foldl'
foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
```

That type signature is pretty generic, so I'll specialize it to using List which is a more common instance of Foldable. That should make it easier to understand.

```haskell
λ> import Data.List
λ> :t foldl'
foldl' :: (b -> a -> b) -> b -> [a] -> b
```

Now I'll show you how the sum function is implemented using the above type signature:

```haskell
λ> foldl' (\a b -> a + b) 0 [1,2,3]
6
```

I prefer writing and looking at folds like this:

```haskell
foldl'
 modifyingFunction accumulatingValue currentValue -- (\a b -> a + b)
 initialValue                                     -- 0
 listToFold over                                  -- [1,2,3]
```

Now we can take a look at folding over Pipes:

```haskell
P.fold
  :: Monad m =>
     (x -> a -> x) -> x -> (x -> b) -> Producer a m () -> m b
```

Or in my preferred style:

```haskell
P.fold :: Monad m =>
        (x -> a -> x) -- modifying function, same as foldl'
     ->  x -- initial value, same as foldl'
     -> (x -> b) -- a function to apply to the folded result of the current step/iteration.
     -> Producer a m () -- We are folding over a pipe instead of a list, so we fold over a Producer of values here.
     -> m b -- our result is the some value that satisfies the Monad constraint
```

Next let's see what it takes to build a Map representing all the locations with a single value by following the types.

Recall that we have a built a function earlier to just insert the value "xxx" into a map for a constant key value:

```haskell
M.insert ("TX" :: T.Text) ("xxx" :: T.Text) M.empty
```

Now though, we need to dynamically provide the key value (first argument) and the initial accumulating value (third argument).

```haskell
\accumMap key -> M.insert key ("xxx" :: T.Text) accumMap
```

This won't quite be sufficient though. We aren't folding over a list of keys, we are folding over a list of rows. It might not seem so obvious at first, but the above would result in a type error. The correct option is to have:

```haskell
λ> :t \accumMap currentRow -> M.insert (rget locationAbbr currentRow) ("xxx" :: T.Text) accumMap
\accumMap currentRow -> M.insert (rget locationAbbr currentRow) ("xxx" :: T.Text) accumMap
  :: RElem
       LocationAbbr
       rs
       (vinyl-0.5.3:Data.Vinyl.TypeLevel.RIndex LocationAbbr rs) =>
     M.Map Text Text -> Record rs -> M.Map Text Text
```

The Relem constraint means "this records or rows must have the LocationAbbr column". It's a little prettier in my opinion to use the unicode ∈ set contains operator, where the signature would look like:

```haskell
λ> :t \accumMap currentRow -> M.insert (rget locationAbbr currentRow) ("xxx" :: T.Text) accumMap
\accumMap currentRow -> M.insert (rget locationAbbr currentRow) ("xxx" :: T.Text) accumMap
  :: LocationAbbr ∈ rs => M.Map Text Text -> Record rs -> M.Map Text Text
```

Then we can read it aloud as "this function takes an accumulating map value and some records that must all contain LocationAbbr columns and returns a map from Text to Text". Now we can plug this function into P.fold as our "modifying function":

```haskell
P.fold :: Monad m =>
        (\accumMap currentRow -> M.insert (rget locationAbbr currentRow) ("xxx" :: T.Text) accumMap)  -- modifying function, same as foldl'
     ->  x -- initial value, same as foldl'
     -> (x -> b) -- a function to apply to the folded result of the current step/iteration.
     -> Producer a m () -- We are folding over a pipe instead of a list, so we fold over a Producer of values here.
     -> m b -- our result is the some value that satisfies the Monad constraint
```

After we provided the new modifying function, the other types we need to fill in will become more specialized. This means it will be easier for us to understand what else is needed to complete our functionality and is central to how I develop with Haskell. If we look at the type now we get (I substituted in the set operator style type signature):

```haskell
P.fold (\accumMap currentRow -> M.insert (rget locationAbbr currentRow) ("xxx" :: T.Text) accumMap)
  :: (LocationAbbr ∈ rs, Monad m) =>
         M.Map Text Text          -- initial value
     -> (M.Map Text Text -> b)    -- a function to apply to a folded result of the current step/iteration
     -> Producer (Record rs) m () -- Producer of values we are folding over
     -> m b                       -- suprisingly this didn't specialize yet, but it should be a M.Map Text Text wrapped in a monad (such as IO)

```

Our initial value will be an empty Map, we can use `M.empty`:

```haskell
λ> :t P.fold (\accumMap currentRow -> M.insert (rget locationAbbr currentRow) ("xxx" :: T.Text) accumMap) M.empty
P.fold (\accumMap currentRow -> M.insert (rget locationAbbr currentRow) ("xxx" :: T.Text) accumMap) M.empty
  :: (LocationAbbr ∈ rs, Monad m) =>
       (M.Map Text Text -> b)     -- a function to apply to a folded result of the current step/iteration
     -> Producer (Record rs) m () -- Producer of values we are folding over 
     -> m b                       -- suprisingly this didn't specialize yet, but it should be a M.Map Text Text wrapped in a monad (such as IO)
```

We don't want to do anything to our folded row after each iteration, so we can just use the `id` function.

```haskell
P.fold (\accumMap currentRow -> M.insert (rget locationAbbr currentRow) ("xxx" :: T.Text) accumMap) M.empty id
  :: (LocationAbbr ∈ rs, Monad m) =>
        Producer (Record rs) m () 
     -> m (M.Map Text Text)
```

Our function to set all elements of the map to constant values by folding over every row is done, we just need to give it a producer. We'll give it `rows`:

```
λ> :t (P.fold (\accumMap currentRow -> M.insert (rget locationAbbr currentRow) ("xxx" :: T.Text) accumMap) M.empty id) rows
(P.fold (\accumMap currentRow -> M.insert (rget locationAbbr currentRow) ("xxx" :: T.Text) accumMap) M.empty id) rows
  :: IO (M.Map Text Text)
λ> -- and actually running it!
λ> (P.fold (\accumMap currentRow -> M.insert (rget locationAbbr currentRow) "xxx" accumMap) M.empty id) rows
fromList [("AK","xxx"),("AL","xxx"),("AR","xxx"),("AZ","xxx"),("CA","xxx"),("CO","xxx"),("CT","xxx"),("DC","xxx"),("DE","xxx"),("FL","xxx"),("GA","xxx"),("GU","xxx"),("HI","xxx"),("IA","xxx"),("ID","xxx"),("IL","xxx"),("IN","xxx"),("KS","xxx"),("KY","xxx"),("LA","xxx"),("MA","xxx"),("MD","xxx"),("ME","xxx"),("MI","xxx"),("MN","xxx"),("MO","xxx"),("MS","xxx"),("MT","xxx"),("NC","xxx"),("ND","xxx"),("NE","xxx"),("NH","xxx"),("NJ","xxx"),("NM","xxx"),("NV","xxx"),("NY","xxx"),("OH","xxx"),("OK","xxx"),("OR","xxx"),("PA","xxx"),("PR","xxx"),("RI","xxx"),("SC","xxx"),("SD","xxx"),("TN","xxx"),("TX","xxx"),("US","xxx"),("UT","xxx"),("VA","xxx"),("VT","xxx"),("WA","xxx"),("WI","xxx"),("WV","xxx"),("WY","xxx")]
(2.27 secs, 8,922,690,440 bytes)
```

Now we can insert a constant value for each state, but we need to go further and actually upsert the map values that will replace the "xxx" text. Remember our end goal from above is this:

```
[
 [
  ...
  ( "TX",
    [ ("Arthritis",2)
    , ("Mental Health",1)
    ]
  ),
  ( "AR",
    [ ("Cancer",1)
    , ("Diabetes",1)
    ]
  )
  ...
 ]
]
```

For nested updates, I tend to pull out the lens library. Hopefully this isn't too unreadable and/or impenetrable:

```haskell
groupTopics :: ( Topic ∈ rs
               , LocationAbbr ∈ rs
               , Monad m) =>
               Producer (Record rs) m () -> m (M.Map Text (M.Map Text Integer))
groupTopics = P.fold
                (\accum currentRow ->
                    accum & at (rget locationAbbr currentRow) . non' _Empty -- if the locationAbbr isn't created, create with an empty map
                          . at (rget topic currentRow) . non 0 +~ 1         -- if topic for given locationAbbr doesn't exist, initialize at 0 and add 1
                )
                M.empty
                id
```

Some examples to try and explain above:

```haskell
λ> -- first we'll start out with an empty but explicitly type annotated Map
λ> (M.empty :: M.Map String Integer)
fromList []
λ> set (at "Cody") (Just 25) (M.empty :: M.Map String Integer)
fromList [("Cody",25)]
λ> -- or using infix (symbols)
λ> (M.empty :: M.Map String Integer) & at "Cody" .~ Just 25
fromList [("Cody",25)]
λ> -- .~ is just set, here's proof:
λ> (M.empty :: M.Map String Integer) & at "Cody" `set` Just 25
fromList [("Cody",25)]
λ> -- now on to our actual Map structure
λ> (M.empty :: M.Map String (M.Map String Integer)) & at "TX" . non M.empty . at "Alcohol" . non 0 +~ 1
fromList [("TX",fromList [("Alcohol",1)])]
λ> -- +~ doesn't have a word name, but if it did it would be "inc" or "increment"
λ> -- non lets you give a default value for the previous lens. So the default value if "TX" doesn't exist is an empty Map
λ> -- the default for Alcohol if it doesn't exist, is 0. Then it is quickly updated by 1
λ> -- I made another small change, and used non' _Empty instead of non M.empty
λ> (M.empty :: M.Map String (M.Map String Integer)) & at "TX" . non' _Empty . at "Alcohol" . non 0 +~ 1
fromList [("TX",fromList [("Alcohol",1)])]
λ> -- what is the difference between non and non'? I don't know or care too much at the moment, but that's okay! You don't have to deeply understand or memorize all of the pieces here to get real work done :)
```

Now let's do something simple. Let's use `wc -l` to see how many lines there are in this csv file:

```shell
cody@zentop:~/source/frames-chronic-disease-indicators$ wc -l data/U.S._Chronic_Disease_Indicators__CDI_.csv 
237962 data/U.S._Chronic_Disease_Indicators__CDI_.csv
```

We can also see how many results a producer has with Frames and Pipes:

```
λ> P.length rows
141382
```

Wait... what's going on here? We are missing just under 100,000 rows! Specifically:

```haskell
λ> 237962 - 1 -- subtract header row
237961
λ> 237961 - 141382
96579
```

Exactly 96579 rows are missing :/

Why? To find out we need to examine our rows producer:

```haskell
rows :: Producer Row IO ()
rows = readTableOpt rowParser "data/U.S._Chronic_Disease_Indicators__CDI_.csv"
```

It produces values of the type:

```haskell
λ> :i Row
type Row =
  Record
    '["YearStart" :-> Int, "YearEnd" :-> Int, "LocationAbbr" :-> Text,
      "LocationDesc" :-> Text, "DataSource" :-> Text, "Topic" :-> Text,
      "Question" :-> Text, "Response" :-> Text, "DataValueUnit" :-> Text,
      "DataValueTypeID" :-> Text, "DataValueType" :-> Text,
      "DataValue" :-> Double, "DataValueAlt" :-> Double,
      "DataValueFootnoteSymbol" :-> Text, "DatavalueFootnote" :-> Text,
      "LowConfidenceLimit" :-> Double, "HighConfidenceLimit" :-> Double,
      "StratificationCategory1" :-> Text, "Stratification1" :-> Text,
      "StratificationCategory2" :-> Text, "Stratification2" :-> Text,
      "StratificationCategory3" :-> Text, "Stratification3" :-> Text,
      "GeoLocation" :-> Text, "TopicID" :-> Text, "QuestionID" :-> Text,
      "ResponseID" :-> Text, "LocationID" :-> Int,
      "StratificationCategoryID1" :-> Text, "StratificationID1" :-> Text,
      "StratificationCategoryID2" :-> Text, "StratificationID2" :-> Text,
      "StratificationCategoryID3" :-> Text, "StratificationID3" :-> Text]
  	-- Defined at /home/cody/source/frames-chronic-disease-indicators/src/Main.hs:17:1
```

Next we need to examine the Record type a little more:

```
λ> :i Record
type Record = Rec Data.Vinyl.Functor.Identity :: [*] -> *
  	-- Defined in ‘Frames.Rec’
```

The `[*]` in this case would be the list of Fields: `'["YearStart" :-> Int, ... "StratificationID3" :-> Text]`

So if import Data.Vinyl.Functor to unqualify Identity and write out the Rec type we would have:

```haskell
  Rec Identity
    '["YearStart" :-> Int, "YearEnd" :-> Int, "LocationAbbr" :-> Text,
      "LocationDesc" :-> Text, "DataSource" :-> Text, "Topic" :-> Text,
      "Question" :-> Text, "Response" :-> Text, "DataValueUnit" :-> Text,
      "DataValueTypeID" :-> Text, "DataValueType" :-> Text,
      "DataValue" :-> Double, "DataValueAlt" :-> Double,
      "DataValueFootnoteSymbol" :-> Text, "DatavalueFootnote" :-> Text,
      "LowConfidenceLimit" :-> Double, "HighConfidenceLimit" :-> Double,
      "StratificationCategory1" :-> Text, "Stratification1" :-> Text,
      "StratificationCategory2" :-> Text, "Stratification2" :-> Text,
      "StratificationCategory3" :-> Text, "Stratification3" :-> Text,
      "GeoLocation" :-> Text, "TopicID" :-> Text, "QuestionID" :-> Text,
      "ResponseID" :-> Text, "LocationID" :-> Int,
      "StratificationCategoryID1" :-> Text, "StratificationID1" :-> Text,
      "StratificationCategoryID2" :-> Text, "StratificationID2" :-> Text,
      "StratificationCategoryID3" :-> Text, "StratificationID3" :-> Text]
```

So what is the type of Rec anyway?

```haskell
data Rec (a :: u -> *) (b :: [u]) where
  RNil :: forall u (a :: u -> *). Rec a '[]
  (:&) :: forall u (a :: u -> *) (r :: u) (rs :: [u]).
       !(a r) -> !(Rec a rs) -> Rec a (r : rs)
```

We'll assume the Identity case to make sense of this:

- a is Identity
- r is a single item in the type level list of the entire record, rs is the remaining portion of the type level list

So why not just have `Rec rs`? What is the use of being able to supply almost any `a` in the `Rec a rs`?

You can supply your own functor! If you don't understand what that means, don't worry about it too much. For us it means
that we can replace Identity with Maybe.

From `Rec Identity rs` to `Rec Maybe rs`. The function readTableMaybeOpt does just this. Below are the type signatures of readTableOpt (our current producer function) and readTableMaybeOpt:

```haskell
λ> :t readTableOpt
readTableOpt
  :: (ReadRec rs, MonadIO m) =>
     ParserOptions -> FilePath -> Producer (Record rs) m () -- Notice the type synonmy `Record` is used, but that's the same as `Rec Identity rs`
λ> :t readTableMaybeOpt
readTableMaybeOpt
  :: (ReadRec rs, MonadIO m) =>
     ParserOptions -> FilePath -> Producer (Rec Maybe rs) m ()
```

The best way to show why this is useful is just to print out some rows with our new producer:

```haskell
-- our new producer definition
rows' :: Producer (ColFun Maybe Row) IO ()
rows' = readTableMaybeOpt rowParser "data/U.S._Chronic_Disease_Indicators__CDI_.csv"
```

```haskell
λ> pipePreview rows' 1 cat
{Just YearStart :-> 2013, Just YearEnd :-> 2013, Just LocationAbbr :-> "CA", Just LocationDesc :-> "California", Just DataSource :-> "YRBSS", Just Topic :-> "Alcohol", Just Question :-> "Alcohol use among youth", Just Response :-> "", Just DataValueUnit :-> "%", Just DataValueTypeID :-> "CrdPrev", Just DataValueType :-> "Crude Prevalence", Nothing, Nothing, Just DataValueFootnoteSymbol :-> "-", Just DatavalueFootnote :-> "No data available", Nothing, Nothing, Just StratificationCategory1 :-> "Overall", Just Stratification1 :-> "Overall", Just StratificationCategory2 :-> "", Just Stratification2 :-> "", Just StratificationCategory3 :-> "", Just Stratification3 :-> "", Just GeoLocation :-> "(37.63864012300047, -120.99999953799971)", Just TopicID :-> "ALC", Just QuestionID :-> "ALC1_1", Just ResponseID :-> "", Just LocationID :-> 6, Just StratificationCategoryID1 :-> "OVERALL", Just StratificationID1 :-> "OVR", Just StratificationCategoryID2 :-> "", Just StratificationID2 :-> "", Just StratificationCategoryID3 :-> "", Just StratificationID3 :-> ""}
```

TODO: Update Rec Maybe instance so that Nothing's still have a column name to make tracking down failures/holes easier

So everything is now wrapped in the Maybe monad... cool! But wait... why is this useful? Notice that some of the column values have `Nothing` instead of a value. What does Frames do when it encounters a `Nothing` or a value it can't parse in the Identity Monad?

It skips it! This is the reason some of our rows were just silently skipped over!

Now eliding some details, I'll tell you there is a function called `recMaybe :: Rec Maybe cs -> Maybe (Record cs)` that creates our original `Record` type from some `Rec Maybe cs`. I'll also tell you that we can use this in a pipes filter operation to find records that failed to parse:

```haskell
λ> pipePreview rows' 1 (P.filter (\r -> recMaybe r == Nothing))
{Just YearStart :-> 2013, Just YearEnd :-> 2013, Just LocationAbbr :-> "CA", Just LocationDesc :-> "California", Just DataSource :-> "YRBSS", Just Topic :-> "Alcohol", Just Question :-> "Alcohol use among youth", Just Response :-> "", Just DataValueUnit :-> "%", Just DataValueTypeID :-> "CrdPrev", Just DataValueType :-> "Crude Prevalence", Nothing, Nothing, Just DataValueFootnoteSymbol :-> "-", Just DatavalueFootnote :-> "No data available", Nothing, Nothing, Just StratificationCategory1 :-> "Overall", Just Stratification1 :-> "Overall", Just StratificationCategory2 :-> "", Just Stratification2 :-> "", Just StratificationCategory3 :-> "", Just Stratification3 :-> "", Just GeoLocation :-> "(37.63864012300047, -120.99999953799971)", Just TopicID :-> "ALC", Just QuestionID :-> "ALC1_1", Just ResponseID :-> "", Just LocationID :-> 6, Just StratificationCategoryID1 :-> "OVERALL", Just StratificationID1 :-> "OVR", Just StratificationCategoryID2 :-> "", Just StratificationID2 :-> "", Just StratificationCategoryID3 :-> "", Just StratificationID3 :-> ""}
```

Then we can see how many failed to parse with:

```haskell
λ> P.length (rows' >-> P.filter (\r -> recMaybe r == Nothing))
96579
```

Does that number look familiar? Good! (hint: it is the number of rows we deduced was missing with wc -l and the number of rows we had).


Cool. So what can we actually do? Well, that depends on how you want to handle nulls in your data. In my case I find that it's usually okay to assign a zero value or default value. I created a function called `defaultingProducer` in my [frames-diff package](https://github.com/codygman/frames-diff).

You can see the defaulting rules [here](https://github.com/codygman/frames-diff/blob/master/Frames/Diff.hs#L40) or inline below:

```haskell
instance Default (s :-> Int) where def = Col 0
instance Default (s :-> Text) where def = Col mempty
instance Default (s :-> Double) where def = Col 0.0
instance Default (s :-> Bool) where def = Col False
```

It can be used like so (after adding frames-diff to your cabal file):

```haskell
rows'' :: Producer Row IO ()
rows'' = defaultingProducer "data/U.S._Chronic_Disease_Indicators__CDI_.csv" "a string label used in errors for your convenience"
```

Then we can get the length:

```haskell
λ> P.length rows''
237961
```

Taking advantage of the fact that we found out the first record our producer produces had some Nothing's, we can see what it looks like now with the defaultingProducer:

```haskell
λ> pipePreview rows'' 1 cat
{YearStart :-> 2013, YearEnd :-> 2013, LocationAbbr :-> "CA", LocationDesc :-> "California", DataSource :-> "YRBSS", Topic :-> "Alcohol", Question :-> "Alcohol use among youth", Response :-> "", DataValueUnit :-> "%", DataValueTypeID :-> "CrdPrev", DataValueType :-> "Crude Prevalence", DataValue :-> 0.0, DataValueAlt :-> 0.0, DataValueFootnoteSymbol :-> "-", DatavalueFootnote :-> "No data available", LowConfidenceLimit :-> 0.0, HighConfidenceLimit :-> 0.0, StratificationCategory1 :-> "Overall", Stratification1 :-> "Overall", StratificationCategory2 :-> "", Stratification2 :-> "", StratificationCategory3 :-> "", Stratification3 :-> "", GeoLocation :-> "(37.63864012300047, -120.99999953799971)", TopicID :-> "ALC", QuestionID :-> "ALC1_1", ResponseID :-> "", LocationID :-> 6, StratificationCategoryID1 :-> "OVERALL", StratificationID1 :-> "OVR", StratificationCategoryID2 :-> "", StratificationID2 :-> "", StratificationCategoryID3 :-> "", StratificationID3 :-> ""}
```

Notice that instead of being nothing, fields such as `DataValue` are defaulted to to 0.0 or their respective type defaults. As a result we can still do calculations on all rows, including those which might have holes in them.



## TODO
- Explain why the obvious approach to setting a value in list of records won't work and show how it's actually done

obvious attempt:

```haskell
λ> pipePreview transactions 2 (P.map (\r -> T.reverse (rget serviceArea r)))
"secivreS snerdlihC"
"secivreS snerdlihC"
```


actual:

```haskell
λ> pipePreview transactions 2 (P.map (\r -> rput serviceArea (T.reverse $ rget serviceArea r) r))
{Service Area :-> "secivreS snerdlihC", Account Description :-> "IT Services", Creditor :-> "123-REG.CO.UK", Transaction Date :-> Chicago (TimeIn 2014-04-23 05:00:00 UTC), JV Reference :-> 93, JV Date :-> Chicago (TimeIn 2014-05-20 05:00:00 UTC), JV Value :-> 143.81}
{Service Area :-> "secivreS snerdlihC", Account Description :-> "Equipment and Materials Repair", Creditor :-> "AFE SERVICELINE", Transaction Date :-> Chicago (TimeIn 2014-04-02 05:00:00 UTC), JV Reference :-> 6, JV Date :-> Chicago (TimeIn 2014-05-20 05:00:00 UTC), JV Value :-> 309.38}
```

- give concrete example of tableTypes invocation that creates Row so the link between the tableTypes type/invocation and the Row type is obvious
- give more context for generalizing
- introduce Select first to cut down on noise, possibly show that it's just Proxy :: TypeOf Column values being "projected"
- fix pipePreview equivalent example that uses take before the filter
- in first `rget lens row` example wrap it in parens to make precedence obvious
- delete spurious comment from distinctOn example
- use bindings for filters to make them named functions to encourage creating descriptive and composable pipelines as well as cut down on noise
- on distinctTopics example point out that we use id because unlike Prelude.fold, the Pipes.fold function also takes a callback applied to every accumulated value. Erase comment that is only useful for people that know what is going on.
- after building distinctTopics tell user about generalized distinctOn function in my library
