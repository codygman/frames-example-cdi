frames example using US chronic disease indicators
===================================================

### 1. Clone this repo

### 2. Follow along

Previewing the data:

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

```haskell
```

```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
```haskell
```
