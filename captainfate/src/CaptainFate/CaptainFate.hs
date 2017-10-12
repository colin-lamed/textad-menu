-- | An example story.
module CaptainFate.CaptainFate
  ( story
  , walkthrough
  ) where

import TextAd.Story

story :: StoryDef
story = do

  setSTitle "Captain Fate"
  setMaxScore 2

  let street            = Rid "street"
      boothR            = Rid "booth"
      cafe              = Rid "cafe"
      toilet            = Rid "toilet"

  let costume           = Oid "costume"
      clothes           = Oid "clothes"
      pedestrians       = Oid "pedestrians"
      booth             = Oid "booth"
      sidewalk          = Oid "sidewalk"
      outsideCafe       = Oid "outsideCafe"
      counter           = Oid "counter"
      food              = Oid "food"
      menu              = Oid "menu"
      customers         = Oid "customers"
      benny             = Oid "benny"
      coffee            = Oid "coffee"
      outsideToiletDoor = Oid "outsideToiletDoor"
      toiletKey         = Oid "toiletKey"
      note              = Oid "note"
      lightSwitch       = Oid "lightSwitch"
      lavatory          = Oid "lavatory"
      insideToiletDoor  = Oid "insideToiletDoor"
      coin              = Oid "coin"

  mkPlayer [costume] street

  wearingCostumeS   <- mkState "wearingCostumeS"   False
  lightOnS          <- mkState "lightOnS"          False
  toiletDoorOpenS   <- mkState "toiletDoorOpenS"   False
  toiletDoorLockedS <- mkState "toiletDoorLockedS" True

  mkObject costume $ do
    setOTitle "your costume"
    setONounType Quantitive
    setODescr $ printLn "STATE OF THE ART manufacture, from chemically reinforced 100% COTTON-lastic(tm)."
    setOCanPickUp True
    setOUse $ action $ do
      location <- currentRoom
      if      location == cafe   then printLn "Benny allows no monkey business in his establishment."
      else if location == street then printLn "In the middle of the street? That would be a PUBLIC SCANDAL, to say nothing of revealing your secret identity."
      else if location == boothR then printLn "Lacking Superman's super-speed, you realise that it would be awkward to change in plain view of the passing pedestrians."
      else if location == toilet then do
        lightOn          <- getState lightOnS
        toiletDoorOpen   <- getState toiletDoorOpenS
        toiletDoorLocked <- getState toiletDoorLockedS
        if lightOn
          then if toiletDoorOpen
            then printLn "The door to the bar stands OPEN at tens of curious eyes. You'd be forced to arrest yourself for LEWD conduct."
            else do printLn "You quickly remove your street clothes and bundle them up together into an INFRA MINUSCULE pack ready for easy transportation."
                    if toiletDoorLocked
                      then do printLn "Then you unfold your INVULNERABLE-COTTON costume and turn into Captain FATE, defender of free will, adversary of tyranny!"
                              setState wearingCostumeS True
                              destroyItem costume
                              takeItem clothes
                      else do printLn "Just as you are slipping into Captain FATE's costume, the door opens and a young woman enters. She looks at you and starts screaming, \"RAPIST! NAKED RAPIST IN THE TOILET!!!\"\n\n Everybody in the cafe quickly comes to the rescue, only to find you ridiculously jumping on one leg while trying to get dressed. Their laughter brings a QUICK END to your crime-fighting career!"
                              printLn "The End!"
                              printLn "Only joking. Let's do that again."
          else printLn "Last time you changed in the dark, you wore the suit inside out!"
      else printLn "There must be better places to change your clothes!" -- this _should_ never happen...


  mkObject clothes $ do
    setOTitle "your clothes"
    setODescr $ printLn "Perfectly ORDINARY-LOOKING street clothes for a NOBODY like John Covarth."
    setOCanPickUp True
    setOUse $ action $ printLn "The town NEEDS the power of Captain FATE, not the anonymity of John Covarth."

-----------------------------------------------------------------------------------------

  streetFirstTimeS <- mkState "streetFirstTimeS" True

  mkRoom street $ do
    setRTitle "Street"
    setRDescr $ do
      wearingCostume <- getState wearingCostumeS
      firstTime      <- getState streetFirstTimeS
      if wearingCostume
        then do printLn "You step onto the sidewalk, where the passing pedestrians recognise the rainbow EXTRAVAGANZA of Captain FATE's costume and cry your name in awe as you JUMP with sensational momentum into the BLUE morning skies!"
                printLn "The End!"
        else do when firstTime $ do
                  printLn "\n\nImpersonating mild mannered John Covarth, assistant help boy at an insignificant drugstore, you suddenly STOP when your acute hearing deciphers a stray radio call from the POLICE. There's some MADMAN attacking the population in Granary Park! You must change into your Captain FATE costume fast...!\n\n"
                  setState streetFirstTimeS False
                printLn "On one side -- which your HEIGHTENED sense of direction indicates is NORTH -- there's an open cafe now serving lunch. To the south, you can see a phone booth."
    setRExits $ do
      exit "Cafe" N cafe
      exit "Booth" S boothR
    setRItems [pedestrians, booth, sidewalk, outsideCafe]

  mkObject pedestrians $ do
    setOTitle "pedestrians"
    setODescr $ printLn "They're just PEOPLE going about their daily HONEST business."
    setONounType Quantitive
    setOIsPlural True
    -- setOUse $ with lockedClockDoor $ do
    --   printLn "The passing pedestrians are of NO concern to you."

  mkObject booth $ do
    setOTitle "phone booth"
    setONounType Quantitive
    setODescr $ printLn "It's one of the old picturesque models, a red cabin with room for one caller."

  mkObject sidewalk $ do
    setOTitle "sidewalk"
    setONounType Quantitive
    setODescr $ printLn "You make a quick surveillance of the sidewalk and discover much to your surprise that it looks JUST like any other sidewalk in the CITY!"

  mkObject outsideCafe $ do
    setOTitle "Benny's cafe"
    setODescr $ printLn "The town's favourite for a quick snack, Benny's cafe has a 50's ROCKETSHIP look."
    setONounType Proper

--------------------------------------------------------------------------------

  mkRoom boothR $ do
    setRTitle "Inside Booth"
    setRDescr $ printLn "It's one of the old picturesque models, a red cabin with room for one caller."
    setRExits $ do
      exit "Leave Booth" N street

--------------------------------------------------------------------------------

  firstViewToiletDoorS <- mkState "firstViewToiletDoorS" True
  readNoteOnceS        <- mkState "readNoteOnceS"        False
  keyAskedForS         <- mkState "keyAskedFor"          False
  coffeeAskedForS      <- mkState "coffeeAskedForS"      False
  bennyHasKeyS         <- mkState "bennyHasKeyS"         True
  coffeeNeedsPayingS   <- mkState "coffeeNeedsPayingS"   False

  mkRoom cafe $ do
    setRTitle "Inside Benny's cafe"
    setRDescr $ do
      printLn "Benny's offers the FINEST selection of pastries and sandwiches. Customers clog the counter, where Benny himself manages to serve, cook and charge without missing a step. At the north side of the cafe you can see a red door connecting with the toilet."
      wearingCostume <- getState wearingCostumeS
      when wearingCostume $ do
        printLn "Nearby customers glance at your costume with open curiosity."
        let comments = [ "\"Didn't know there was a circus in town,\" comments one customer to another. \"Seems like the clowns have the day off.\""
                       , "\"These fashion designers don't know what to do to show off,\" snorts a fat gentleman, looking your way. Those within earshot try to conceal their smiles."
                       , "\"Must be carnival again,\" says a man to his wife, who giggles, stealing a peek at you. \"Time sure flies.\""
                       , "\"Bad thing about big towns\", comments someone to his table companion, \"is you get the damnedest bugs coming out from toilets.\""
                       , "\"I sure WISH I could go to work in my pyjamas,\" says a girl in an office suit to some colleagues. \"It looks SO comfortable.\""
                       ]

        -- TODO pick one randomly
        printLn $ head comments

    setRExits $ do
      toiletDoorOpen <- getState toiletDoorOpenS
      when toiletDoorOpen $ do
        exit "Toilet" N toilet
      exit "Street" S $ action $ do
        bennyHasKey       <- getState bennyHasKeyS
        coffeeNeedsPaying <- getState coffeeNeedsPayingS
        if coffeeNeedsPaying || not bennyHasKey
          then do printLn "Just as you are stepping into the street, the big hand of Benny falls on your shoulder."
                  printLn $ case (coffeeNeedsPaying, not bennyHasKey) of
                    (True, True)  -> "\"Hey! You've got my key and haven't paid for the coffee. Do I look like a chump?\" You apologise as only a HERO knows how to do and return inside."
                    (True, False) -> "\"Just waidda minute here, Mister,\" he says. \"Sneaking out without paying, are you?\" You quickly mumble an excuse and go back into the cafe. Benny returns to his chores with a mistrusting eye."
                    (False, True) -> "\"Just where you think you're going with the toilet key?\" he says. \"You a thief?\" As Benny forces you back into the cafe, you quickly assure him that it was only a STUPEFYING mistake."
                  return Nothing
          else return $ Just street

    setRItems [counter, menu, customers, benny, outsideToiletDoor]

  mkObject counter $ do
    setOTitle "counter"
    setODescr $ printLn "The counter is made of an astonishing ALLOY of metals, STAIN-PROOF, SPILL-RESISTANT and VERY EASY to clean. Customers enjoy their snacks with UTTER tranquillity, safe in the notion that the counter can take it all."

  mkObject menu $ do
    setOTitle "menu"
    setODescr $ printLn "The menu board lists Benny's food and drinks, along with their prices. Too bad you've never learnt how to read, but luckily there is a picture of a big cup of coffee among the incomprehensible writing."

  mkObject customers $ do
    setOTitle "customers"
    setONounType Quantitive
    setOIsPlural True
    setODescr $ do
      wearingCostume <- getState wearingCostumeS
      if wearingCostume
        then printLn "Most seem to be concentrating on their food, but some do look at you quite blatantly. Must be the MIND-BEFUDDLING colours of your costume."
        else printLn "A group of HELPLESS and UNSUSPECTING mortals, the kind Captain FATE swore to DEFEND the day his parents choked on a DEVIOUS slice of RASPBERRY PIE."

  mkObject benny $ do
    setOTitle "Benny"
    setONounType Proper
    setODescr $ printLn "A deceptively FAT man of uncanny agility, Benny entertains his customers crushing coconuts against his forehead when the mood strikes him."
    talkO $ do
      readNoteOnce   <- getState readNoteOnceS
      keyAskedFor    <- getState keyAskedForS
      coffeeAskedFor <- getState coffeeAskedForS
      bennyHasKey    <- getState bennyHasKeyS
      when readNoteOnce $ do
        say "Can I have the toilet key?" $ do
          printLn "You say \"Hello, can I have the toilet key?\"."
          if coffeeAskedFor
            then do
              if bennyHasKey
                then do printLn "Benny tosses the key to the rest rooms on the counter, where you grab it with a dextrous and precise movement of your HYPER-AGILE hand."
                        takeItem toiletKey
                        setState bennyHasKeyS False
                else  printLn "\"Last place I saw that key, it was in YOUR possession,\" grumbles Benny. \"Be sure to return it before you leave.\""
            else printLn "\"Toilet is only fer customers,\" he grumbles, looking pointedly at a menu board behind him."
          setState keyAskedForS True
      when (keyAskedFor && not coffeeAskedFor) $ do
        say "Can I have a coffee?" $ do
          printLn "You say \"Hello, can I have a coffee?\"."
          printLn "With two gracious steps, Benny places his world-famous Cappuccino in front of you."
          addItem cafe coffee
          setState coffeeAskedForS True
          setState coffeeNeedsPayingS True

  mkObject coffee $ do
    setOTitle "cup of coffee"
    setODescr $ printLn "It smells delicious."
    setOCanPickUp True

  let useToiletDoor = action $ do
        toiletDoorLocked <- getState toiletDoorLockedS
        toiletDoorOpen   <- getState toiletDoorOpenS
        if toiletDoorLocked
          then printLn "The toilet door is locked"
          else do printLn $ "The toilet door is now " <> if toiletDoorOpen then "closed" else "open"
                  setState toiletDoorOpenS (not toiletDoorOpen)

  mkObject outsideToiletDoor $ do
    setOTitle "door to the toilet"
    setODescr $ do
      firstViewToiletDoor <- getState firstViewToiletDoorS
      when firstViewToiletDoor $ do
        addItem cafe note
        setState firstViewToiletDoorS False
      printLn "A red door with the unequivocal black man-woman silhouettes marking the entrance to hygienic facilities. There is a scribbled note stuck on its surface."
    setOUse useToiletDoor

  let useToiletDoorWithKey = do
        toiletDoorLocked <- getState toiletDoorLockedS
        if toiletDoorLocked
          then do printLn "You unlock and open the toilet door"
                  setState toiletDoorOpenS   True
                  setState toiletDoorLockedS False
          else do printLn "You close and lock the toilet door"
                  setState toiletDoorOpenS   False
                  setState toiletDoorLockedS True

  mkObject toiletKey $ do
    setOTitle "toilet key"
    setODescr $ do
      wearingCostume <- getState wearingCostumeS
      if wearingCostume
        then printLn "the used and IRRELEVANT key"
        else printLn "the CRUCIAL key"
    setOCanPickUp True
    setOUse $ do
      with benny $ do
        printLn "Benny nods as you ADMIRABLY return his key."
        destroyItem toiletKey
        setState bennyHasKeyS True
      with outsideToiletDoor useToiletDoorWithKey
      with insideToiletDoor  useToiletDoorWithKey


  mkObject note $ do
    setOTitle "scribbled note"
    setODescr $ do
      readNoteOnce <- getState readNoteOnceS
      if readNoteOnce
        then printLn "The scorched undecipherable note holds no SECRETS from you NOW! Ha!"
        else do
          printLn "You apply your ENHANCED ULTRAFREQUENCY vision to the note and squint in concentration, giving up only when you see the borders of the note begin to blacken under the incredible intensity of your burning stare. You reflect once more how helpful it would've been if you'd ever learnt to read."
          printLn "A kind old lady passes by and explains:"
          printLn "\"You have to ask Benny for the key, at the counter.\""
          printLn "You turn quickly and begin, \"Oh, I KNOW that, but...\""
          printLn "\"My pleasure, son,\" says the lady, as she exits the cafe."
          setState readNoteOnceS True
    setOCanPickUp False

--------------------------------------------------------------------------------

  lavatoryFirstViewS <- mkState "lavatoryFirstViewS" True

  mkRoom toilet $ do
    setRTitle "Unisex toilet"
    setRDescr $ printLn "A surprisingly CLEAN square room covered with glazed-ceramic tiles, featuring little more than a lavatory and a light switch. The only exit is south, through the door and into the cafe."
    setRExits $ do
      toiletDoorOpen      <- getState toiletDoorOpenS
      when toiletDoorOpen $ do
        exit "Back to cafe" S cafe
    setRItems [lightSwitch, lavatory, insideToiletDoor]

  mkObject lightSwitch $ do
    setOTitle "light switch"
    setODescr $ printLn "A notorious ACHIEVEMENT of technological SCIENCE, elegant yet EASY to use."
    setOUse $ action $ do
      lightOn <- getState lightOnS
      printLn $ "The light is now " <> if lightOn then "off" else "on" <> "."
      setState lightOnS (not lightOn)

  mkObject lavatory $ do
    setOTitle "lavatory"
    setODescr $ do
      lightOn <- getState lightOnS
      if lightOn
        then do
          lavatoryFirstView <- getState lavatoryFirstViewS
          if lavatoryFirstView
            then do printLn "The latest user CIVILLY flushed it after use, but failed to pick up the VALUABLE coin that fell from his pants."
                    takeItem coin
                    setState lavatoryFirstViewS False
            else printLn "It's just a dirty, well used lavatory."
        else printLn "It's too dark to see"
    setOUse $ do
      with coin $ do
        printLn "While any other MORTALS might unwittingly throw just about ANYTHING into the lavatory, you remember the WISE teachings of your mentor, Duke ELEGANT, about elderly plumbing and rising waters."

  mkObject insideToiletDoor $ do
    setOTitle "toilet door"
    setODescr $ printLn "A red door with no OUTSTANDING features."
    setOUse useToiletDoor

  mkObject coin $ do
    setOTitle "valuable coin"
    setODescr $ printLn "It's a genuine SILVER QUIDBUCK."
    setOCanPickUp True
    setOUse $ do
      with benny $ do
        wearingCostume <- getState wearingCostumeS
        let clothes = if wearingCostume then "BULLET-PROOF costume" else "ordinary street clothes"
        printLn $ "With marvellous ILLUSIONIST gestures, you produce the coin from the depths of your " <> clothes <> " as if it had dropped on the counter from Benny's ear! People around you clap politely. Benny takes the coin and gives it a SUSPICIOUS bite. \"Thank you, sir. Come back anytime,\" he says."
        setState coffeeNeedsPayingS False


--------------------------------------------------------------------------------


-- | provides complete solution, which can be useful for testing app
-- e.g. replaying as cookie
walkthrough :: Text
walkthrough = intercalate "\n" $ reverse
  [ "go Cafe"
  , "examine door to the toilet"
  , "examine scribbled note"
  , "talk to Benny"
  , "say Can I have the toilet key?"
  , "talk to Benny"
  , "say Can I have a coffee?"
  , "talk to Benny"
  , "say Can I have the toilet key?"
  , "use toilet key with door to the toilet"
  , "go Toilet"
  , "use your hand with light switch"
  , "examine lavatory"
  , "use toilet key with toilet door"
  , "use your hand with your costume"
  , "use toilet key with toilet door"
  , "go Back to cafe"
  , "use toilet key with Benny"
  , "use valuable coin with Benny"
  , "go Street"
  ]
