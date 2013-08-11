{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Character
    ( Character (..),
      getClassFromType,
      characterToList,
    ) where

import qualified Data.Text as T

------------------------------------------------------------------------------
-- | A character
data Character = Character
                 { cName     :: T.Text
                 , cLevel    :: T.Text
                 , cType     :: Int
                 , cStatePts :: Int
                 , cInt      :: Int
                 , cStr      :: Int
                 , cDex      :: Int
                 , cAgi      :: Int
                 , cVit      :: Int
                 , cExp      :: Int
                 , cPosX     :: Int
                 , cPosY     :: Int
                 } deriving (Show, Eq)

--TODO: Write each class name from the game
getClassFromType :: Int -> T.Text
getClassFromType 1 = T.pack $ "Mage"
getClassFromType 2 = T.pack $ "Archer"
getClassFromType 3 = T.pack $ "Priest"
getClassFromType 4 = T.pack $ "Warrior"
getClassFromType 5 = T.pack $ "Thief";
getClassFromType 6 = T.pack $ "Engineer";
getClassFromType _ = T.pack $ "Unknown"

characterToList :: Character -> [(T.Text, T.Text)]
characterToList Character{..} =
  [ ("characterName", cName)
  , ("characterLevel", cLevel)
  , ("characterType", getClassFromType cType)
  , ("characterStatePts", sh cStatePts)
  , ("characterInt", sh cInt)
  , ("characterStr", sh cStr)
  , ("characterDex", sh cDex)
  , ("characterAgi", sh cAgi)
  , ("characterVit", sh cVit)
  , ("characterExp", sh cExp)
  , ("characterPosX", sh cPosX)
  , ("characterPosY", sh cPosY)
  ]
  where
    sh = T.pack . show
