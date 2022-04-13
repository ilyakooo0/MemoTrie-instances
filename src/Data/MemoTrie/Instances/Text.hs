{-# OPTIONS_GHC -Wno-orphans #-}

module Data.MemoTrie.Instances.Text () where

import Data.Foldable (for_)
import Data.MemoTrie
import Data.Text.Array
import qualified Data.Text.Internal as T
import qualified Data.Text.Lazy as TL
import Data.Word

instance HasTrie T.Text where
  newtype T.Text :->: b = TextTrie ((Int, [Word16]) :->: b)
  trie f = TextTrie $ trie $ f . textFromList
  untrie (TextTrie x) = untrie x . textToList
  enumerate (TextTrie x) = (\(a, b) -> (textFromList a, b)) <$> enumerate x

instance HasTrie TL.Text where
  newtype TL.Text :->: b = LazyTextTrie (T.Text :->: b)
  trie f = LazyTextTrie $ trie $ f . TL.fromStrict
  untrie (LazyTextTrie x) = untrie x . TL.toStrict
  enumerate (LazyTextTrie x) = (\(a, b) -> (TL.fromStrict a, b)) <$> enumerate x

textToList :: T.Text -> (Int, [Word16])
textToList (T.Text arr off len) = (len, toList arr off len)

textFromList :: (Int, [Word16]) -> T.Text
textFromList (len, ws) = T.text arr 0 len
  where
    arr = run $ do
      a <- new len
      for_ (zip [0 ..] ws) $ \(i, w) -> unsafeWrite a i w
      pure a
