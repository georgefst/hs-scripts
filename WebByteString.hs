{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module WebByteString where

#ifdef wasi_HOST_OS

import Data.Array.Byte
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B
import Data.ByteString.Short.Internal qualified as SB
import Data.Coerce
import Data.Text.Encoding
import Data.Text.IO.Utf8 qualified as T
import Data.Text.Internal
import GHC.Exts
import GHC.IO
import GHC.Wasm.Prim

newtype JSUint8Array = JSUint8Array JSVal

foreign import javascript unsafe "$1.byteLength"
    js_buf_len :: JSUint8Array -> IO Int

foreign import javascript unsafe "(new Uint8Array(__exports.memory.buffer, $2, $1.byteLength)).set($1)"
    js_from_buf :: JSUint8Array -> Ptr a -> IO ()

foreign import javascript unsafe "(new Uint8Array(__exports.memory.buffer, $1, $2))"
    js_to_buf :: Ptr a -> Int -> IO JSUint8Array

byteStringFromJSBytes :: JSUint8Array -> SB.ShortByteString
byteStringFromJSBytes buf = unsafeDupablePerformIO $ do
    I# len# <- js_buf_len buf
    IO $ \s0 -> case newByteArray# len# s0 of
        (# s1, mba# #) -> case unIO (js_from_buf buf (Ptr (mutableByteArrayContents# mba#))) s1 of
            (# s2, _ #) -> case unIO (freeJSVal (coerce buf)) s2 of
                (# s3, _ #) -> case unsafeFreezeByteArray# mba# s3 of
                    (# s4, ba# #) -> (# s4, SB.ShortByteString (ByteArray ba#) #)

byteStringToJSBytes :: SB.ShortByteString -> JSUint8Array
byteStringToJSBytes sbs@(SB.ShortByteString (ByteArray ba#)) = unsafeDupablePerformIO $
    IO $ \s0 -> case newPinnedByteArray# len# s0 of
        (# s1, mba# #) -> case copyByteArray# ba# 0# mba# 0# len# s1 of
            s2 -> keepAlive# mba# s2 $ unIO $ js_to_buf (Ptr (mutableByteArrayContents# mba#)) $ I# len#
    where
        I# len# = SB.length sbs

main :: IO ()
main = do
    B.putStr $ SB.fromShort $ byteStringFromJSBytes $ byteStringToJSBytes $ SB.toShort $ encodeUtf8 "lorem ipsum"
    putStrLn ""

#endif
