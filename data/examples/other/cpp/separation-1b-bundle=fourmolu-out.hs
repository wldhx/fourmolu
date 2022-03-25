<<<<<<< HEAD:data/examples/other/cpp/separation-1b-bundle=fourmolu-out.hs
{-# LANGUAGE CPP #-}

decompressingPipe ::
    (PrimMonad m, MonadThrow m, MonadResource m) =>
    CompressionMethod ->
    ConduitT ByteString ByteString m ()
=======
decompressingPipe
    :: (PrimMonad m, MonadThrow m, MonadResource m)
    => CompressionMethod
    -> ConduitT ByteString ByteString m ()
>>>>>>> expiplus1/joe-align-leading-arrow:data/examples/other/cpp/separation-1b-four-out.hs
decompressingPipe Store = C.awaitForever C.yield
decompressingPipe Deflate = Z.decompress $ Z.WindowBits (-15)
#ifdef ENABLE_BZIP2
decompressingPipe BZip2   = BZ.bunzip2
#else
decompressingPipe BZip2   = throwM BZip2Unsupported
#endif

foo :: Int
foo = undefined
