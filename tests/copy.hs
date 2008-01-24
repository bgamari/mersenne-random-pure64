import System.Random.Mersenne.Pure

main = do

    g <- init_genrand64 7

    let go :: Int -> PureMT -> IO ()
        go 0 _ = return ()
        go n p = do q <- copyPureMT p
                    go (n-1) q

    go 10000000 g -- 10s constant space
