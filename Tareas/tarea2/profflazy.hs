proofflazy :: Int -> Int
proofflazy 0 = let x = (head []) in 2+3
proofflazy 1 = let x = (head []) in x

r1 = proofflazy 0
r2 = proofflazy 1
