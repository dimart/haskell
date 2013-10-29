data M a b = E 
	     |M 
	        Integer  -- height
	        (a, [b]) -- (key, [values])
	        (M a b)  -- left
	        (M a b)  -- right
	deriving Show

--help functions
h E                  = 0
h (M height _ _ _)   = height
make p l r = M (max (h l) ((h r)+1)) p l r   
fmin m@( M _ n E E ) = m
fmin   ( M _ _ l _ ) = fmin l
val  (M _ n _ _)     = n
--
balance m@(M height pair@(k, v) l r) 
	| h l >  h r + 1 = rotR pair l r
	| h l == h r + 1 = make pair l r
	| h r >  h l + 1 = rotL pair l r 
	| h r == h l + 1 = make pair l r
	| otherwise      = m
	where 
	      rotR p (M _ p' ll lr) r = if h ll < h lr then rotR p  (rotL p' ll lr)  r
						       else make p' ll (make p lr r  )  
	      rotL p l (M _ p' rl rr) = if h rl > h rr then rotL p  l (rotR p' rl rr)             
                                                       else make p' (make p  l  rl)  rr
------------------------------------------------------------------------------------------------

emptyMap = E

insert E (k, v) = M 1 (k, [v]) E E
insert (M  ht (k, vs) l r) p@(k', v')
        | k' < k   = balance (M  ht (k, vs)  (insert l p)  r)
        | k' > k   = balance (M  ht (k, vs)  l  (insert r p))
        |otherwise = M ht (k, v':vs) l r

find E _ = Nothing
find (M _ (k, v:vs) l r) k'
        |k' < k    = find l k'
        |k' > k    = find r k'
        |otherwise = Just v

remove E _ = E
remove m@(M  ht pair@(k, vs)  l  r) k'
        |k' < k    = balance $ make pair (remove l k') r
        |k' > k    = balance $ make pair (remove r k') l
        |otherwise = case vs of
			[v]   -> remNode m
			v:vs' -> (M ht (k, vs') l r) 

remNode (M _ _ E E) = E
remNode (M _ _ l E) = l
remNode (M _ _ E r) = r
remNode (M _ _ l r) = make (val (fmin r)) l (remNode (fmin r))		
