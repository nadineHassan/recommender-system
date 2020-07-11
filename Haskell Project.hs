import System.Random 
import System.IO.Unsafe






users = ["user1", "user2", "user3", "user4"]
items = ["item1", "item2", "item3", "item4", "item5", "item6"]
purchasesHistory = [ ("user1", [["item1", "item2", "item3"] , ["item1", "item2", "item4"]]) , 
                      ("user2", [["item2", "item5"] , ["item4", "item5"]]) , 
					  ("user3", [["item3", "item2"]]) , 
					  ("user4", []) ]
 
 
 
createEmptyFreqList []=[]
createEmptyFreqList (x:xs)=(x,[]):(createEmptyFreqList xs)



countItems i1 i2 []=0               --helper counts two elements together in a list of lists
countItems i1 i2 (p:ps)| elem i1 p && elem i2 p =1+countItems i1 i2 ps
                       |otherwise =countItems i1 i2 ps 


concat1[]=[]                                 --create list of tupels
concat1 ((item,[]):z)=[]++concat1 z
concat1 ((item,(x:xs)):z)= (x:xs)++concat1 z

tupelContain item []=False                                --check if list of tupels contain an item
tupelContain item ((x,n):xs)= if (item==x) then True else tupelContain item xs

sumTupel x []=0
sumTupel x ((y,n):z)= if(x==y) then n+sumTupel x z else sumTupel x z

sumTupelItems [] _=[]
sumTupelItems (x:xs) y= if (tupelContain x y) then (x,(sumTupel x y)):sumTupelItems xs y else sumTupelItems xs y

freqListItemsHelper i ((user,x):xs)=if (i==user) then sumTupelItems items (concat1 x) else freqListItemsHelper i xs
freqListItems  x=freqListItemsHelper x (getAllUsersStats (purchasesHistory))


getStatForItem  i1 [] p=[]               --helper get statistic for one item of one user
getStatForItem  i1 (i:is) p = if ((i/=i1 )&& ((countItems i1 i p)>0) )then (  (i,countItems i i1 p):(getStatForItem i1 is p))
                                                               else  (getStatForItem i1 is p )
															   
															   
															   
getOneUserStats [] (i:is) p = []		--helper get statistic of all items of a certain user												   
getOneUserStats (id:ids) (i:is) p= ((id,(getStatForItem id (i:is) p))	:(getOneUserStats ids (i:is) p ))


getAllUsersStats [] =[]
getAllUsersStats ((u,p):ps) = ((u,getOneUserStats items items  p):getAllUsersStats ps)


count i []=0   --helper counts how many times an item occur in a list of lists
count i (p:ps)=if(elem i p) then 1+count i ps 
                            else count i ps
							
							
countAllItemsMul2 [] p=[]  --helper count elemets one user buyes and multiplies by 2  							
countAllItemsMul2 (i:is) p	= if (count i p >0)then ((i,2*(count i p)):countAllItemsMul2 is p)
                                               else countAllItemsMul2 is p 						
							
							

countItemCart i [] p=0     -- helper count occurences of item i with every element from cart
countItemCart i (c:cs) p =if (i/=c)then(( countItems i c p )+ (countItemCart i cs p))
                                            else (countItemCart i cs p)

											
countItemsCart  [] (c:cs) p=	[]	-- helper count occurences of all items with respect to catr								
countItemsCart (i:is) (c:cs) p =if (countItemCart i (c:cs) p )>0 then((i,countItemCart i (c:cs) p):countItemsCart is (c:cs) p	)									
                                                                              else  countItemsCart is (c:cs) p
																			  																		  
																			  
freqListCartHelper u1 (c:cs)  []=[]		-- helper freqListCart to take purchasesHistory															  
freqListCartHelper u1 (c:cs) ((u,p):ps)	= if u1==u then countItemsCart items (c:cs) p
                                                   else freqListCartHelper u1 (c:cs) ps	

freqListCart u1	(c:cs) =freqListCartHelper u1 (c:cs) purchasesHistory											   
																	

freqListCartAndItems u1 (c:cs) =addTwoListUnordered (freqListItems u1) (freqListCart u1 (c:cs))




addTwoListUnordered [] []=[]    --helper adds the items in two unordered lists
addTwoListUnordered (i1:i1s) []=(i1:i1s)
addTwoListUnordered [] (i2:i2s) = (i2:i2s)
addTwoListUnordered ((i1,f1):i1s) ((i2,f2):i2s)= if(element i1 ((i2,f2):i2s)) then ((addTwoItems (i1,f1)( extract i1 ((i2,f2):i2s))):(addTwoListUnordered i1s (remove i1 ((i2,f2):i2s))))
                                                                              else ((i1,f1):(addTwoListUnordered i1s ((i2,f2):i2s)))
																			  
element i1 []=False		   --helper returns if i1 is present in the pair																	  
element i1 ((i,f):is) = if( i1==i) then True else element i1 is

extract i1 []=("b",0)       --helper extracts pair from list of pairs
extract i1 ((i,a):is)= if (i1==i) then (i,a) 
                                   else extract i1 is 
								   
								   
remove i1 []=[]               --helper removes pair from a list of pairs 
remove  i1 ((i,f):is)=if(i1==i) then is 
                                else ((i,f):(remove i1 is))							   
								   
addTwoItems (i1,f) (i2,f2)=(i1,(f+f2)) --helper takes two pairs and returns one pair with frequency added



addTwoListUnorderedAllItems [] []=[]     --helper adds all items of one user 
addTwoListUnorderedAllItems ((i1,a):i1s) ((i2,b):i2s) =if(a/= [] && b/=[]) then ((i1,addTwoListUnordered a b):addTwoListUnorderedAllItems i1s i2s)
                                                                           else addTwoListUnorderedAllItems i1s i2s
																		   
																		   
purchasesIntersection x []=[]																	   
purchasesIntersection x ((u,y):us) =((addTwoListUnorderedAllItems x y ):purchasesIntersection x us)	

																   
addItemInOneUser i1 []=0     --helper adds one item in one user
addItemInOneUser i1 ((i,a):is)= if(element i1 a ) then (extractNumber i1 a )+addItemInOneUser i1 is
                                                      else addItemInOneUser i1 is
													  											  

extractNumber i1 []=0     --helper extracts the number from pair of numbers
extractNumber i1 ((i,a):is)= if (i1==i) then a 
                                        else extractNumber i1 is 
										
										
addItemInAllUsers i1 []=0		--helper counts the frequency of one item in all user purchasess intersection											  
addItemInAllUsers i1 (i:is)= ((addItemInOneUser i1 i )+addItemInAllUsers i1 is)



addAllItemsAllUsers [] x=[]    --helper produces a list of adding each item in all users
addAllItemsAllUsers (i:is) x =if (addItemInAllUsers i x  >0) then ((i,addItemInAllUsers i x):addAllItemsAllUsers is x)	
                                                             else addAllItemsAllUsers is x

getUserFromStat u1 []=[]    --helper extracts the statistic of one user
getUserFromStat u1 ((u,s):ss)= if(u1==u) then s 
                                         else getUserFromStat u1 ss	
										 
removeUserFromStat u1 []=[]    --helper removes the u1 from the statistics and keepse all other users
removeUserFromStat u1 ((u,s):ss) =	if(u1==u) then ss
                                              else (u,s):removeUserFromStat u1 ss	

freqListUsers u1 = (addAllItemsAllUsers items (purchasesIntersection (getUserFromStat u1 (getAllUsersStats purchasesHistory)) (removeUserFromStat u1 (getAllUsersStats purchasesHistory))))


extractItem 0 ((i,a):is)=i    --helper extracts item in a given index
extractItem n ((i,a):is)= extractItem (n-1) is 

										
randomZeroToX :: Int -> Int 	--helper generates random number from o to x														   
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))
											  
											  

chooseRandomIndex []=[]  --helper takes a list and chooses a random index in it and onlyy extracts the element
chooseRandomIndex a = extractItem (    randomZeroToX      ((length (replicateList a )) -1)            )      (  replicateList a   )    





replicateItem  0 a =[]
replicateItem  n a=(a:(replicateItem (n-1) a))



replicateList []=[]
replicateList ((a,b):xs) =((replicateItem b (a,b))++(replicateList xs))

recommendEmptyCart u =chooseRandomIndex (freqListItems u)



recommendBasedOnItemsInCart u [] =recommendEmptyCart u
recommendBasedOnItemsInCart u  (x:xs) =chooseRandomIndex (freqListCartAndItems u (x:xs))


recommendBasedOnUsers  u = chooseRandomIndex (freqListUsers u)


selectItem 0 (i:is)=i
selectItem n (i:is)=selectItem (n-1) is


recommend u x  | recommendBasedOnItemsInCart u x==[] &&  recommendBasedOnUsers u ==[] = items!!(randomZeroToX ((length items )-1))
                    | recommendBasedOnItemsInCart u x==[] =recommendBasedOnUsers u
					| recommendBasedOnUsers u ==[] = recommendBasedOnItemsInCart u x
					| otherwise = selectItem (randomZeroToX 1) ((recommendBasedOnItemsInCart u x ):(recommendBasedOnUsers u):[])

