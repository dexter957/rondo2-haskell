import Data.List
import Data.Bool
import System.IO

--Question 1

{-
Έστω ότι έχουμε μια συνάρτηση f(i,j,k).Τότε βάσει της εκφώνησης,ζητείται το εξής:
			sum=0;
			for(i=1;i<=a;++i)
			{
				for(j=1;j<b;++j)
				{
					for(k=1;k<=c;++k)
					{
						sum+=f(i,j,k);
					}
				}
			}
Οι συναρτήσεις που ακολουθούν υλοποιούν σε Haskell την παραπάνω λογική
-}


f :: Int->Int->Int->Int

f i j k = 1


sssum :: (Int->Int->Int->Int)->Int->Int->Int->Int

sssum f a b c = topBreadIteration f 1 a b c


bottomBreadIteration :: (Int->Int->Int->Int)->Int->Int->Int->Int->Int

bottomBreadIteration f i j k c 
								|(k<=c)= f i j k +bottomBreadIteration f i j (k+1) c
								|otherwise = 0


hamIteration ::  (Int->Int->Int->Int)->Int->Int->Int->Int->Int

hamIteration f i j b c
						|(j<=b) = (bottomBreadIteration f i j 1 c ) + hamIteration f i (j+1) b c
						|otherwise =0


topBreadIteration ::  (Int->Int->Int->Int)->Int->Int->Int->Int->Int

topBreadIteration f i a b c 
							|(i<=a) = (hamIteration f i 1 b c ) + topBreadIteration f (i+1) a b c
							|otherwise =0

---------------------------------------------------------------------------------------------------------------------------------------------

--Question 2

checkForCool2 :: [Int]->Int->Bool

checkForCool2 [] two 
					|(two==2)=True
					|otherwise= False

checkForCool2 (x:xs) two 
						|(two==2)=True
						|((x==0) || (x==2))=checkForCool2 xs (two+1)
						|otherwise = checkForCool2 xs 0

checkForCool3 :: [Int]->Int->Bool

checkForCool3 [] three 
						|(three==3)=True
						|otherwise=False
checkForCool3 (x:xs) three
							|(three==3)=True
							|((x==0)||(x==3))=checkForCool3 xs (three+1)
							|otherwise=checkForCool3 xs 0

checkForCool5 :: [Int]->Int->Bool

checkForCool5 [] five 
						|(five==5) = True
						|otherwise = False
checkForCool5 (x:xs) five
							|(five==5)=True
							|((x==0)||(x==5))=checkForCool5 xs (five+1)
							|otherwise=checkForCool5 xs 0

checkForCool7 :: [Int]->Int->Bool

checkForCool7 [] seven 
						|(seven==7) = True
						|otherwise = False
checkForCool7 (x:xs) seven
							|(seven==7)=True
							|((x==0)||(x==7))=checkForCool7 xs (seven+1)
							|otherwise=checkForCool7 xs 0




cool :: [Int]->Bool

cool (x:xs)
			|(checkForCool2 (x:xs) 0)==True=True
			|(checkForCool3 (x:xs) 0)==True=True
			|(checkForCool5 (x:xs) 0)==True=True
			|(checkForCool7 (x:xs) 0)==True=True
			|otherwise=False
----------------------------------------------------------------------------------------------------------------------

--Question 3
{-
Η συλλογιστική της επίλυσης αυτής της άσκησης εδράζεται στο ότι η σχετική θέση 2 στοιχείων (ποιο προγείται και ποιο έπεται ποιου)
 μπορεί ν'αλλάξει το πολύ 2 φορές, αν δηλαδή σε 2 διαφορετικές μεταθέσεις-λίστες , και αν μετακινηθούν έτσι που να αλλάζει η αρχική σχετική τους
 θέση. Οπότε τα βήματα της επίλυσης έχουν ως εξής:
 -Αρχικά κάθε λίστα-μετάθεση σπάει σε λίστες των 2 στοιχείων με χρήση built in συναρτήσεων της Haskell.Κατά τη διαδικασία αυτή η σειρά διατηρείται
 καθώς σε όλες τις λίστες που προκύπτουν,το πρώτο στοιχείο προηγείται του δεύτερου σε κάθε περίπτωση.
 -Στη συνέχεια αυτές οι λίστες συνενώνονται σε μία ενιαία.
 -Ακολούθως,από αυτήν τη λίστα,μπορούμε να βρούμε,για κάθε στοιχείο,ποια προηγούνται αυτού (και ποια έπονται).
 Για κάθε στοιχείο λοιπόν φτιάχνουμε μια λίστα με όσα προηγούνται αυτού,βάσει των λιστών των 2 στοιχείων.Έτσι καταλήγουμε να έχουμε μια λίστα για κάθε
 στοιχείο,με όσα στοιχεία προηγούνται αυτού σε καθεμιά από τις λίστες που μάς δόθηκαν σαν όρισμα.
-Προφανώς η εν λόγω λίστα θα έχει διπλότυπα.Αλλά εδώ τα διπλότυπα δε μάς ενοχλούν; αντίθετα μάς χρησιμεύουν.Αυτό συμβαίνει γιατί,όπως προειπώθηκε,
η σχετική θέση 2 στοιχείων "χαλάει" το πολύ 2 φορές.Επομένως,όσα διπλότυπα συναντώνται,τουλάχιστον 3 φορές σε μια λίστα,πάει να πει ότι προηγούνται 
του στοιχείου αυτού και στην αρχική λίστα,αυτήν που ψάχνουμε.Φτιάχνουμε λοιπόν για κάθε στοιχείο μια λίστα,με όλα τα διπλότυπα που συναντάμε 
τουλάχιστον 3 φορές.(και μετά σβήνουμε τα διπλότυπα από αυτήν τη λίστα,γιατί τώρα δε τα χρειαζόμαστε άλλο)
-Μέχρι τώρα λοιπόν έχουμε φτιαξει,για κάθε στοιχείο της λίστας που ψάχνουμε,μια λίστα με όσα σίγουρα προηγούνται αυτού στη λίστα που ψάχνουε.
-Εν συνεχεία βρίσκουμε για κάθε στοιχείο τη θέση του μέσα στη ζητούμενη λίστα,βάσει του αριθμού των στοιχείων που προηγούνται.
-Με βάση και την τελευταία μας γνώση,φτιάχνουμε τη λίστα.
-}


breakInTwos :: [[Int]]->[[Int]]
--Δέχεται μαι λίστα με λίστες και σπάει καθεμιά από αυτές σε λίστες των δύο στοιχείων,τις οποίες και στη συνέχεια ενώνει σε μια καινούργια λίστα
breakInTwos [] = []
breakInTwos (l:ls) = filter (\x->length x==2) (subsequences l) ++ breakInTwos ls

subs :: Int->[[Int]]->[Int]
--Δέχεται ένα στοιχείο και μια λίστα που αποτελείται από λίστες 2 στοιχείων,και φτιάχνει μια καινούργια λίστα,με όλα τα στοιχεία-ουρές από τις 
--λίστες 2 στοιχείων στις οποίες το δοθέν στοιχείο είναι κεφαλή
subs el [] = []
subs el ([e1,e2]:ls)
					|(el==e1)= [e2] ++ subs el ls
					|otherwise = [] ++ subs el ls


prevs :: Int->[[Int]]->[Int]
--Δέχεται ένα στοιχείο και μια λίστα που αποτελείται από λίστες 2 στοιχείων και φτιάχνει μια καινούργια λίστα με όλα τα στοιχεία από τις λίστες 
--αυτές που προηγούνται αυτού του στοιχείου
prevs el [] = []
prevs el ([e1,e2]:ls)
					|(el==e2)= [e1] ++ prevs el ls
					|otherwise = [] ++ prevs el ls



howManyDups :: Int->Int->[Int]->Int
--Δέχεται ένα στοιχείο και μια λίστα κι επιστρέφει τον αριθμό των διπλοτύπων του στοιχείου αυτού σε μια λίστα
--Αν το αποτέλεσμα ειναι 0,τότε το στοιχείο είτε δεν υπάρχει,είτε δεν έχει διπλότυπα
howManyDups x counter [] 
							|(counter == 1) = 0 --Το στοιχείο έχει μόνο μία εμφάνιση,άρα καθόλου διπλότυπα
							|(counter==0) = -1 --Το στοιχείο δεν υπάρχει στη λίστα
							|otherwise = counter
howManyDups x counter (l:ls)
								|(x==l) = howManyDups x (counter+1) ls
								|otherwise = howManyDups x counter ls

eraseTheRare ::[Int]->[Int]
--Δέχεται μια λίστα και διαγράφει από αυτήν όλα τα στοιχεία που έχουν λιγότερες από 3 εμφανίσεις (ουσιαστικά φτιάχνει μια καινούργια,με όλα
--τα στοιχεία που έχουν τουλάχιστον 3 εμφανίσεις στην αρχική λίστα)
eraseTheRare [] = []
eraseTheRare (x:xs) 
					|((howManyDups x 0 (x:xs))>=3)= [x] ++ eraseTheRare xs
					|otherwise = [] ++eraseTheRare xs 



makeTHEList :: [[Int]]->[([Int],Int)]
--Δέχεται την αρχική λίστα και φτιάχνει μια καινούργια,η οποία απότελείται από ζεύγη λίστα-στοιχείο,με το στοιχείο να είναι ένα στοιχείο της λίστας
--που ψάχνουμε και τη λίστα να περιέχει τα στοιχεία που προηγούνται αυτού
makeTHEList ls = process (head ls) ls


process :: [Int]->[[Int]]->[([Int],Int)]
--Ουσιαστικά αυτή κάνει τη δουλειά και παραπάνω συνάρτηση επιστρέφει το αποτέλεσμα ^^ .Ο διασωρισμός έγινε για έίναι πιο "κομψό" για το χρήστη
--καθώς χρειζόμαστε τα στοιχεία της λίστας για τη διάσχιση και την κατασκευή,και αν δεν γινοταν ο διαχωρισμός,ο χρήστης θα έπρεπε να δώσει το
--ίδιο όρισμα 2 φορές 
process [] ls = []
process (x:xs) ls = [(eraseDuplicates((eraseTheRare (prevs x (breakInTwos ls)))),x)] ++ process xs ls


findThePositions :: [([Int],Int)]->[(Int,Int)]
--Δέχεται ως όρισμα τη λίστα που επιστρέφει η makeTHEList (ok,process),και βρίσκει για κάθε στοιχείο τη θέση του μέσα στη ζητούμενη από την άσκηση 
--λίστα με βάση πόσα στοιχεία προηγούνται αυτού (το ζεύγος είναι στοιχείο-θέση )
findThePositions [] = []
findThePositions ((ls,el):xs) = [(el,((length ls)+1))] ++ findThePositions xs

findTheNthElement :: [(Int,Int)]->Int->Int
--Δέχεται τη λίστα με τα ζεύγη που αντιστοιχούν στο στοιχείο και τη θέση του στην τελική λίστα,καθώς και μια θέση,κι επιστρέφει το στοιχείο 
--που βρίσκεται σε αυτή τη θέση 
findTheNthElement [] nth = -1 --Αν έχεις φθάσει μέχρι εδώ σημαίνει ότι ζήτησες θέση που δεν υπάρχει,δλδ η λίστα που ζητάς είναι μεγαλύτερη
findTheNthElement ((el,pos):xs) nth
									|(nth==pos) = el
									|otherwise = findTheNthElement xs nth 

makeTheInitialList :: [(Int,Int)]->Int->Int->[Int]
--Δέχεται τη λίστα με τα ζευγάρια στοιχείο,έναν ακέραιο που χρησιμοποιείται για να βρούμε ποια θέση ζητάμε,και το μήκος της τελικής λίστας 
--και φτιάχνει τη ζητούμενη λίστα,εισάγοντας σε κάθε θέση το σωστό στοιχείο
makeTheInitialList l1 l2 0 = []
makeTheInitialList l1 nth len = [findTheNthElement l1 (nth+1) ] ++ makeTheInitialList l1 (nth+1) (len-1)


findlist :: [[Int]]->[Int]
findlist ls = makeTheInitialList (findThePositions (makeTHEList ls)) 0 (length (head ls))

----------------------------------------------------------------------------------------------------------------------

--Question 4

findWhatSickAte :: [(Int,Int,Int)] -> [(Int,Int)] -> [Int]
--Δημιουργεί μια λίστα με ό,τι φαγητό έφαγε κάποιος πριν αρρωστήσει
findWhatSickAte [] []=[]
findWhatSickAte xs ys = eraseDuplicates ([z2 |(z1,z2,z3)<-xs,(w1,w2)<-ys,(z1==w1),(z3<=w2)])

eraseDuplicates :: [Int]->[Int]
--Διαγράφει τα διπλότυπα σε μια λίστα
eraseDuplicates []=[]
eraseDuplicates (x:xs) 
						| x `elem` xs = eraseDuplicates xs
						|otherwise = [x] ++ eraseDuplicates xs


whoAteSuspiciousFood :: [(Int,Int,Int)]->[Int]->[Int]
--Βρίσκει ποιος από την αρχική λίστα έφαγε κάποιο ύποπτο φαγητό
whoAteSuspiciousFood [] [] =[]
whoAteSuspiciousFood xs ys = eraseDuplicates ([ x1|(x1,x2,x3)<-xs,y1<-ys,(x2==y1)])

whoGotSick ::  [(Int,Int,Int)] -> [(Int,Int)] -> [Int]
--Βρίσκει ποιος αρρωστησε
whoGotSick [] [] = []
whoGotSick xs ys = eraseDuplicates ([ x1 | (x1,x2,x3)<-xs,(y1,y2)<-ys,(x1==y1)])

potentiallySick :: [Int]->[Int]->[Int]
--Βρίσκει,από αυτούς που έφαγαν ύποπτο φαγητό ποιοι αρρώστησαν,και φτιάχνει μια νέα λίστα με όσους έφαγαν ύποπτο φαγητό και δεν αρρώστησαν
potentiallySick [] ys =[]
potentiallySick (x:xs) ys 
							| x `elem` ys = potentiallySick xs ys
							|otherwise = [x] ++potentiallySick xs ys


doses::Int->Int-> [(Int,Int,Int)] -> [(Int,Int)]->Int

doses m n ls ws = length (whoAteSuspiciousFood ls (findWhatSickAte ls ws ))

--------------------------------------------------------------------------------------------------------------------------------------

--Question 5

{-Για την επίλυση του προβλήματος σχηματίζουμε μια λίστα στην οποία αντιστοιχίζουμε την ταχύτητα του Γιώργου στο "τεμάχισμα" της διαδρομής βάσει
των επιτρεπτών ταχυτήτων.Δηλαδή για το παράδειγμα,η λίστα που θέλουμε αν φτιάξουμε είναι η εξής: [(40,76),(50,40),(10,40)].Η συνάρτηση 
makeTheList φτιάχνει την εν λόγω λίστα.Προκειμένου να το επιτύχει αυτό,κανει χρήση 2 βοηθητικών συναρτήσεων,
της getGsSpeed και της findWhereToGoOnFrom.Η πρώτη,όταν κάποιο κομμάτι διαδρομής είναι μεγαλύτερο από κάποιο του Γιώργου,πχ [...,(50,35),...] 
[...,(20,30),(40,40)] βρίσκει γι'αυτό το κομμάτι διαδρομής -εδω τα 50 χλμ- την μέγιστη ταχύτητα του Γιώργου -εδώ 40 χλμ/ώρα.
Η δεύτερη βρίσκει πάλι στην ίδια περίπτωση από πού θα πρέπει να συνεχίσει τη δουλειά της η makeTheList-εν προκειμένω από το [...,(10,40)] -}

makeTheList :: [(Int,Int)]->[(Int,Int)]->[(Int,Int)]
--Η συνάρτηση αυτή φτιάχνει τη λίστα που αντιστοιχεί τις ταχύτητες του Γιώργου στο τεμάχισμα της διαδρομής
makeTheList [] gs = []
makeTheList ((ad,asp):as) ((gd,gsp):gs) 
										|(ad==gd)= [(gd,gsp)] ++ makeTheList as gs
										|(ad<gd)= [(ad,gsp)] ++ makeTheList as (((gd-ad),gsp):gs) 
										|(ad>gd)= [(ad,maximum (getGsSpeed (ad,asp) ((gd,gsp):gs)))] ++ makeTheList as (findWhereToGoOnFrom (ad,asp) ((gd,gsp):gs))
{-
Οι 2 πρώτες περιπτώσεις είναι σχετικά εύκολες,στην πρώτη η διαδρομή του Γιώργου είναι ίδια με την επιτρεπτή (τα χιλιόμετρα δηλαδή)
και στη 2η τα χιλιόμετρα της επιτρεπ΄της διαδρομής είναι περισσότερα από αυτά που έκανε ο Γιώργος με κάποια συγκεκριμένη ταχύτητα.Η "δύσκολη"
περίπτωση είναι όταν έχουμε ένα τεμάχισμα του στυλ (50,30)<- επιτρεπτή και (20,40)<- του Γιώργου,οπότε μετά και πρέπει να προχωρήσουμε να εξετάσουμε
κι άλλα tuples της λίστας του Γιώργου,και τελικά να κρατήσουμε την υψηλότερη ταχύτητα.Για την περίπτωση αυτή καλούνται 2 βοηθητικές συναρτήσεις,η  
getGsSpeed που επιστρέφει όλες τις ταχύτητες του Γιώργου που αντιστοιχούν στο συγκεκριμένο κομμάτι επιτρεπτής διαδρομής,και μετά η findWhereToGoOnFrom,
η οποία μάς δείχνει από ποιο tuple του Γιώργου θα συνεχίσουμε -και αν χρειαστεί να υποστεί αυτό το tuple κάποια αλλαγή πχ αν αφαιρέθηκε απόσταση που 
"χρησιμοποιήθηκε",η συνάρτηση την κάνει.
-}

getGsSpeed :: (Int,Int)->[(Int,Int)]->[Int]

getGsSpeed (ad,asp) ((gd,gsp):gs) 
									|(ad>gd) = [gsp] ++ getGsSpeed ((ad-gd),asp) gs
									|(ad<=gd)= [gsp]


findWhereToGoOnFrom :: (Int,Int)->[(Int,Int)]->[(Int,Int)]

findWhereToGoOnFrom (ad,asp) ((gd,gsp):gs) 
											|(ad>gd)= findWhereToGoOnFrom ((ad-gd),asp) gs
											|(ad==gd) = gs
											|(ad<gd) =(((gd-ad),gsp):gs)


findTheDifs :: [(Int,Int)]->[(Int,Int)]->[Int]
--Φτιάχνει τη λίστα με τη διαφορές μεταξύ της ταχύτητας του Γιώργου και της επιτρεπτής για κάθε κομμάτι διαδρομής.Αν η διαφορά είναι αρνητική
--σημαίνει ότι ο Γιώργος πήγαινε πιο σιγά από το όριο
findTheDifs [] []= []
findTheDifs er gr = [ (gsp-asp) | (ad,asp)<- er,(gd,gsp)<- gr, (ad==gd) ]


maxdiff :: [(Int,Int)]->[(Int,Int)]->Int

maxdiff ls ms = maximum (findTheDifs ls (makeTheList ls ms))

-----------------------------------------------------------------------------------------------------------------------------------------------------
























