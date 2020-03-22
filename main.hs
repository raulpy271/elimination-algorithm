
nullMatrix m | null m = True | null (m !! 0) = True | otherwise = False

roundToNcasasDecimais f n = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

roundListToNcasasDecimais m n = map (\x -> roundToNcasasDecimais x n) m

showMatrix m = showm m 0 (length m)
  where
    casasdecimais = 1
    showm m i len = do
      putStrLn (show (roundListToNcasasDecimais (m !! i) casasdecimais))
      if i < (len - 1)
        then 
          showm m (i + 1) len
        else return ()


multlist constant list = map (\x -> x * constant) list

powlist constant list = map (\x -> x ^^ constant) list

sumlist [] [] = []
sumlist (x:xs) (y:ys) = 
  if length xs /= length ys then []  
  else (x + y): sumlist xs ys 

sublist a b = sumlist a (multlist (-1) b)

multListofMatrix constant index matrix = 
  insert constant matrix [] index ((length matrix) - 1)
  where 
    insert a m newm l i
      | i < 0 = newm
      | i == l = insert a m ((multlist a (m !! i)) : newm) l (i-1)
      | otherwise = insert a m (m !! i : newm) l (i-1)

sumListofMatrix indexResult index matrix = 
  insert index matrix [] indexResult ((length matrix) - 1)
  where
    insert a m newm l i
      | i < 0 = newm
      | i == l = insert a m ((sumlist (m !! a) (m !! l)):newm) l (i-1)
      | otherwise = insert a m (m !! i:newm) l (i-1)


swiplines matrix indexLine1 indexLine2 =
  insert matrix [] indexLine1 indexLine2 ((length matrix) - 1 )
  where
    insert m newm indexLine1 indexLine2 index
      | index < 0 = newm
      | index == indexLine1 = 
        insert m (m !! indexLine2 :newm) indexLine1 indexLine2 (index-1)
      | index == indexLine2 = 
        insert m (m !! indexLine1 :newm) indexLine1 indexLine2 (index-1)
      | otherwise = insert m (m !! index :newm) indexLine1 indexLine2 (index-1)

changePivo lin col matrix = change lin lin col matrix
  where 
    change pivo lin col matrix
      | lin >= length matrix = [[]] -- coluna esta zerada
      | (matrix !! pivo !! col) /= 0 = matrix 
      | (matrix !! lin !! col) /= 0 = swiplines matrix pivo lin
      | otherwise = change pivo (lin+1) col matrix

linofPivo col matrix = searchpivo (length matrix - 1) col matrix
  where 
    searchpivo lin col matrix
      | lin < 0 = 0
      | matrix !! lin !! col == 1 = lin
      | otherwise = searchpivo (lin - 1) col matrix


zerar linpivo lin col matrix = 
  if matrix !! lin !! col == 0 then matrix else newmatrix'
  where
    newlin = linofPivo col matrix
    a =  (- (matrix !! lin !! col)) / matrix !! linpivo !! col
    newmatrix = sumListofMatrix lin linpivo (multListofMatrix a linpivo matrix)
    newmatrix' = multListofMatrix (a^^(-1)) linpivo newmatrix

iden lin col matrix = 
  multListofMatrix ((matrix !! lin !! col) ^^ (-1)) lin matrix


escalonar matrix = finalmatrix
  where
    newmatrix = escalar 0 1 0 matrix
    finalmatrix = reduzir ((linofPivo (colunas - 2) newmatrix) - 1) (colunas - 2) newmatrix

    linhas = length matrix
    colunas = length (matrix !! 0) --falta verificar se a matrix é correta
    
    escalar pivo lin col matrix =
      if col >= (colunas - 1) then matrix

      else if pivo == (lin - 1)
        then
          if nullMatrix newmatrix 
            then escalar pivo lin (col + 1) matrix
          else

            if lin > (linhas - 1)
              then
                escalar (pivo + 1) (pivo + 2) (col + 1) (iden pivo col newmatrix)

            else if lin >= (linhas - 1)
              then
                escalar (pivo + 1) (pivo + 2) (col + 1) (iden pivo col (newmatrixnull))
            
            else escalar pivo (lin + 1) col newmatrixnull
      
      else if lin > (linhas - 1) 
        then 
          escalar (pivo + 1) (pivo + 2) (col + 1) (iden pivo col matrix)
      
      else if lin >= (linhas - 1) 
        then 
          escalar (pivo + 1) (pivo + 2) (col + 1) (iden pivo col (zerar pivo lin col matrix))

      else escalar pivo (lin + 1) col (zerar pivo lin col matrix)

      where
        newmatrix = changePivo pivo col matrix
        newmatrixnull = zerar pivo lin col newmatrix

    reduzir lin col matrix
      | col < 1 || lin == pivoposition = matrix
      | lin < 0 = reduzir (leftpivoposition - 1) (col - 1) matrix
      | otherwise = reduzir (lin - 1) col (zerar pivoposition lin col matrix)
      where
        pivoposition = linofPivo col matrix
        leftpivoposition = linofPivo (col - 1) matrix

     
main = do
  putStrLn "Matriz original:"
  showMatrix mymatrix
  putStrLn "Matriz escalonada:"
  (showMatrix . escalonar) (mymatrix)
  where
    mymatrix = 
      [[ 0,  0,    0,    0,    0,     0], 
      [  0,  0,    2,    1, (-1),     8], 
      [  0,  0, (-3), (-1),    2, (-11)], 
      [  0,  0,    6,    2, (-4),    22], 
      [  0,  0, (-2),    1,    2,  (-3)]]
