doubleMe x = x + x

-- doubleUs x y = x * 2 + y * 2

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                          then x
                          else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

conanO'Brien = "It's me, Conan O'Brien!"

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs , odd x ]

removeNonUppercase st = [ c | c <- st , c `elem` [ 'A' .. 'Z' ]]

let xxs = [[1,3,5,2,3,1,2,4,5], [1,2,3,4,5,6,7,8,9], [1,2,4,2,1,6,3,1,3,2,3,6]]
-- [[x | x <- xs, even x] | xs <- xxs]









