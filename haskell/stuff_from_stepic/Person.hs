module Person where

data Person = Person {
	firstName :: String,
	lastName :: String,
	age ::Int
} deriving (Show, Eq)

updateAge :: Int -> Person -> Person
updateAge newAge person = person {age = newAge}

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p2 {lastName = lastName p1}

name :: Person -> String
name (Person {lastName = ln, firstName = fn}) = fn ++ " " ++ ln

abbrFirstName :: Person -> Person
abbrFirstName p@(Person {firstName = (x:xs)}) =
	p {firstName = [x] ++ end}
		where
			end = if xs == [] then "" else "."


john = Person "John" "Smith" 33
xsvier = Person {
	age = 40,
	firstName = "Phideaux",
	lastName = "Xavier"
}