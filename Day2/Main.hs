data Policy = Policy {
	mini :: Int,
	maxi :: Int,
	letter :: Char
}

data Password = Password {
	policy :: Policy,
	word :: String
}

passwords :: [Password]
passwords = [
	Password {
		policy = Policy {
			mini = 1,
			maxi = 3,
			letter = 'a'
		},
		word = "abcde"
	},
	Password {
		policy = Policy {
			mini = 1,
			maxi = 3,
			letter = 'b'
		},
		word = "cdefg"
	},
	Password {
		policy = Policy {
			mini = 2,
			maxi = 9,
			letter = 'c'
		},
		word = "ccccccccc"
	}]

letterCount :: String -> Char -> Int
letterCount s c = length $ filter (== c ) s

isPasswordValid :: Password -> Bool
isPasswordValid p = 
	let p' = policy p;
		count = letterCount (word p) (letter p')
	in
		(count >= (mini p')) && (count <= (maxi p'))

main = print . length . filter (== True) $ fmap isPasswordValid passwords