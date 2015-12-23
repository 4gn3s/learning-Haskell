deposit value account = account + value
withdraw value account = account - value

eligible :: (Num a, Ord a) => a -> Bool
eligible a =
	let a1 = deposit 100 a in
	 if a1 < 0
	 	then False
   	 else
		let a2 = withdraw 200 a1 in
	  	 if a2 < 0
			then False
   		 else
	  		let a3 = deposit 100 a2 in
		 	 if a3 < 0
				then False
			 else
				let a4 = withdraw 300 a3 in
				 if a4 < 0
					then False
	 			 else
	  				let a5 = deposit 1000 a4 in
		   			 if a5 < 0
						then False
	  				 else
						True

main =
	(print $ eligible 300) >>
	 (print $ eligible 299)
