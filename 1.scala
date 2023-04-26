val partition: (List[Int], Int) => (List[Int], List[Int]) = {
	case (x, p) => (less(x, p), more(x, p))
	}
	
val less: (List[Int], Int) => List[Int] = {
	case (x :: xs, p) if (x < p) => (x :: less(xs, p))
	case (x :: xs, p) if (x >= p) => less(xs, p)
	case (nil, p) => nil
	}
	
val more: (List[Int], Int) => List[Int] = {
	case (x :: xs, p) if (x < p) => more(xs, p)
	case (x :: xs, p) if (x >= p) => (x :: more(xs, p))
	case (nil, p) => nil
	}
	
partition(List(1, 2, 3, 4), 3)
partition(List(1, 2, 3, 4, 5, 6), 3)
