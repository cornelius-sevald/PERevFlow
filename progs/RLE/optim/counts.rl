(counts text) -> (text) with (c n temp)
// specialized w.r.t. chars

init_1:
	entry
	assert(!(text))
	(n . counts) <- counts
	if text
		goto boolT_1_21
		else boolF_1_21

boolT_1_21:
	fi (hd(text) = 'd)
		from inner_1_21
		else init_1
	if (n = '1)
		goto outer_1_21
		else inner_1_21

outer_1_21:
	fi text
		from boolT_1_21
		else boolF_1_21
	'1 <- n
	c <- 'd
	text <- (c . text)
	(n . counts) <- counts
	if text
		goto boolT_1_19
		else boolF_1_19

boolT_1_19:
	fi (hd(text) = 'c)
		from inner_1_19
		else outer_1_21
	if (n = '1)
		goto outer_1_19
		else inner_1_19

outer_1_19:
	fi text
		from boolT_1_19
		else boolF_1_19
	'1 <- n
	c <- 'c
	text <- (c . text)
	(n . counts) <- counts
	if text
		goto boolT_1_17
		else boolF_1_17

boolT_1_17:
	fi (hd(text) = 'b)
		from inner_1_17
		else outer_1_19
	if (n = '1)
		goto outer_1_17
		else inner_1_17

outer_1_17:
	fi text
		from boolT_1_17
		else boolF_1_17
	'1 <- n
	c <- 'b
	text <- (c . text)
	(n . counts) <- counts
	if text
		goto boolT_1_15
		else boolF_1_15

boolT_1_15:
	fi (hd(text) = 'a)
		from inner_1_15
		else outer_1_17
	if (n = '1)
		goto outer_1_15
		else inner_1_15

outer_1_15:
	fi text
		from boolT_1_15
		else boolF_1_15
	'1 <- n
	c <- 'a
	text <- (c . text)
	exit

inner_1_15:
	fi text
		from boolT_1_15
		else boolF_1_15
	temp <- 'a
	text <- (temp . text)
	n -= '1
	goto boolT_1_15

boolF_1_15:
	from outer_1_17
	if (n = '1)
		goto outer_1_15
		else inner_1_15

inner_1_17:
	fi text
		from boolT_1_17
		else boolF_1_17
	temp <- 'b
	text <- (temp . text)
	n -= '1
	goto boolT_1_17

boolF_1_17:
	from outer_1_19
	if (n = '1)
		goto outer_1_17
		else inner_1_17

inner_1_19:
	fi text
		from boolT_1_19
		else boolF_1_19
	temp <- 'c
	text <- (temp . text)
	n -= '1
	goto boolT_1_19

boolF_1_19:
	from outer_1_21
	if (n = '1)
		goto outer_1_19
		else inner_1_19

inner_1_21:
	fi text
		from boolT_1_21
		else boolF_1_21
	temp <- 'd
	text <- (temp . text)
	n -= '1
	goto boolT_1_21

boolF_1_21:
	from init_1
	if (n = '1)
		goto outer_1_21
		else inner_1_21
