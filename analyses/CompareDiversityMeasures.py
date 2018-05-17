import math, itertools

def table(labels):
	count = labels.count 
	return [count(item) for item in set(labels)]


def diShannon(tx):
	s = sum(tx)
	tx = [x /float(s) for x in tx]
	return - sum([x*math.log(x) for x in tx])

	
def diSimpson(tx):
	nn = sum(tx)
	return sum([x * (x-1) for x in tx])/ float(nn* (nn-1))
	
out = []

for n in range(14):
	# between 1 and 17 responses
	for resp in range(16):
		lx = itertools.combinations_with_replacement(range(n+2), resp+2)
		for x in lx:
			tx = table(x)

			r = (diShannon(tx),diSimpson(tx))
			if not r in out:
				out.append(r)
				print n,resp,r[0],r[1]
		
