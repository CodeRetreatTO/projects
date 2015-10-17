class Feigs:
	def __init__(self, r):
		self.r = r
		self.list_of_convergence = self.find_points_of_convergence(r)

	def find_points_of_convergence(self, r):
		tmp_list = []
		x = 0.4
		for i in range(0, 100):
			tmp_list.append(x)
			x = foo(x, r)
			#tmp_list.append(foo((i/ 100.0), r))
		return_list = tmp_list[-10:]
		#print return_list
		return return_list

def foo(x, r):
	return r * x * (1 - x)

feig = Feigs(3.2)

feig_list = []

for i in range(320, 350):
	r = i/100.0
	feig_list.append(Feigs(r))
	
test_list = feig_list[-2:]

for val in test_list:
	print val.list_of_convergence
