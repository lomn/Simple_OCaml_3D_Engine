def stl_to_ascii(filename):
	file_in = open(filename, 'r')
	lines = file_in.readlines()
	file_in.close()
	result = []
	i = 0
	for k in range(len(lines)):
		if lines[k][0] == 'v':
			result.append(lines[k][7:])
			result[i] = result[i].replace(" ", ",")
			i += 1
	file_out = open("custom" + filename, 'w')
	file_out.writelines(result)
	file_out.close()
	return