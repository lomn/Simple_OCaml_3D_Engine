import os

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
	
        file_out = open(filename + ".custom", 'w')
	file_out.writelines(result)
	file_out.close()
	return

if __name__ == "__main__":
    in_stl = raw_input("Enter the source stl file : ")
    if not os.path.exists(in_stl):
        print("Path for the stl file invalid")
    else:
        stl_to_ascii(in_stl)


