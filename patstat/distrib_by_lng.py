d = dict()

src = open(r'C:\work\others\Patstat\tls202_part01.txt', 'r', encoding="utf-8")
for l in src:
	st = l.find(',"')
	lng = l[st+2: st+4]
	if not lng in d:
		d[lng] = list()
	d[lng].append(l) 

for lng in d:
	w = open(r'C:/work/others/Patstat/tls202_part01_' + lng + '.txt', 'w', encoding="utf-8")
	for l in d[lng]:
		w.write(l)
	w.close()

