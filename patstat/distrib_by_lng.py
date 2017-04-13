
##########
import os, os.path, codecs

main_path = 'C:/work/others/Patstat/!1/'
processed_path = 'C:/work/others/Patstat/!1/processed/'
d = dict()
dest = dict()

cntr = 0
max_cntr = 10000

for fname in (f for f in os.listdir(main_path) if os.path.isfile(os.path.join(main_path, f)) and f.endswith('txt')):
  with open(os.path.join(main_path, fname), encoding="utf-8") as src:
    for l in src:
      st = l.find(',"')
      if st == -1:
        continue
      out_file_name = os.path.splitext(fname)[0] + '_' + l[st+2: st+4] + '.txt'
      if not out_file_name in d:
        d[out_file_name] = list()
        dest[out_file_name] = open(os.path.join(processed_path, out_file_name), 'w', encoding="utf-16")
        #dest[out_file_name].write(codecs.BOM_UTF16_LE)
      d[out_file_name].append(l)

      cntr = cntr + 1
      if cntr > max_cntr:
        cntr = 0

        for out_file_name in d:
          for l in d[out_file_name]:
            dest[out_file_name].write(l)
          d[out_file_name] = list()
          w.close()

    for out_file_name in d:
      for l in d[out_file_name]:
        dest[out_file_name].write(l)
      d[out_file_name] = list()

for out_file_name in dest:
  dest[out_file_name].close()