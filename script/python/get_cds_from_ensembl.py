import requests, sys
from sys import argv
import regex

id_file = argv[1]
ids = []
for line in open(id_file):
    components = line.strip().split(',')
    ids.append(components)

for gene in range(0, len(ids)):
  gene_id = ids[gene][0]
  species = ids[gene][1]
  server = "http://rest.ensembl.org"
  ext = "/sequence/id/{0}?type=cds".format(gene_id)
  r = requests.get(server+ext, headers={ "Content-Type" : "text/x-fasta"})
  if not r.ok:
    r.raise_for_status()
    sys.exit()
  reg_pat = regex.compile(r'>.*\n')
  new_seq = regex.sub(reg_pat, ">{}\n".format(species), r.text)
  print(new_seq)
