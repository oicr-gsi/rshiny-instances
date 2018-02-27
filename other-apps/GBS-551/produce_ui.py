import os
import re
import sys

input_dir = "/home/ubuntu/data/other-apps/GBS-551/data/bwa/binned_count"

chr_L = ["chr1" , "chr2" , "chr3" , "chr4" , "chr5",
         "chr6" , "chr7" , "chr8" , "chr9" , "chr10",
         "chr11" , "chr12" , "chr13" , "chr14" , "chr15",
         "chr16" , "chr17" , "chr18" , "chr19" , "chr20",
         "chr21" , "chr22" , "chrX" , "chrY" , "chrM"]

for subd in os.popen('''ls -1d %s/*''' % (input_dir)):
    subd_r = subd.rstrip()
    basename = subd_r.split("/")[-1]
    
    new_tabset = []
    new_tabset.append('''\t\t\ttabPanel("%s",''' % (basename))
    new_tabset.append('''\t\t\t\tmainPanel(''')
    new_tabset.append('''\t\t\t\t\ttabsetPanel(type = "tabs",''')
    
    for ch in chr_L:
        new_tabset.append('''\t\t\t\t\t\ttabPanel("%s"),''' % (ch))
    
    
