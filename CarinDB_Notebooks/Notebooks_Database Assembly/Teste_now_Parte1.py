import glob
import os
import gzip
import xml.etree.ElementTree as ET
import csv

output_file_name = "table1_Grupo1.csv"
output_file_name_aux = "table1_Grupo1_aux.csv"
fields = ["PDB","db_name","pdbx_align_begin", "pdbx_db_accession", "pdbx_seq_one_letter_code"]
list_of_rows = [fields]
separator = ";"
i = 1;
ns = {"": "http://pdbml.pdb.org/schema/pdbx-v50.xsd"}

for file_name in glob.glob('F:\BasesAuxiliares\XML_PDB_Baixados\Grupo1_Processado\*.xml'):
   head, tail = os.path.split(file_name) #tail tem o nome do arquivo com extensão
   PDB = tail.split(".")[0]
   print(i, ' PDB: ', PDB)
   tree = ET.parse(file_name) 
   root = tree.getroot()
   for struct_ref in root.findall(".//struct_ref", ns):
      lista = [PDB]
      for field in fields[1:]: 
        val = struct_ref.find(field, ns)
        if val is None:
         lista.append("-")
        else:
         lista.append(struct_ref.find(field, ns).text)
      list_of_rows.append(lista)
   print(list_of_rows)
   i = i + 1

  
      
    
with open(output_file_name, 'w',newline='') as file:
    csv.writer(file, delimiter = separator).writerows(list_of_rows)






#,"pdbx_PDB_id_code"