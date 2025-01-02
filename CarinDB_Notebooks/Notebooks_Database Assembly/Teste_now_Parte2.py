import glob
import gzip
import xml.etree.ElementTree as ET
import csv

output_file_name = "table2_Grupo1.csv"
fields = ["db_align_beg","db_align_end", "pdbx_PDB_id_code", "pdbx_auth_seq_align_beg", 
"pdbx_auth_seq_align_end", "pdbx_db_accession", "pdbx_strand_id", "seq_align_beg", "seq_align_end" ]
list_of_rows = [fields]
separator = ";"
i = 1;
ns = {"": "http://pdbml.pdb.org/schema/pdbx-v50.xsd"}


for file_name in glob.glob('F:\BasesAuxiliares\XML_PDB_Baixados\Grupo1_Processado\*.xml'):
   print(i, ' PDB: ', file_name)
   tree = ET.parse(file_name)
   root = tree.getroot()
   for struct_ref_seq in root.findall(".//struct_ref_seq", ns):
      list_of_rows.append([struct_ref_seq.find(field, ns).text for field in fields])
   i = i + 1
   
    
with open(output_file_name, 'w',newline='') as file:
    csv.writer(file, delimiter = separator).writerows(list_of_rows)




