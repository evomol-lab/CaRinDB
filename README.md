# CaRinDB: An integrated database of Cancer Mutations and Residue Interaction Networks

CaRinDB is an interactive database designed to streamline cancer mutation research by integrating data from The Cancer Genome Atlas (TCGA) and advanced structural analysis tools, along with advanced effect predictions and molecular features such as Residue Interaction Networks (RINs) derived from Protein Data Bank experimental structures and AlphaFoldDB computational models. Covering 33 distinct cancer types, CaRinDB offers a broad spectrum of insights into cancer mutation dynamics.

This platform allows users to extract, visualize, and interactively explore diverse mutations through an intuitive interface, evaluate their structural impact. 

CaRinDB provides a curated dataset featuring residue connectivity metrics, allele frequencies, references to biological databases, and functional predictions from 22 distinct tools, making it a valuable resource for AI/ML-based research. CaRinDB is well suited for training AI and machine learning models, enabling breakthroughs in understanding the molecular basis of cancer and its clinical implications, such as precision medicine and therapeutic target discovery

Unlike existing tools, CaRinDB facilitates integration of polymorphism data with protein structural data and residue interaction networks, offering precision in mutation analysis.

**Data Sources**: The construction of the databases available in **CaRinDB** involved numerous public data repositories: [National Cancer Institute - GDC Data Portal](https://portal.gdc.cancer.gov/repository), missense mutations were annotated in [ANNOVAR](https://annovar.openbioinformatics.org/en/latest/user-guide/download/), [SnpEFF](https://pcingola.github.io/SnpEff/), [NCBI - National Center for Biotechnology Information](https://www.ncbi.nlm.nih.gov/), [ClinVar](https://www.ncbi.nlm.nih.gov/clinvar/), [Uniprot](https://www.uniprot.org/uploadlists), [PDB - Protein Data Bank](https://www.rcsb.org/). Residue interaction network data was obtained through the [RING](https://ring.biocomputingup.it/submit) program, 3D protein structure predictions were also obtained from [Alphafold](https://alphafold.ebi.ac.uk/), and predictions to verify the pathogenicity of mutations were also obtained from [AlphaMissense](https://alphamissense.hegelab.org/).

**Contact**: The CaRinDB team is available to assist users who want to import their data on demand. If you have some question, feedback, or request: [contact us](https://bioinfo.imd.ufrn.br/CaRinDB/).

## Serving the application

Run a Docker container of CaRinDB on your local machine through the terminal.

1. Get and install docker from: <https://docs.docker.com/get-docker/>.

2. Git clone this repository and go to its directory:

```bash
git clone https://github.com/terrematte/CaRinDB/
cd CaRinDB
```

3. Build the container image and serve the application.

```bash
docker compose up
```
     
4. Access the application at <http://localhost:3839>.
