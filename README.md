# CaRinDB: An integrated database of Cancer Mutations and Residue Interaction Networks

CaRinDB is an interactive database designed to streamline cancer mutation research by integrating data from The Cancer Genome Atlas (TCGA) and advanced structural analysis tools, along with advanced effect predictions and molecular features such as Residue Interaction Networks (RINs) derived from Protein Data Bank experimental structures and AlphaFoldDB computational models. Covering 33 distinct cancer types, CaRinDB offers a broad spectrum of insights into cancer mutation dynamics.

This platform allows users to extract, visualize, and interactively explore diverse mutations through an intuitive interface, evaluate their structural impact. 

CaRinDB provides a curated dataset featuring residue connectivity metrics, allele frequencies, references to biological databases, and functional predictions from 22 distinct tools, making it a valuable resource for AI/ML-based research. CaRinDB is well suited for training AI and machine learning models, enabling breakthroughs in understanding the molecular basis of cancer and its clinical implications, such as precision medicine and therapeutic target discovery

Unlike existing tools, CaRinDB facilitates integration of polymorphism data with protein structural data and residue interaction networks, offering precision in mutation analysis.

**Data Sources**: The construction of the databases available in **CaRinDB** involved numerous public data repositories: [National Cancer Institute - GDC Data Portal](https://portal.gdc.cancer.gov/repository), missense mutations were annotated in [ANNOVAR](https://annovar.openbioinformatics.org/en/latest/user-guide/download/), [SnpEFF](https://pcingola.github.io/SnpEff/), [NCBI - National Center for Biotechnology Information](https://www.ncbi.nlm.nih.gov/), [ClinVar](https://www.ncbi.nlm.nih.gov/clinvar/), [Uniprot](https://www.uniprot.org/uploadlists), [PDB - Protein Data Bank](https://www.rcsb.org/). Residue interaction network data was obtained through the [RING](https://ring.biocomputingup.it/submit) program, 3D protein structure predictions were also obtained from [Alphafold](https://alphafold.ebi.ac.uk/), and predictions to verify the pathogenicity of mutations were also obtained from [AlphaMissense](https://alphamissense.hegelab.org/).

**Contact**: The CaRinDB team is available to assist users who want to import their data on demand. If you have some question, feedback, or request: [contact us](https://bioinfo.imd.ufrn.br/CaRinDB/).

## Citation
If you use CaRinDB data or jupyter notebooks, please cite our article:

Daniela Coelho Batista Guedes Pereira, João Vitor Ferreira Cavalcante, Laise Florentino Cavalcanti, Raul Maia Falcão, Jorge Estefano Santana de Souza, Rodrigo Juliani Siqueira Dalmolin, Thaís Gaudencio do Rêgo, Serghei Mangul, Gustavo Antônio de Souza, Patrick Terrematte, João Paulo Matos Santos Lima. **CaRinDB: an integrated database of common cancer mutations and residue interaction network parameters**, **Bioinformatics Advances**, Volume 6, Issue 1, 2026, vbaf313, [https://doi.org/10.1093/bioadv/vbaf313](https://doi.org/10.1093/bioadv/vbaf313)

```bibtex
@article{10.1093/bioadv/vbaf313,
    author = {Guedes Pereira, Daniela Coelho Batista and Ferreira Cavalcante, João Vitor and Cavalcanti, Laise Florentino and Falcão, Raul Maia and Santana de Souza, Jorge Estefano and Dalmolin, Rodrigo Juliani Siqueira and Rêgo, Thaís Gaudencio do and Mangul, Serghei and de Souza, Gustavo Antônio and Terrematte, Patrick and Lima, João Paulo Matos Santos},
    title = {CaRinDB: an integrated database of common cancer mutations and residue interaction network parameters},
    journal = {Bioinformatics Advances},
    volume = {6},
    number = {1},
    pages = {vbaf313},
    year = {2026},
    month = {01},
    issn = {2635-0041},
    doi = {10.1093/bioadv/vbaf313},
    url = {https://doi.org/10.1093/bioadv/vbaf313},
    eprint = {https://academic.oup.com/bioinformaticsadvances/article-pdf/6/1/vbaf313/68211468/vbaf313.pdf},
}
```

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
