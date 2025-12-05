# 🌐 Network Analysis Project (Análise de Redes)

**Course:** Network Analysis (Análise de Redes)  
**Institution:** ISCTE - Instituto Universitário de Lisboa  
**Program:** BSc in Data Science  
**Academic Year:** 2025/2026


## 📖 Project Overview

This repository contains the source code and documentation for a group project developed to explore social network structures and synthetic network models using **R** and the `igraph` library.

The project is divided into two main analytical components:

### 🎉 Part I: Real-World Network Analysis (The "Party" Dataset)
Analysis of an undirected network representing social interactions between people at a party. The goal is to interpret structural roles and community dynamics based on the edge list provided in `trab.txt`.
* **Key Metrics:** Density, Degree Distribution, Heterogeneity, and Path Lengths.
* **Advanced Analysis:** Clustering Coefficients, k-core Decomposition (shell analysis), and identification of "hubs" or key influencers in the social context.

### ✨Part II: Synthetic Network Simulation
Generation and stress-testing of synthetic networks to understand topological properties.
1.  **Preferential Attachment Model:** Generating a scale-free network (500 nodes, ~1000 edges) using the Barabási-Albert model.
2.  **Robustness Testing:** Simulating network degradation by removing edges with a probability of $p=0.1$ and comparing the structural changes (connectivity, diameter, and clustering) against the original model.

## 📂 Repository Structure

| File | Description |
| :--- | :--- |
| `R_Code` | Main R script containing the `igraph` implementation for both Part I and Part II. |
| `trab.txt` | The edge list for Part I |
| `Relatório de Redes.pdf` | Final report detailing methodology, results, and social interpretations (In Portuguese from Portugal). |
| `Enunciado...pdf` | Original assignment guidelines and requirements. |

## 🛠 Technologies Used

The project is built entirely within the **R ecosystem**:

![R](https://img.shields.io/badge/R-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
![RStudio](https://img.shields.io/badge/RStudio-75AADB?style=for-the-badge&logo=RStudio&logoColor=white)
![igraph](https://img.shields.io/badge/igraph-Package-orange?style=for-the-badge)
![Markdown](https://img.shields.io/badge/markdown-%23000000.svg?style=for-the-badge&logo=markdown&logoColor=white)

## 👥 Authors

* **Daniel Fonseca** @ Daniel99Fonseca
* **Francisco Gonçalves** @ FZizzy
* **João Filipe** @ Tigerman-dev
* **Guilherme Pires** @ GuiPrs335
