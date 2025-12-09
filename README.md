# Digital Scams in Australia

## Overview
This repository contains an interactive narrative visualisation examining digital scam activity in Australia between 2021 and 2023. The project integrates Scamwatch data, Australian Bureau of Statistics (ABS) population data, and SEIFA socioeconomic indicators to provide a comprehensive analysis of scam trends, demographic vulnerability, geographic patterns, and reporting behaviour.

The aim of the application is to support policymakers, government agencies, researchers, and informed public users in understanding the scale, distribution, and drivers of digital scams in Australia. The tool combines an author-driven introductory narrative with a reader-driven exploratory dashboard to accommodate both guided interpretation and independent analysis.

---

## Research Objectives
The visualisation addresses three principal research questions:

1. **Which scam types result in the greatest financial losses, and how have these losses evolved over time?**
2. **Do socioeconomic disadvantage and digital inclusion influence scam incidence or reporting?**
3. **Which demographic groups (age and gender) are most affected, and through which contact mechanisms are scams commonly executed?**

These objectives informed the dashboard’s structure, chart selection, and interaction design.

---

## Key Findings
The analysis reveals several significant insights:

- Investment scams are the most financially damaging category, considerably surpassing all others.  
- Total scam losses display a clear upward trend from 2021 to 2023.  
- Individuals aged 65 and older experience the highest per-capita financial losses.  
- Socioeconomic disadvantage (IRSD) shows little relationship with reported scams.  
- Digital inclusion (IEO) exhibits a strong positive relationship (r = 0.89) with scam reporting, indicating that disadvantaged communities may be underreporting rather than experiencing fewer scams.

---

## Dashboard Structure

### 1. Narrative Landing Page
The application opens on a narrative-driven introduction presenting:

- Project summary  
- Key statistics (Total Losses, Total Reports, Data Points)  
- Core insights  
- A contextual overview to prepare the user for exploration  

This page transitions the user from author-driven framing to self-guided analysis.

---

### 2. Interactive Dashboard
The main dashboard is organised into two analytical tabs, supported by persistent global filters (Year range, Metric selection, Gender) and dynamic KPIs.

#### Tab 1: Geographic Analysis
This tab examines spatial and socioeconomic dimensions using:

- Choropleth map of Australia displaying state-level losses or reports  
- Scatter plots showing relationships between:  
  - IRSD (socioeconomic disadvantage) vs metric  
  - IEO (digital inclusion) vs metric  
- Trend comparison line chart (filtered state vs national average)

All components support hover-based details and click-based cross-filtering.

#### Tab 2: Demographic Analysis
This tab analyses demographic vulnerability and scam modalities through:

- Treemap of scam types  
- Grouped bar chart of age and gender trends  
- Sankey diagram mapping contact methods to scam categories  

Selections in any visualisation filter the entire dashboard.

---

## Design Methodology
A structured Five Design-Sheet (FdS) approach guided the development process:

- **Sheet 1:** Brainstormed 16 visualisation concepts  
- **Sheets 2–4:** Developed three fully articulated design alternatives:  
  - Single-page dashboard  
  - Storytelling (scroll-based) design  
  - Tab-structured analytical dashboard  
- **Sheet 5:** Synthesised final design, combining narrative introduction with a scalable exploratory framework  

This method ensured systematic evaluation and justification of design decisions.

---

## Technical Implementation

### Technologies and Libraries
The visualisation was implemented in R using the following packages:

- shiny – application framework  
- plotly – interactive visualisation  
- leaflet – geographic mapping  
- dplyr, tidyr – data manipulation  
- sf, rnaturalearth – spatial data management  
- shinyjs – UI control logic  

### Key Technical Features
- Persistent global filters maintained across tabs  
- Full cross-filtering between all visualisations using shared reactive values  
- Dynamic KPI computation and trend line generation  
- Construction of node/link structures for Sankey diagrams  
- Responsive interaction elements, including hover details and filter state display  

These implementation details ensured a cohesive and coordinated interactive experience.

---

## Data Sources
- Scamwatch (ACCC): Scam statistics dataset  
- Australian Bureau of Statistics (2023): Regional population data  
- SEIFA (2016): Socio-Economic Indexes for Areas (IRSD, IEO)  

All data sources are cited in the accompanying project report.

---

## How to Run the Application

### Prerequisites
- R and RStudio installed  
- Required R packages (`shiny`, `plotly`, `leaflet`, `dplyr`, `tidyr`, `sf`, etc.)

### Execution Steps
1. Clone or download this repository.  
2. Place `Final1.csv` (cleaned dataset) in the project root directory.  
3. Open `app.R` in RStudio.  
4. Select **Run App**.  

The application will launch with the introductory narrative page, followed by the interactive dashboard.

---
