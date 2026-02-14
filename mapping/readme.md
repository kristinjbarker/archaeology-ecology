\# ArcGIS Pro Project



\## Purpose



ArcGIS Pro is used here to:

\- visualize cleaned and validated spatial data

\- create cartography-heavy maps and layouts

\- export final map figures for reports and presentations



All authoritative data preparation, cleaning, and summarization occurs in R.



\## Folder structure



mapping/

├── projects/

│   └── ArchMap.aprx

├── exports/

│   ├── maps/

│   └── layers/

└── README.md



\### projects/

Storea ArcGIS project configuration (maps, layouts, symbology) and should point to

data in `data/`.





\### exports/

Contains exported map products that may be regenerated at any time.



Tracked in Git:

\- `.aprx` files

\- this README



Ignored in Git:

\- geodatabases (`\*.gdb`)

\- cache folders (ImportLog, Index, Logs)

\- lock files

\- exported map products



\## Notes



If the project is moved or cloned:

\- open the `.aprx`

\- repair broken data sources to point at `data/`

\- save the project



No other setup should be required.

