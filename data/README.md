# Data

The 40dB microdata are not included in this repository. Download them from the
El País website and unzip each file into its own folder under `data/raw/`:

| Folder in `data/raw/` | Source |
|---|---|
| `40dB_preelectoral_I/` | https://ep00.epimg.net/infografias/encuestas40db/2026/11-preelectoral/2026_11_preelectoral_descargables.zip |
| `40dB_tracking_dia02/` | https://ep00.epimg.net/infografias/encuestas40db/2026/11-tracking-electoral/40dB_tracking_dia02.zip |
| `40dB_tracking_dia03/` | https://ep00.epimg.net/infografias/encuestas40db/2026/11-tracking-electoral/40dB_tracking_dia03.zip |
| `40dB_tracking_dia04/` | https://ep00.epimg.net/infografias/encuestas40db/2026/11-tracking-electoral/40dB_tracking_dia04.zip |
| `40dB_tracking_dia05/` | https://ep00.epimg.net/infografias/encuestas40db/2026/11-tracking-electoral/40dB_tracking_dia05.zip |

Each folder should contain the files from the zip directly (`01_Cuestionario_*.pdf`,
`02_Nota_metodologica_*.pdf`, `03_Datos_*.dta`, ...), without the extra subfolder
the zips unpack into. `R/01_build_data.R` reads the `03_Datos_*.dta` file in each folder.

`data/processed/` is created by `Rscript run_all.R` and holds respondent-level data
derived from the raw files; it is not published either.
