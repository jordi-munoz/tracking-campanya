# Data

The 40dB microdata are not included in this repository. Download them from the
El País website with, from the project root:

```
Rscript R/update_data.R
```

The script downloads every folder listed in `config/fieldwork_blocks.csv` that is
missing, plus any tracking day published since. It unzips each file into its own
folder under `data/raw/`, without the extra subfolder the zips unpack into. New
days are appended to `config/fieldwork_blocks.csv`. Use `--check` to see what it
would do without changing anything.

| Folder in `data/raw/` | Source |
|---|---|
| `40dB_preelectoral_I/` | https://ep00.epimg.net/infografias/encuestas40db/2026/11-preelectoral/2026_11_preelectoral_descargables.zip |
| `40dB_tracking_diaNN/` | https://ep00.epimg.net/infografias/encuestas40db/2026/11-tracking-electoral/40dB_tracking_diaNN.zip (NN = 02, 03, …; day 1 is contained in day 2) |

Each folder holds the files from the zip (`01_Cuestionario_*.pdf`,
`02_Nota_metodologica_*.pdf`, `03_Datos_*.dta`, ...). `R/01_build_data.R` reads
the `03_Datos_*.dta` file in each folder.

`data/processed/` is created by `Rscript run_all.R` and holds respondent-level data
derived from the raw files; it is not published either.
