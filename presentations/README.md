# pep725 Presentations & Media

This folder contains marketing and educational materials for the pep725 package.

## Contents

### Presentations
| File | Description |
|------|-------------|
| `pep725-overview.qmd` | Quarto Revealjs presentation (source) |
| `pep725-overview.html` | Rendered HTML presentation |
| `custom.scss` | Custom styling for presentation |

### Media
| File | Type | Description |
|------|------|-------------|
| `Reading_Nature_s_Calendar.mp4` | Video | Package overview video |
| `Analyzing_Nature's_Calendar_Through_12_Million_Records.m4a` | Audio/Podcast | NotebookLM-generated discussion |
| `Big_Data_Phenology_in_R.pdf` | PDF | Detailed documentation/slides |
| `mindmap_pep725.png` | Image | Package feature mindmap |

## Viewing the Presentation

Open `pep725-overview.html` in any web browser.

**Keyboard shortcuts:**
- `→` / `←` - Navigate slides
- `S` - Open speaker notes
- `F` - Fullscreen
- `O` - Overview mode
- `?` - Help

## Re-rendering the Presentation

```bash
cd presentations
quarto render pep725-overview.qmd
```

## Speaker Notes

Press `S` while presenting to open speaker view with notes and timer.

## Note

This folder is excluded from the R package build (via `.Rbuildignore`) since these are supplementary marketing materials, not package functionality.

## License

Same as the pep725 package (GPL-3).
