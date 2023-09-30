
<!-- README.md is generated from README.Rmd. Please edit that file -->

# satres

<!-- badges: start -->
<!-- badges: end -->

I teach a GIS course at the University of Granada (Spain). Each edition
of the course the students and I download files with satellite bands
from the [*ESA*](https://dataspace.copernicus.eu/) and
[*USGS*](https://glovis.usgs.gov/) websites to perform data analysis,
transform them or store them in DBMS.

They are downloaded as files in ZIP or TAR format. In some cases, it is
necessary to download several files that cover the study area, so the
first operation to perform is to merge the bands.

Currently we perform this operation using the
[`terra`](https://CRAN.R-project.org/package=terra) package, but it is
quite laborious because we have to decompress and access the different
bands to apply the operations.

The goal of `satres` is to partially automate these operations. We can
use it to decompress all the downloaded files in a single operation. It
is enough to indicate the folder that contains the decompressed files:
Satellite bands are automatically obtained by name and spatial
resolution; if we have several files to cover a geographical area, the
corresponding bands are automatically merged. The result can be stored
on disk and can also be obtained as objects of class `SpatRaster` of
package `terra`.

## Installation

<!-- You can install the released version of `satres` from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("satres") -->
<!-- ``` -->
<!-- And the development version from [GitHub](https://github.com/) with: -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("josesamos/satres")
```

## Example

To cover the area of the municipality of Lanjarón (Granada), I have
downloaded two satellite band files from the *ESA* website.

- `S2A_MSIL2A_20230905T105621_N0509_R094_T30SVF_20230905T170700.zip`:
  0.91 GB
- `S2A_MSIL2A_20230905T105621_N0509_R094_T30SVG_20230905T170700.zip`:
  1.14 GB

I have unzipped them directly into the `datos/esa` folder, creating two
subfolders.
