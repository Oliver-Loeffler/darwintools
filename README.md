# Darwintools

Collection of functions for import of data from different pattern & blank inspection tools used in photomask fabrication.

## License

Copyright (C) 2019  Pavel Nesladek

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. 

You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

## Description

Collection of functions for import of data from different pattern & blank inspection tools  e.g. KLA, NuFlare, Lasertec, Siemens DFx (ascii format), KLA (XML), as well as Verity .svd spectrometer data and N&K reflectometer data. Calculation of particle density from defect data, plot and co-plot of defect maps, defect subtraction and cluster estimation, calculation of defect density, smoothing, etc.

## Requirements

* R >= 2.11 with XML and fields libraries installed

## How to build

```cmd

R CMD INSTALL --build --no-multiarch --force darwintools

```

## Release notes

### 0.6.9

* Patched recently discovered bug in readLT() function (thanks JIrka), causing defect size estimation based on raw or DISE in X-direction, in some cases ignoring the DISE measurement. Current version of the readLT() is estimating the defect size either as maximum from the both X- and Y- sizes in DISE, or X direction from raw part, if DISE not available.

* All Verity spectrometer related function were transferred from darwintools to emma, to simplify the package maintenance and avoid very frequent updates of darwintools, as the Tetra related functions are currently changing very frequently.

### 0.6.8

* First public release.
