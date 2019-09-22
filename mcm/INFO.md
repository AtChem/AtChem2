The `mcm/` directory contains:

- an example chemical mechanism, extracted from the Master Chemical Mechanism v3.3.1: `mechanism_test.fac`

- the complete lists of organic peroxy radicals (RO2) in the Master Chemical Mechanism v3.1, v3.2 and v3.3.1: `peroxy-radicals_v*`.

- the parameters for the calculation of the photolysis rates in the Master Chemical Mechanism v3.1, v3.2 and v3.3.1: `photolysis-rates_v*`.

The default version of the MCM used in AtChem2 is **v3.3.1**. To use the previous versions, change the corresponding lines in `tools/mech_converter.py` (for the peroxy radicals) and `src/inputFunctions.f90` (for the photolysis rates).
