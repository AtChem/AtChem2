The `mcm/` directory contains:

- a minimal `.fac` file with the required sections headers: `mechanism_skel.fac`

- a minimal `.kpp` file with the required sections headers: `mechanism_skel.kpp`

- the complete lists of organic peroxy radicals (RO2) in the Master Chemical
  Mechanism v3.1, v3.2 and v3.3.1: `peroxy-radicals_v*`

- the parameters for the calculation of the photolysis rates in the Master
  Chemical Mechanism v3.1, v3.2 and v3.3.1: `photolysis-rates_v*`

---

**How to change the MCM version**

The default version of the Master Chemical Mechanism (MCM) is **v3.3.1**. This
means that AtChem2 uses the list of RO2 in the file `peroxy-radicals_v3.3.1`
and the photolysis rates parameters in the file `photolysis-rates_v3.3.1`.

The filenames are hard-coded in the AtChem2 source code. This will change in future
versions of AtChem2, see [issue #297](https://github.com/AtChem/AtChem2/issues/297).
For now, to use previous versions of the MCM change the corresponding lines in
`build/mech_converter.py` (for the peroxy radicals) and `src/inputFunctions.f90`
(for the photolysis rates).
