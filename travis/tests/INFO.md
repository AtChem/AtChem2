The directory `travis/tests/` contains the **behaviour tests** for
AtChem2. These tests check the overall behaviour of the model for a
range of possibile configurations.

- `firstorder`, `secondorder`: generic chemical mechanism (1 reaction, 2 species)

- `static`: generic chemical mechanism (2 reactions, 2 species)

- `short*`: chemical mechanism of CH3OH, C2H5OH, BUT2OL (324 reactions, 105 species), no constraints
  - `short`: change values of TEMP, , H2O, DEC. Set ROOF=CLOSED. Two species initialized, one species output , invert order of outputRates. Start time = 19/11/08 12:00, runtime= 15 min. Change latitude and longitude. output reaction rates and jacobian every 300 seconds.
  - `short_dense`: Solver Type = Dense.
  - `short_end_of_day`: start time = 31/12/08 23:30, runtime = 50 min.
  - `short_ext1`: change values of TEMP, PRESS, H2O, latitude, longitude, day, month, yeat. Set DEC=CALC, ROOF=OPEN. Five species initialized, five species output, invert order of outputRates. Don't output jacobian.
  - `short_ext2`: Set RH, DEC and calculate H2O.
  - `short_ext3`: Set RH, DEC and calculate H2O, change month.
  - `short_ext4`: Change H2O and month.
  - `short_no_pre`: Solver Type = SPGMR.

- `spec*`: chemical mechanism of CH4 (71 reactions, 30 species), with constraints.
  - `spec_no_env_yes1`: Temp and Press set, BLHEIGHT constrained
  - `spec_no_env_yes2`: Temp and Press constrained, BLHEIGHT not used
  - `spec_yes_env_no`: change runtime and starttime and output times. change rtol and atol parameters, two species constrained.
  - `spec_yes_env_no_with_jfac`: constrained to J$, calculate JFAC from J4. three species constrained.
  - `spec_yes_env_no_with_jfac_fail1`: same but J4 file is missing
  - `spec_yes_env_no_with_jfac_fixed`: JFAC set to 0.9, not constrained to J4.
  - `spec_yes_env_no_with_photo`: JFAC not used, constrained to two species. nchange output reaction rates
  - `spec_yes_env_yes`: BLHEIGHT and two species constrained, jacobian output. change runtime and starttime and output times. Change rtol.
  - `spec_yes_plus_fixed_env_no`: same as spec_yes_env_no, but different solver and model parameters. Two species set to constant and two constrained.

- `full`: complete MCM (17224 reactions, 5833 species), no constraints. Temp, H2O, DEC and DILUTE set to constant values. One species initialized. Change3d atol and rtol, and all model parameters.
