The directory `travis/tests/` contains the **behaviour tests** for
AtChem2. These tests check the overall behaviour of the model for a
range of possibile configurations.

- `firstorder`, `secondorder`, `static`: generic chemical mechanism (1 or 2 reaction, 2 species). These are manufactured ODE systems with known solutions.

- `short*`: chemical mechanism of CH3OH, C2H5OH, BUT2OL (324 reactions, 105 species), no constraints:
  - `short`: change values of TEMP, H2O, DEC. Set ROOF to CLOSED. Two species initialized, one species output, inverted order of outputRates. Start time = 19/11/08 12:00, runtime= 15 min. Change latitude and longitude. Change output reaction rates and jacobian to 300 seconds.
  - `short_dense`: set Solver Type = Dense.
  - `short_end_of_day`: set start time = 31/12/08 23:30, runtime = 50 min.
  - `short_ext1`: change values of TEMP, PRESS, H2O, latitude, longitude, day, month, yeat. Set DEC=CALC, ROOF=OPEN. Five species initialized, five species output. Don't output jacobian.
  - `short_ext2`: set RH, DEC and calculate H2O.
  - `short_ext3`: set RH, DEC and calculate H2O, change month.
  - `short_ext4`: Change H2O and month.
  - `short_no_pre`: set Solver Type = SPGMR.

- `spec*`: chemical mechanism of CH4 (71 reactions, 30 species), with constraints:
  - `spec_no_env_yes1`: set TEMP, PRESS, BLHEIGHT constrained.
  - `spec_no_env_yes2`: TEMP and PRESS constrained, BLHEIGHT not used.
  - `spec_yes_env_no`: change runtime, start time and output times. Change rtol and atol parameters. Two species constrained.
  - `spec_yes_env_no_with_jfac`: constrained to J4, calculate JFAC from J4. Three species constrained.
  - `spec_yes_env_no_with_jfac_fail1`: same as `spec_yes_env_no_with_jfac` but J4 file is missing.
  - `spec_yes_env_no_with_jfac_fixed`: JFAC set to 0.9, J4 not constrained.
  - `spec_yes_env_no_with_photo`: JFAC not used, constrained to two species. Change output reaction rates.
  - `spec_yes_env_yes`: BLHEIGHT and two species constrained. Output jacobian. Change runtime, start time and output times. Change rtol parameter.
  - `spec_yes_plus_fixed_env_no`: same as `spec_yes_env_no`, but different solver parameters and model parameters. Two species set to constant and two species constrained.

- `full`: complete MCM (17224 reactions, 5833 species), no constraints. Set TEMP, H2O, DEC, DILUTE. One species initialized. Change atol and rtol parameters, and all model parameters.
