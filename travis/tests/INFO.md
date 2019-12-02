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
  - `spec_no_env_yes1`:
  - `spec_no_env_yes2`:
  - `spec_yes_env_no`:
  - `spec_yes_env_no_with_jfac`:
  - `spec_yes_env_no_with_jfac_fail1`:
  - `spec_yes_env_no_with_jfac_fixed`:
  - `spec_yes_env_no_with_photo`:
  - `spec_yes_env_yes`:
  - `spec_yes_plus_fixed_env_no`:

- `full`: complete MCM (17224 reactions, 5833 species), no constraints.
