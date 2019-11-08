The directory `travis/tests/` contains the **behaviour tests** for
AtChem2. These tests check the overall behaviour of the model for a
range of possibile configurations.

- `firstorder`, `secondorder`: generic chemical mechanism (1 reaction, 2 species)

- `static`: generic chemical mechanism (2 reactions, 2 species)

- `short*`: chemical mechanism of CH3OH, C2H5OH, BUT2OL (324 reactions, 105 species), no constraints

- `spec*`: chemical mechanism of CH4 (71 reactions, 30 species), with constraints.

  - `spec_no*`:

  - `spec_yes_env_no*`:

  - `spec_yes_env_yes*`:

- `full`: complete MCM (17224 reactions, 5833 species), no constraints.
