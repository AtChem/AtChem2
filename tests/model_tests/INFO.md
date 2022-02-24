The directory `tests/model_tests/` contains the **model tests** for AtChem2.
These tests check the overall behaviour of the model for a range of possibile configurations.

The `env_model_*` tests check the configuration of the environment variables.
They use a minimal inorganic chemical mechanism with a runtime of 4 hours (10 min timestep),
starting at 2 pm on 02/02/2002.

- `env_model_1`: TEMP=288, PRESS=1010, H2O=1.8e10, RH=NOTUSED, DEC=CALC, JFAC=NOTUSED. No constraints.

- `env_model_2`: TEMP=288, PRESS=1010, H2O=CALC, RH=42, DEC=-0.2976, JFAC=1. No constraints.

- `env_model_3`: same as `env_model_1` but JFAC=0.

- `env_model_4`: same as `env_model_2` but ROOF=CLOSED.
