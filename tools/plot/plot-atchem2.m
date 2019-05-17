% -----------------------------------------------------------------------------
%
% Copyright (c) 2017 Sam Cox, Roberto Sommariva
%
% This file is part of the AtChem2 software package.
%
% This file is covered by the MIT license which can be found in the file
% LICENSE.md at the top level of the AtChem2 distribution.
%
% -----------------------------------------------------------------------------

%% Plotting tool for the AtChem2 model output
%% --> GNU Octave/MATLAB version
%%
%% ARGUMENT:
%% - directory with the model output
%%
%% USAGE:
%%   octave ./tools/plot/plot-atchem2.m ./model/output/
%% ---------------------------------------------- %%
arg_list = argv();
cd(arg_list{1});
pwd

fin = fopen('speciesConcentrations.output','r');
var1 = strsplit(fgetl(fin))(2:end);
fclose(fin);
fin = fopen('environmentVariables.output','r');
var2 = strsplit(fgetl(fin))(2:end);
fclose(fin);
fin = fopen('photolysisRates.output','r');
var3 = strsplit(fgetl(fin))(2:end);
fclose(fin);
fin = fopen('photolysisRatesParameters.output','r');
var4 = strsplit(fgetl(fin))(2:end);
fclose(fin);

df1 = dlmread ('speciesConcentrations.output', '', 1, 0);
df2 = dlmread ('environmentVariables.output', '', 1, 0);
df3 = dlmread ('photolysisRates.output', '', 1, 0);
df4 = dlmread ('photolysisRatesParameters.output', '', 1, 0);

nc1 = size(df1)(2);
nc2 = size(df2)(2);
nc3 = size(df3)(2);
nc4 = size(df4)(2);

%% ---------------------------- %%

%% speciesConcentrations.output
j = 1;
for i = 2:1:nc1
  subplot(3,2,j)
  plot(df1(:,1), df1(:,i), '-k')
  xlabel('seconds'), ylabel('')
  if (j == 6)
    j = 1;
  else
    j = j + 1;
  end
end

%% environmentVariables.output
j = 1;
for i = 2:1:nc2
  subplot(3,2,j)
  plot(df2(:,1), df2(:,i), '-k')
  xlabel('seconds'), ylabel('')
  if (j == 6)
    j = 1;
  else
    j = j + 1;
  end
end

%% photolysisRates.output
j = 1;
for i = 2:1:nc3
  subplot(3,2,j)
  plot(df3(:,1), df3(:,i), '-k')
  xlabel('seconds'), ylabel('')
  if (j == 6)
    j = 1;
  else
    j = j + 1;
  end
end

%% photolysisRatesParameters.output
j = 1;
for i = 2:1:nc4
  subplot(3,2,j)
  plot(df4(:,1), df4(:,i), '-k')
  xlabel('seconds'), ylabel('')
  if (j == 6)
    j = 1;
  else
    j = j + 1;
  end
end

%% ---------------------------- %%

%fprintf(['\n===> atchem2_output.pdf created in directory: ', arg_list{1}, '\n\n'])
