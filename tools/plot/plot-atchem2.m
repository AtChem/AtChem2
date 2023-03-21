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
%% --> version for GNU Octave/MATLAB
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

df1 = readtable('speciesConcentrations.output');
df2 = readtable('environmentVariables.output');
df3 = readtable('photolysisRates.output');
df4 = readtable('photolysisRatesParameters.output');

nc1 = size(df1, 2);
nc2 = size(df2, 2);
nc3 = size(df3, 2);
nc4 = size(df4, 2);

%% ---------------------------- %%

%% speciesConcentrations.output
for i = 2:nc1
    subplot(3,3,i-1)
    plot(df1{:,1}, df1{:,i}, '-k')
    title(df1.Properties.VariableNames{i})
    xlabel('seconds'), ylabel('')
end

%% environmentVariables.output
for i = 2:nc2
    subplot(3,3,i-1)
    plot(df2{:,1}, df2{:,i}, '-k')
    title(df2.Properties.VariableNames{i})
    xlabel('seconds'), ylabel('')
end

%% photolysisRates.output
for i = 2:nc3
    subplot(3,3,i-1)
    plot(df3{:,1}, df3{:,i}, '-k')
    title(df3.Properties.VariableNames{i})
    xlabel('seconds'), ylabel('')
end

%% photolysisRatesParameters.output
for i = 2:nc4
    subplot(3,3,i-1)
    plot(df4{:,1}, df4{:,i}, '-k')
    title(df4.Properties.VariableNames{i})
    xlabel('seconds'), ylabel('')
end

print('atchem2_output.pdf', '-dpdf', '-S1100,700')

%% ---------------------------- %%

fprintf('\n==> atchem2_output.pdf created in directory: %s\n\n', arg_list{1});
