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
if length(argv) < 1
    error('[!] Please provide the model output directory as an argument.');
else
    output_dir = argv{1};
    cd(output_dir);
end

fin = fopen('speciesConcentrations.output','r');
var1 = strsplit(fgetl(fin), ' ');
df1 = dlmread ('speciesConcentrations.output', '', 1, 0);
fclose(fin);

fin = fopen('environmentVariables.output','r');
var2 = strsplit(fgetl(fin), ' ');
df2 = dlmread ('environmentVariables.output', '', 1, 0);
fclose(fin);

fin = fopen('photolysisRates.output','r');
var3 = strsplit(fgetl(fin), ' ');
df3 = dlmread ('photolysisRates.output', '', 1, 0);
fclose(fin);

fin = fopen('photolysisRatesParameters.output','r');
var4 = strsplit(fgetl(fin), ' ');
df4 = dlmread ('photolysisRatesParameters.output', '', 1, 0);
fclose(fin);

nc1 = size(df1, 2);
nc2 = size(df2, 2);
nc3 = size(df3, 2);
nc4 = size(df4, 2);

%% ---------------------------- %%

pdf_file = 'atchem2_output.pdf';
if exist(pdf_file, 'file') == 2
    delete(pdf_file);
end

figure('PaperSize', [11 7], 'visible', 'off');

%% speciesConcentrations.output
j = 1;
for i = 2:1:nc1
    subplot(3,3,j);
    plot(df1(:,1), df1(:,i), '-k');
    title(var1{i+1}), xlabel('seconds'), ylabel('');
    set(gca, 'YTickLabel', sprintfc('%.1e', get(gca, 'YTick')));
    ytickformat('%.1e');
    if (j == 9 || i == nc1)
        print(pdf_file, '-dpdf', '-fillpage', '-append');
        j = 1; clf;
    else
        j = j + 1;
    end
end

%% environmentVariables.output
j = 1;
for i = 2:1:nc2
    subplot(3,3,j);
    plot(df2(:,1), df2(:,i), '-k');
    title(var2{i+1}), xlabel('seconds'), ylabel('');
    set(gca, 'YTickLabel', sprintfc('%.1e', get(gca, 'YTick')));
    ytickformat('%.1e');
    if (j == 9 || i == nc2)
        print(pdf_file, '-dpdf', '-fillpage', '-append');
        j = 1; clf;
    else
        j = j + 1;
    end
end

%% photolysisRates.output
j = 1;
for i = 2:1:nc3
    subplot(3,3,j);
    plot(df3(:,1), df3(:,i), '-k');
    title(var3{i+1}), xlabel('seconds'), ylabel('');
    set(gca, 'YTickLabel', sprintfc('%.1e', get(gca, 'YTick')));
    ytickformat('%.1e');
    if (j == 9 || i == nc3)
        print(pdf_file, '-dpdf', '-fillpage', '-append');
        j = 1; clf;
    else
        j = j + 1;
    end
end

%% photolysisRatesParameters.output
j = 1;
for i = 2:1:nc4
    subplot(3,3,j);
    plot(df4(:,1), df4(:,i), '-k');
    title(var4{i+1}), xlabel('seconds'), ylabel('');
    set(gca, 'YTickLabel', sprintfc('%.1e', get(gca, 'YTick')));
    ytickformat('%.1e');
    if (j == 9 || i == nc4)
        print(pdf_file, '-dpdf', '-fillpage', '-append');
        j = 1; clf;
    else
        j = j + 1;
    end
end

%% ---------------------------- %%

fprintf('\n==> atchem2_output.pdf created in directory: %s\n\n', output_dir);
