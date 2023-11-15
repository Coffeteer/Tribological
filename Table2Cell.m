% Load the .mat file (replace 'your_file.mat' with the actual file path)
loaded_data = load('Cr20Mat.mat');

% Extract the table from the loaded data
data_table = loaded_data.Cr20.data;  % Replace 'your_table' with the actual variable name
overview_table = loaded_data.Cr20.Overview;
% Convert the table to a cell array
data_cell = table2cell(data_table);
overview_cell = table2cell(overview_table);

filename = "Cr20Mat2.mat";
save(filename)