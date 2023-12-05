loaded_data = load('Ti20Mat.mat');
data_table = loaded_data.Ti20.data;  
overview_table = loaded_data.Ti20.Overview;
tribometer_table = loaded_data.Ti20.Tribometer;
data_cell = table2cell(data_table);
overview_cell = table2cell(overview_table);
tribometer_cell = table2cell(tribometer_table);
filename = "Ti20Mat2.mat";
save(filename)