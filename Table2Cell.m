loaded_data = load('Cr20Mat.mat');
data_table = loaded_data.Cr20.data;  
overview_table = loaded_data.Cr20.Overview;
data_cell = table2cell(data_table);
overview_cell = table2cell(overview_table);

filename = "Cr20Mat3.mat";
save(filename)