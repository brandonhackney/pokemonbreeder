% Read in your two tables
dex = readcsv('dex.tsv', 'Delimiter', '\t');
groups = readtable('groups.xlsx'); % maybe?

% Assuming that got the header info...
output = false(height(dex), width(groups));
for i = 1:height(dex)
    pokemon = dex.Name{i};
    % Not sure this format works...
    output(1,:) = contains(groups, pokemon);
end

% Then smash them into a 'table' object