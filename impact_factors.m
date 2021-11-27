opts = detectImportOptions('IF-3-1-1.csv', 'ReadRowNames', true);
opts = setvartype(opts,{'D7', 'D8', 'D9', 'D10'},'double');

IF_311 = table2array(readtable('IF-3-1-1.csv', opts));
IF_312 = table2array(readtable('IF-3-1-2.csv', opts));
IF_321 = table2array(readtable('IF-3-2-1.csv', opts));
IF_321_marginal = table2array(readtable('IF-3-2-1_marginal.csv', opts));
IF_322 = table2array(readtable('IF-3-2-2.csv', opts));
IF_41 = table2array(readtable('IF-4-1.csv', opts));
IF_42 = table2array(readtable('IF-4-2.csv', opts));
IF_5 = table2array(readtable('IF-5.csv', opts));

%x_ax = axes('Accident Year', [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
%y_ax = axes('Development Year', [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])

figure()
h_IF311 = bar3(IF_311);
ylabel('Accident Year')
xlabel('Development Year')
zlabel('Impact')
for i = 1:numel(h_IF311)
  index = logical(kron(isnan(IF_311(:,i)),ones(6,1)));
  zData = get(h_IF311(i),'ZData');
  zData(index,:) = nan;
  set(h_IF311(i),'ZData',zData);
end

figure()
h_IF312 = bar3(IF_312);
xlim([0,11])
ylim([0,11])
ylabel('Accident Year')
xlabel('Development Year')
zlabel('Impact')
for i = 1:numel(h_IF312)
  index = logical(kron(isnan(IF_312(:,i)),ones(6,1)));
  zData = get(h_IF312(i),'ZData');
  zData(index,:) = nan;
  set(h_IF312(i),'ZData',zData);
end

figure()
h_IF321 = bar3(IF_321);
ylabel('Accident Year')
xlabel('Development Year')
zlabel('Impact')
for i = 1:numel(h_IF321)
  index = logical(kron(isnan(IF_321(:,i)),ones(6,1)));
  zData = get(h_IF321(i),'ZData');
  zData(index,:) = nan;
  set(h_IF321(i),'ZData',zData);
end

figure()
h_IF321m = bar3(IF_321_marginal);
ylabel('Accident Year')
xlabel('Development Year')
zlabel('Impact')
for i = 1:numel(h_IF321m)
  index = logical(kron(isnan(IF_321_marginal(:,i)),ones(6,1)));
  zData = get(h_IF321m(i),'ZData');
  zData(index,:) = nan;
  set(h_IF321m(i),'ZData',zData);
end

figure()
h_IF322 = bar3(IF_322);
ylabel('Accident Year')
xlabel('Development Year')
zlabel('Impact')
for i = 1:numel(h_IF322)
  index = logical(kron(isnan(IF_322(:,i)),ones(6,1)));
  zData = get(h_IF322(i),'ZData');
  zData(index,:) = nan;
  set(h_IF322(i),'ZData',zData);
end

figure()
h_IF41 = bar3(IF_41);
ylabel('Accident Year')
xlabel('Development Year')
zlabel('Impact')
for i = 1:numel(h_IF41)
  index = logical(kron(isnan(IF_41(:,i)),ones(6,1)));
  zData = get(h_IF41(i),'ZData');
  zData(index,:) = nan;
  set(h_IF41(i),'ZData',zData);
end

figure()
h_IF42 = bar3(IF_42);
ylabel('Accident Year')
xlabel('Development Year')
zlabel('Impact')
for i = 1:numel(h_IF42)
  index = logical(kron(isnan(IF_42(:,i)),ones(6,1)));
  zData = get(h_IF42(i),'ZData');
  zData(index,:) = nan;
  set(h_IF42(i),'ZData',zData);
end

figure()
h_IF5 = bar3(IF_5);
ylabel('Accident Year')
xlabel('Development Year')
zlabel('Impact')
for i = 1:numel(h_IF5)
  index = logical(kron(isnan(IF_5(:,i)),ones(6,1)));
  zData = get(h_IF5(i),'ZData');
  zData(index,:) = nan;
  set(h_IF5(i),'ZData',zData);
end
% https://stackoverflow.com/questions/2050367/how-to-hide-zero-values-in-bar3-plot-in-matlab

