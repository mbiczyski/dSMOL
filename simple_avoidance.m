function simple_avoidance(id)
    extension = ['_' id '.csv'];
    params = csvread(['control' extension], 0, 0, [0 0 0 4]); % c d g e r
    r=params(5);
    self = csvread(['control' extension], 1, 0, [1 0 1 2]);
    D = csvread(['control' extension], 2, 0, [2 0 2 2]);
    
    fid=fopen(['control' extension]); 
    test = textscan(fid,'%d',1,'delimiter','\n', 'headerlines',3);
    fclose(fid);
    if isempty(test)
        local = csvread(['control' extension], 3, 0);
        F = sum((self-local)./abs((self-local).^3), 1);
    else
        F = [0 0 0];
    end
    
    D_new = D + r*F;
    dlmwrite(['control' extension], D_new, 'delimiter', ',', 'precision', 9);
end