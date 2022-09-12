function simple_swarm(id)
    rng('shuffle');
    
    extension = ['_' id '.csv'];
    params = csvread(['control' extension], 0, 0, [0 0 0 4]); % c d g e r
    c=params(1); d=params(2); g=params(3); e=params(4);
    self = csvread(['control' extension], 1, 0, [1 0 1 2]);
    target = csvread(['control' extension], 2, 0, [2 0 2 2]);
    D = csvread(['control' extension], 3, 0, [3 0 3 2]);
    
    fid=fopen(['control' extension]); 
    test = textscan(fid,'%d',1,'delimiter','\n', 'headerlines',4);
    fclose(fid);
    if isempty(test)
        local = csvread(['control' extension], 4, 0);
        number = size(local, 1);
        CoM = sum(local, 1)./number;
        C = CoM - self;
        C_norm = C/norm(C);
    else
        C_norm = [0 0 0];
    end
    
    if D == [0 0 0]
        D_norm = [0 0 0];
    else
        D_norm = D/norm(D);
    end
    G = target - self;
    E = [2*rand-1 2*rand-1 2*rand-1];
    E_norm = E/norm(E);
    
    D_new = d*D_norm + c*C_norm + g*G + e*E_norm;
    dlmwrite(['control' extension], D_new, 'delimiter', ',', 'precision', 9);
end