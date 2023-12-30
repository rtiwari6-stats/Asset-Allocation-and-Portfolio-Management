%% setup
m = [0.13; 0.1; 0.08];
C = [0.0400 0.02160 0.01200;
     0.0216 0.03240 0.00648; 
     0.0120 0.00648 0.01440];

AssetScenarios = mvnrnd(m, C, 1000);

target_returns = linspace(0.08,0.13,100);


%% MAD frontier
p = PortfolioMAD('Scenarios', AssetScenarios, 'LowerBound', 0);

A = [ 1 1 1];
b = 1;
p = PortfolioMAD(p, 'AEquality', A, 'bEquality', b);

pwgt = estimateFrontierByReturn(p, target_returns);
plotFrontier(p, pwgt);
hold on;


%% M-V frontier
p1 = Portfolio('AssetMean', m, 'AssetCovar', C);
A = [ 1 1 1];
b = 1;
p1 = Portfolio(p1, 'AEquality', A, 'bEquality', b, 'LowerBound', 0);
p1wgt = estimateFrontierByReturn(p1, target_returns);
plotFrontier(p1,p1wgt);
legend('MAD frontier', 'MV frontier', 'location', 'best');
hold off;


