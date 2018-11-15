%% plot 3D density plot of 

Data = readtable('../EyeTracking_data/Data.csv');
x = Data{:,'x'};
y = Data{:,'y'};

DATA = [x,y];
[bandwidth,density,X,Y]=kde2d(DATA);

% plot the data and the density estimate

% surf(X,Y,density,'LineStyle','none')
W = rand(256,256);
surf(X, Y, density, 'cdata', W)
shading interp
alpha 0.3

colormap hot, hold on, alpha(.3)
set(gca, 'color', 'red');
plot(data(:,1),data(:,2),'w.','MarkerSize',5)


%% 3D Contour Plot with additional information

X = -1:0.01:1;
Y = 0:0.01:2;

[X, Y] = meshgrid(X, Y);

XL = reshape(X, [1, length(Y)*length(X)]);
YL = reshape(Y, [1, length(Y)*length(X)]);
ZL = - ((XL-0).^2 + (YL-1).^2)./(2*sqrt(2*pi));

Z = reshape(ZL, [length(Y), length(X)]);
W = rand(length(Y), length(X)); % additional variable

surf(X, Y, Z, 'cdata', W)
shading interp
alpha 0.3


