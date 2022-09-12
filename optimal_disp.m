close all;
clear;

list = dir('optimal_history_*.csv');
fitness = csvread('fitness.csv');

R2D = 180/pi;
id = {};
f_3d = figure;
f_fitness = figure;
f_velocity = figure;
f_heading = figure;
f_params = figure;

for ii = 1:size(list,1)
    name = list(ii).name;
    hist = csvread(name);

    figure(f_3d);
    pp = plot3(hist(:,3), hist(:,2), -hist(:,4)); hold on;
    color = get(pp, 'Color');
    scatter3(hist(end,3), hist(end,2), -hist(end,4), 30, color, 'filled'); hold on;
    struct_name = ['structure' name(8:end)];
    if exist(struct_name, 'file')
        struct = csvread(struct_name);
        idx = all(isnan(struct),2);
        idr = diff(find([1;diff(idx);1]));
        polymat = mat2cell(struct,idr,size(struct,2));
        for pp = 1:size(polymat,1)
            if size(polymat{pp},1) > 1
                polygon = polymat{pp};
                bound = boundary(polygon);
                trisurf(bound,polygon(:,2),polygon(:,1),-polygon(:,3),0,'Facecolor',color,'EdgeColor',color,'FaceAlpha',0.3); hold on;
            end
        end
    end

    id_pos = strfind(name, '_');
    id(ii) = {name(id_pos(2)+1:end-4)};

    figure(f_velocity);
    body_velocity = [hist(:,5) hist(:,6) hist(:,7)];
    
    earth_velocity = zeros(size(body_velocity,1),3);
    for jj = 1:size(body_velocity,1)
        phi = hist(jj,11);
        theta = hist(jj,12);
        psi = hist(jj,13);
        sh = sin(phi);
        ch = cos(phi);
        st = sin(theta);
        ct = cos(theta);
        ss = sin(psi);
        cs = cos(psi);
        DCM_nb = [ct*cs, ct*ss, -st;
                  sh*st*cs-ch*ss, sh*st*ss+ch*cs, sh*ct;
                  ch*st*cs+sh*ss, ch*st*ss-sh*cs, ch*ct];
        DCM_bn = DCM_nb';

        earth_velocity(jj,:) = (DCM_bn*body_velocity(jj,:)')';
    end
    plot(hist(:,1), sqrt(earth_velocity(:,1).^2 + earth_velocity(:,2).^2)); hold on;
    xlabel('Time [s]'); ylabel('Velocity [m/s]');
    title('Horizontal velocity');
    
    figure(f_heading); 
    plot(hist(:,1), hist(:,13)*R2D); hold on;
    xlabel('Time [s]'); ylabel('Yaw [deg]');
    title('Heading');
end

figure(f_fitness);
plot(1:size(fitness,1), fitness(:,1)); hold on;
plot(1:size(fitness,1), fitness(:,2));
xlabel('Iteration'); ylabel('Miss distance [m]');
title('Optimisation progress');
legend('Optimal', 'Current', 'Location', 'southeast');

figure(f_params);
scatter3(fitness(:,3), fitness(:,4), fitness(:,2), 10*fitness(:,2).^2, 'filled');
xlabel('c'); ylabel('d'); zlabel('fitness');
title('Local maxima');

figure(f_3d);
grid; axis equal;
xlabel('east'); ylabel('north'); zlabel('up');  
xlim([-12 12]); ylim([-12 12]); zlim([0 10]);
title('Optimal paths');

figure(f_heading);
legend(id, 'Location', 'southeast');

figure(f_velocity);
legend(id, 'Location', 'northeast');