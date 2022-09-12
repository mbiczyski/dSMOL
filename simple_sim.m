function simple_sim(id)
    k_horizontal = 10;
    k_vertical = -25;
    k_incline = 40;
    k_turn = 32;
    vel_limit = [25 25 -13 2.1 2.1 4.6];
    pos_limit = [inf inf inf 0.7 0.7 pi];

    extension = ['_' id '.csv'];
    t = csvread(['control' extension], 0, 2, [0 2 0 2]);
    param_control = csvread(['control' extension], 4, 0);
    init_dynamic_state = csvread(['state' extension]);
    
    phi = init_dynamic_state(10);
    theta = init_dynamic_state(11);
    psi = init_dynamic_state(12);
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
    
    control(1) = param_control(1,2); %roll
    control(2) = param_control(1,1); %pitch
    control(3) = param_control(1,4); %thrust
    control(4) = param_control(1,3); %yaw
    
    control_temp(1) = (control(2)-1500)/500;
    control_temp(2) = (control(1)-1500)/500;
    control_temp(3) = (control(3)-1000)/1000;
    control_temp(4) = 0;
    control_temp(5) = 0;
    control_temp(6) = (control(4)-1500)/500;
    k = [k_horizontal, k_horizontal, k_vertical, k_incline, k_incline, k_turn];
    gravity = [0 0 9.81];
    
    control_norm(1:3) = (DCM_bn*control_temp(1:3)')';
    control_norm(4:6) = control_temp(4:6);
    
    init_position = init_dynamic_state(1:3);
    init_velocity = (DCM_bn*init_dynamic_state(4:6)')';
    init_angle = init_dynamic_state(10:12);
    init_rate = init_dynamic_state(13:15);
    
    for ii = 1:3
        nav_acceleration(ii) = k(ii)*control_norm(ii) + gravity(ii);
        nav_velocity(ii) = init_velocity(ii) + nav_acceleration(ii)*t;
        nav_position(ii) = init_position(ii) + init_velocity(ii)*t + 0.5*nav_acceleration(ii)*t^2;

        nav_angacceleration(ii) = k(3+ii)*control_norm(3+ii);
        nav_angvelocity(ii) = init_rate(ii) + nav_angacceleration(ii)*t;
        nav_angposition(ii) = init_angle(ii) + init_rate(ii)*t + 0.5*nav_angacceleration(ii)*t^2;
    end
    
    acceleration = (DCM_nb*nav_acceleration')';
    velocity = (DCM_nb*nav_velocity')';
    position = nav_position;
    angacceleration = nav_angacceleration;
    angvelocity = nav_angvelocity;
    angposition = nav_angposition;
    
    for ii = 1:3
        if abs(velocity(ii)) > abs(vel_limit(ii)) 
            velocity(ii) = vel_limit(ii)*sign(velocity(ii)); 
        end
        if abs(position(ii)) > abs(pos_limit(ii)) 
            position(ii) = pos_limit(ii)*sign(position(ii)); 
        end
        
        if abs(angposition(ii)) > abs(pos_limit(3+ii)) 
            angposition(ii) = pos_limit(3+ii)*sign(angposition(ii)); 
        end
        if abs(angvelocity(ii)) > abs(vel_limit(3+ii)) 
            angvelocity(ii) = vel_limit(3+ii)*sign(angvelocity(ii)); 
        end
    end
    
    dynamic_state = [position, velocity, acceleration, angposition, angvelocity, angacceleration];
%     state_change = dynamic_state - init_dynamic_state;
%     disp(state_change);
    dlmwrite(['state' extension], dynamic_state, 'delimiter', ',', 'precision', 9);
end