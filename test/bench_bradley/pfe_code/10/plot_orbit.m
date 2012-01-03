function plot_orbit(y0, T, s, solve)
% Input:
%  y0    - the initial state of the satellite
%  T     - the maximum time to solve to
%  s     - the step-size (delta-T)
%  solve - a function to a solver
% Plots the orbit and the velocity vs. time for the 
% satellite system, using the provided solver.

  % Solve the system.
  sol = solve(@orbit, y0, 0:s:T);

  % Clear the plot window.
  clf;
  % Plot the orbit and the velocity w.r.t. time.
  % plot 1: the orbit
  subplot(2, 1, 1);
  hold on;
  title('Position');
  xlabel('X');
  ylabel('Y');
  % central mass
  plot([0], [0], 'or');
  % orbit
  plot(sol(1,:), sol(2,:), '-b');
  axis('equal');
  % plot 2: the velocity w.r.t. time
  v = velocity(sol);
  subplot(2, 1, 2);
  hold on;
  title('Velocity');
  xlabel('Time');
  ylabel('Absolute velocity');
  plot(0:s:T, v, '-b');
end

function ydot = orbit(t, y)
% Returns the vector of the derivative of y at time t.
  r = sqrt(y(1:2)' * y(1:2));
  ydot = [y(3);          ... % an ellipsis continues the 
          y(4);          ... % statement to the next line
          -1/r^3 * y(1); ...
          -1/r^3 * y(2)];
end

function V = velocity(sol)
% Returns the vector of the velocities of the satellite at
% the timesteps.
  V = sqrt(sol(3,:).*sol(3,:) + sol(4,:).*sol(4,:));
end
