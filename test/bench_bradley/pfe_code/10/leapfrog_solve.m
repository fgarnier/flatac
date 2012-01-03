function sol = leapfrog_solve(ydot, init, t)
% Input:
%  ydot - a function for computing ydot given t and y
%  init - the initial condition
%  t    - a row vector of times at which to solve
% Output: a length(init) x length(t) matrix giving the
%  solutions at the specified time steps t
  steps = length(t);
  % only works for this problem: 2 position dimensions, 
  % 2 velocity dimensions
  sol = zeros(4, steps);

  sol(:,1) = init;
  for i = 1:length(t)-1
    step = t(i+1)-t(i);
    % Compute next-time position using
    %  1. current-time velocity
    %  2. current-time acceleration
    dot1 = ydot(t(i), sol(:,i));
    sol(1:2,i+1) = sol(1:2,i) + step * sol(3:4,i) + ...
      step*step/2 * dot1(3:4);
    % Compute next-time velocity using
    %  1. current-time acceleration
    %  2. next-time acceleration (which requires next-time 
    %     position from above)
    dot2 = ydot(t(i+1), sol(:,i+1));
    sol(3:4,i+1) = sol(3:4,i) + ...
      step * (dot1(3:4) + dot2(3:4))/2;
  end
end
