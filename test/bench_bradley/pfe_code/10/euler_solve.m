function sol = euler_solve(ydot, init, t)
% Input:
%  ydot - a function for computing ydot given t and y
%  init - the initial condition
%  t    - a row vector of times at which to solve
% Output: a length(init) x length(t) matrix giving the
%  solutions at the specified time steps t
  n = length(init);   % determine the dimension
  steps = length(t);  % determine how many time steps
  % create solution matrix
  % #columns is # of discrete time steps
  % #rows is # of dimensions
  sol = zeros(n, steps);

  % at time t(1), the state is init
  sol(:,1) = init;
  % iterate through time
  for i = 1:length(t)-1
    % add slope * time-step to current values
    sol(:,i+1) = sol(:,i) + ...
      (t(i+1)-t(i)) * ydot(t(i), sol(:,i));
  end
end
