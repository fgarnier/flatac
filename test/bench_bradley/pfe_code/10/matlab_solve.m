function sol = matlab_solve(ydot, init, t)
% Input:
%  ydot - a function for computing ydot given t and y
%  init - the initial condition
%  t    - a row vector of times at which to solve
% Output: a length(init) x length(t) matrix giving the
%  solutions at the specified time steps t
  % ignore the time-step part of the output (dummy)
  [dummy, sol] = ode15s(ydot, t, init);
  % transpose it to be like the output of the other solvers
  sol = sol';
end
