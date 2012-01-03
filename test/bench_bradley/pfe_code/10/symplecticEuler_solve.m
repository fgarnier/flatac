function sol = symplecticEuler_solve(ydot, init, t)
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
    dot = ydot(t(i), sol(:,i));
    % Compute the next-time velocity...
    sol(3:4,i+1) = sol(3:4,i) + (t(i+1)-t(i)) * dot(3:4);
    % ... and use to compute the next-time position.
    sol(1:2,i+1) = sol(1:2,i) + ...
      (t(i+1)-t(i)) * sol(3:4,i+1);
  end
end
