function rv = chord(spec, dur)
  % obtain number of notes in chord
  [N, width] = size(spec);
  if (width ~= 2)
    % malformed input
    rv = [];
    return
  end
 
  % sample times
  t = (0:8192*dur-1)/8192;
  % initialize signal and accumulate notes into it
  rv = zeros(1, length(t));
  for j = 1:N
    f = 440 * 2^(spec(j,1) + spec(j,2)/12);
    rv = rv + sin(2*pi*f*t);
  end
  % scale the signal to within (-1, 1)
  rv = rv/N * 0.999;
end
