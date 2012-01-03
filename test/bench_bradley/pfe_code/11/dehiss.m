function rv = dehiss(f, th_freq)
% Input:
%  f       - sampled signal
%  th_freq - threshold frequency beyond which to cancel
%            frequency components
% Output:
%  original signal with high frequency components canceled

  % modify one "window" of 8192 samples at a time
  for i = 0:8192:length(f)
    if i+8192 <= length(f)
      lo = i+1;
      hi = i+8192;

      % eliminate hiss in f(lo:hi)
      % YOUR CODE HERE
    end
  end
end
