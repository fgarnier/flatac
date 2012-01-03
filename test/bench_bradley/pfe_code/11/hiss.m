function rv = hiss(f, th_freq)
% Input:
%  f       - sampled signal
%  th_freq - threshold frequency beyond which to add 
%            white noise
% Output:
%  original signal with high frequency white noise added

  % make the magnitude of the white noise proportional
  % to the maximum magnitude of f
  noise_lvl = max(abs(f))/3;

  % add high-frequency white noise
  w = randn(1, 8192);  % normally distributed
  W = fft(w);          % transform of white noise
  % zero out frequencies at and below th_freq
  W(1:th_freq) = 0;
  W((8192-th_freq):8192) = 0;
  % transform back to obtain high-frequency white noise
  hfw = noise_lvl * real(ifft(W));

  % add high-frequency white noise to each segment of f in
  % increments of 8192 samples
  rv = zeros(1, length(f));
  for i = 0:8192:length(f)
    sz = min(8192, length(f)-i);
    rv(i+1:i+sz) = f(i+1:i+sz) + hfw(1:sz);
  end
end
