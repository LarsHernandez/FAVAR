% Lags (leads) data by "period" periods

function laggedseries = lagn(data,period)

% data is a column vector.
% period is an integer.
% If period < 0 ==> lead operator

[T,N] 					= size(data);

if period > 0

lowerlaggedseries		= data(1:T-period,:);

topoflaggedseries		= NaN*ones(period,N);

laggedseries			= [topoflaggedseries;lowerlaggedseries];

else
   
period=abs(period);   

topoflaggedseries		= data(period+1:T,:);

lowerlaggedseries		= NaN*ones(period,N);

laggedseries			= [topoflaggedseries;lowerlaggedseries];

end