% IMPORTANT NOTE: Decoder only accept the Recommended Huffman Table Set
% presented in the Jpeg Standard.
% Please site following paper for academic use;
% Uzun, Erkam, and Hüsrev Taha Sencar. "Carving orphaned JPEG file fragments." IEEE Transactions on Information Forensics and Security 10.8 (2015): 1549-1563.

% NOTE 2: Do NOT remove dht.mat and obj.mat files.
% Example Usage
% --------------
% rgb=eu_ofd(fragment_string);

% Output:
% rgb: uint8 rgb matrix

% Input:
% fragment_tring: Binary or Hexadecimal String 
% Binary: 101101010101010....
% Hexadecimal: e2 94 af dd ca 95 ...

%% Read hex data from test_hex_data.txt
hex_data=[];
fid=fopen('test_hex_data.txt','r');
line=fgetl(fid);
while ~isnumeric(line)
    hex_data=strcat(hex_data,line);
    line=fgetl(fid);
end

%% Decode Orphaned Fragment Data: hex_data
rgb=eu_ofd(hex_data);

%% Show recovered fragment
imtool(rgb);