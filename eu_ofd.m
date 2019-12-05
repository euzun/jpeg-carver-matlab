
% IMPORTANT NOTE: Decoder only accept the Recommended Huffman Table Set
% presented in the Jpeg Standard.

% NOTE 2: Do NOT remove dht.mat and obj.mat files.

% Please site following paper for academic use;
% Uzun, Erkam, and Hüsrev Taha Sencar. "Carving orphaned JPEG file fragments." IEEE Transactions on Information Forensics and Security 10.8 (2015): 1549-1563.

function rgb=eu_ofd(chunk)
chunk=regexprep(chunk,'[^\w'']','');

hexflag=0;
for i=2:15
    hexflag=numel(strfind(chunk,dec2hex(i)));
    if hexflag>0
        break;
    end
end

if hexflag>0
    chunkSize=floor(numel(chunk)/12)*12;
    chunkBin='';
    for i=1:12:chunkSize
        chunkBin=[chunkBin dec2bin(hex2dec(chunk(i:i+11)),48)];
    end
    chunk=[chunkBin dec2bin(hex2dec(chunk(i:end)),numel(chunk(i:end))*4)];
end

bndryBackUp=eu_check_byte_boundary(chunk,1);

chunk(1:bndryBackUp(1))=[];

load('dht.mat');

lb1=[9, 16,11, 16];

[rgb,flag]=decode4huffman(chunk,lb1,dht.h1);
if ~flag
    errordlg('Unknown Huffman Table!','Error');
end
% 
% if flag
%     imtool(rgb);
% end

end

function [rgb_final,dht_flag]=decode4huffman(chunk,last_bound,huffman)
invalid_cw=zeros(3,1);
overflow=zeros(3,1);

[y_array2,cb_array2,cr_array2,ptr,int_code,overflow(1),invalid_cw(1)]=decodeAs(chunk,2,last_bound,huffman);
% invalid_cw(1)= eu_is_full_decoded( int_code,ptr,chunk_size );

[y_array4,cb_array4,cr_array4,ptr,int_code,overflow(2),invalid_cw(2)]=decodeAs(chunk,4,last_bound,huffman);
% invalid_cw(2)= eu_is_full_decoded( int_code,ptr,chunk_size );

[y_array1,cb_array1,cr_array1,ptr,int_code,overflow(3),invalid_cw(3)]=decodeAs(chunk,1,last_bound,huffman);
% invalid_cw(3)= eu_is_full_decoded( int_code,ptr,chunk_size );

rgb_final=uint8(0);
dht_flag=1;
%----------------AUTHENTICATE HUFFMAN TABLE--------------------------------
if min(overflow)>5
    dht_flag=0;
    %     errordlg('Wrong Huffman Table!','Error');
    return;
end
%--------------------------------------------------------------------------


try
    %--------------------------DETECT CHROMA SUBSAMPLING-----------------------
    [chr_sub,y_array,cb_array,cr_array]=eu_detect_chr_sub(invalid_cw,overflow,y_array2,cb_array2,cr_array2,y_array4,cb_array4,cr_array4,y_array1,cb_array1,cr_array1);
    %--------------------------------------------------------------------------
    
    %--------------------------DETECT IMAGE SIZE-------------------------------
    [wfinal,offset_bound] = eu_detect_width( chr_sub,y_array,cb_array,cr_array );
    %--------------------------------------------------------------------------
    
    %--------------------------DETECT IMAGE OFFSET-----------------------------
    [rgb_after_offset,offset,tail] = eu_detect_offset(y_array,cb_array,cr_array,wfinal,chr_sub);
    
    %     if is_big_chunk
    %         rgb_final=eu_decode_big_jpeg([chunk chunk_rest],offset,wfinal,chr_sub,huffman);
    %     else
    %----------------CALIBRATE BRIGHTNESS--------------------------------------
    %         rgb_final = imadjust(rgb_after_offset,stretchlim(rgb_after_offset),[]);
    rgb_final=rgb_after_offset;
    %--------------------------------------------------------------------------
    %     end
catch
    errordlg('Pseudo Header Creation Error!','Error');
end

end

function rgb_final=eu_decode_big_jpeg(chunk,offset,width,chr_sub,huffman)
eob=eu_resolve_eob_cw(huffman);
switch chr_sub
    case '4:2:2'
        offset_rep=offset/16;
        zero_block_cw=[eob{1} eob{2} eob{1} eob{2} eob{3} eob{4} eob{3} eob{4} ];
    case '4:4:0'
        offset_rep=2*offset/8;
        zero_block_cw=[eob{1} eob{2} eob{1} eob{2} eob{3} eob{4} eob{3} eob{4} ];
    case '4:2:0'
        offset_rep=2*offset/16;
        zero_block_cw=[eob{1} eob{2} eob{1} eob{2} eob{1} eob{2} eob{1} eob{2} eob{3} eob{4} eob{3} eob{4} ];
    case '4:4:4'
        offset_rep=offset/8;
        zero_block_cw=[eob{1} eob{2} eob{3} eob{4} eob{3} eob{4} ];
end

offset_cw=[];
for i=1:offset_rep
    offset_cw=[offset_cw zero_block_cw];
end

chunk=[offset_cw chunk];
padding='00000000';

if ~isequal(chunk(end-15:end),'1111111111011001')
    if mod(numel(chunk),8)~=0
        chunk=[chunk padding(1:8-mod(numel(chunk),8))];
    end
    chunk=[chunk '1111111111011001'];
else
    if mod(numel(chunk),8)~=0
        chunk=[chunk(1:end-16) padding(1:8-mod(numel(chunk),8)) chunk(end-15:end)];
    end
end
header='11111111110110001111111111100000000000000001000001001010010001100100100101000110000000000000000100000001000000000000000000000001000000000000000100000000000000001111111111011011000000000100001100000000000000010000000100000001000000100000000100000001000000100000001000000010000000100000001100000010000000100000001100000011000001100000010000000011000000110000001100000011000001110000010100001000000001000000011000001000000010000000101000001001000010000000011100001011000010000000101000001110000011010000101100001010000010100000110000001010000010000000100000001011000100000000110000001100000011010000111100001111000011110000111100001001000010110001000000010001000011110000111000010001000011010000111000001110000011101111111111011011000000000100001100000001000001000000010000000100000001010000010000000101000010010000010100000101000010010000111100001010000010000000101000001111000110100001001100001001000010010001001100011010000110100001101000011010000011010001101000011010000110100001101000011010000110100001101000011010000110100001101000011010000110100001101000011010000110100001101000011010000110100001101000011010000110100001101000011010000110100001101000011010000110100001101000011010000110100001101000011010000110100001101000011010000110100001101000011010000110101111111111000000000000000001000100001000000001100000000000001000000000000000001100000001001000010000000000000010000100010000000100000011000100010000000111111111110001000000000000011111000000000000000000000001000001010000000100000001000000010000000100000001000000010000000000000000000000000000000000000000000000000000000000000000000000010000001000000011000001000000010100000110000001110000100000001001000010100000101111111111110001000000000010110101000100000000000000000010000000010000001100000011000000100000010000000011000001010000010100000100000001000000000000000000000000010111110100000001000000100000001100000000000001000001000100000101000100100010000100110001010000010000011000010011010100010110000100000111001000100111000100010100001100101000000110010001101000010000100000100011010000101011000111000001000101010101001011010001111100000010010000110011011000100111001010000010000010010000101000010110000101110001100000011001000110100010010100100110001001110010100000101001001010100011010000110101001101100011011100111000001110010011101001000011010001000100010101000110010001110100100001001001010010100101001101010100010101010101011001010111010110000101100101011010011000110110010001100101011001100110011101101000011010010110101001110011011101000111010101110110011101110111100001111001011110101000001110000100100001011000011010000111100010001000100110001010100100101001001110010100100101011001011010010111100110001001100110011010101000101010001110100100101001011010011010100111101010001010100110101010101100101011001110110100101101011011011010110111101110001011100110111010110000101100001111000100110001011100011011000111110010001100100111001010110100101101001111010100110101011101011011010111110110001101100111011010111000011110001011100011111001001110010111100110111001111110100011101001111010101111000111110010111100111111010011110101111101101111011111111000111110011111101011111111110001000000000000011111000000010000000000000011000000010000000100000001000000010000000100000001000000010000000100000001000000000000000000000000000000000000000000000000000000010000001000000011000001000000010100000110000001110000100000001001000010100000101111111111110001000000000010110101000100010000000000000010000000010000001000000100000001000000001100000100000001110000010100000100000001000000000000000001000000100111011100000000000000010000001000000011000100010000010000000101001000010011000100000110000100100100000101010001000001110110000101110001000100110010001000110010100000010000100000010100010000101001000110100001101100011100000100001001001000110011001101010010111100000001010101100010011100101101000100001010000101100010010000110100111000010010010111110001000101110001100000011001000110100010011000100111001010000010100100101010001101010011011000110111001110000011100100111010010000110100010001000101010001100100011101001000010010010100101001010011010101000101010101010110010101110101100001011001010110100110001101100100011001010110011001100111011010000110100101101010011100110111010001110101011101100111011101111000011110010111101010000010100000111000010010000101100001101000011110001000100010011000101010010010100100111001010010010101100101101001011110011000100110011001101010100010101000111010010010100101101001101010011110101000101010011010101010110010101100111011010010110101101101101011011110111000101110011011101011000010110000111100010011000101110001101100011111001000110010011100101011010010110100111101010011010101110101101101011111011000110110011101101011100010111000111110010011100101111001101110011111101000111010011110101011110010111100111111010011110101111101101111011111111000111110011111101011111111110110100000000000001100000000110000000100000000000000100001000100000011000100010000000000111111';
chunk=[header chunk];
rgb_final=write_bin_data(chunk);
bw=im2bw(rgb_final);

s=sum(sum(bw(end-7:end,:)));
n=numel(bw(end-7:end,:));
c=0;
while s==n
    c=c+1;
    s=sum(sum(bw(end-7-(c-1)*8:end-(c-1)*8,:)));
    n=numel(bw(end-7-(c-1)*8:end-(c-1)*8,:));
end

rgb_final(end-c*8+9:end,:,:)=[];
end

function rgb_final=write_bin_data(chunk)
fid = fopen('jpeg1.jpg','a+');

try
    for i=1:8:length(chunk)
        dec=bin2dec(chunk(i:i+7));
        fwrite(fid,dec);
    end
catch me
    dec=bin2dec(chunk(i:end));
    fwrite(fid,dec);
end
fclose(fid);

rgb_final=imread('jpeg1.jpg');
end

function eob=eu_resolve_eob_cw(huffman)
eob=cell(4,1);
eob(1)=cellstr(eu_get_comp_eob(huffman.y_dc_map));
eob(2)=cellstr(eu_get_comp_eob(huffman.y_ac_map));
eob(3)=cellstr(eu_get_comp_eob(huffman.c_dc_map));
eob(4)=cellstr(eu_get_comp_eob(huffman.c_ac_map));

end

function eob=eu_get_comp_eob(map)
keys=map.keys;
eob=[];
for i=1:numel(keys)
    if  isequal( map(keys{i}),'00')
        eob= keys{i};
        return;
    end
end

end

function bndry=eu_check_byte_boundary(chunk,check_int)
bndry=0:1:7;
for i=[65281:65487 65498:65535]
    cand=intersect( unique( mod( strfind( chunk,dec2bin(i) )-1,8 ) ), bndry);
    if ~isempty(cand)
        for j=cand
            bndry(bndry==j)=[];
        end
        if numel(bndry)==0
            return;
        end
    end
end

if check_int==1
    rstBndry=[];
    for i=16711888:16711895
        bndry1=unique( mod( strfind( chunk,dec2bin(i) )-1,8 ) );
        if ~isempty(bndry1)
            rstBndry=[rstBndry bndry1];
        end
    end
    bndryInt=intersect(bndry,rstBndry);
    if ~isempty(bndryInt)
        bndry=bndryInt;
    end
end
end

function [y_array,cb_array,cr_array,ptr,int_code,overflow,invalid]=decodeAs(chunk,y_count,last_bound,huffman)

overflow=0;
invalid=0;
y_dc_previous=0;
cb_dc_previous=0;
cr_dc_previous=0;

y_array=[];
cb_array=[];
cr_array=[];

cnt=0;
ptr=1;

is_valid=1;
is_diff=1;
is_dec_diff=1;
nextbyte=0;
int_code='null';
chunk_size=numel(chunk);

% h=waitbar(ptr/numel(chunk),'Decoding');
% set(h,'Name',['Decode As' num2str(y_count) 'YCbCr']);

while overflow<6 && ptr<chunk_size-16
%     wb=ptr/chunk_size;
%     waitbar(wb,h,['Decoding ' num2str(floor(100*wb)) '%']);
    
    %     last_bound=[9,16];
    y_array_temp=[];
    for i=1:y_count
        [is_valid,ptr,cnt,nextbyte,zzmatris,int_code,overflow]=decodeBlock(last_bound(1:2),chunk,huffman.y_dc_map,huffman.y_ac_map,y_dc_previous,ptr,cnt,nextbyte,overflow);
        y_dc_previous=zzmatris(1);
        if ~is_valid
            ptr=ptr+1;
            break;
        end
        y_array_temp=[y_array_temp zzmatris];
    end
    
    if is_valid
        [is_valid,ptr,cnt,nextbyte,zzmatris,int_code,overflow]=decodeBlock(last_bound(3:4),chunk,huffman.c_dc_map,huffman.c_ac_map,cb_dc_previous,ptr,cnt,nextbyte,overflow);
        cb_dc_previous=zzmatris(1);
    else
        if ptr<chunk_size-16
            ptr=ptr+1;
            invalid=invalid+1;
        end
    end
    
    
    
    if is_valid
        cb_array_temp=zzmatris;
        [is_valid,ptr,cnt,nextbyte,zzmatris,int_code,overflow]=decodeBlock(last_bound(3:4),chunk,huffman.c_dc_map,huffman.c_ac_map,cr_dc_previous,ptr,cnt,nextbyte,overflow);
        cr_dc_previous=zzmatris(1);
    else
        if ptr<chunk_size-16
            ptr=ptr+1;
            invalid=invalid+1;
        end
    end
    
    if is_valid
        y_array=[y_array y_array_temp];
        cb_array=[cb_array cb_array_temp];
        cr_array=[cr_array zzmatris];
    else
        if ptr<chunk_size-16
            ptr=ptr+1;
            invalid=invalid+1;
        end
    end
    
end

% waitbar(1,h,'Decoded');
% close(h);
end

function [is_valid,ptr,cnt,nextbyte,zzmatris,int_code,overflow]=decodeBlock(last_bound,chunk,dc_map,ac_map,dc_previous,ptr,cnt,nextbyte,overflow)

zzmask = [1,11,34,28,29,44,24,47;9,4,27,35,22,51,32,40;2,5,20,42,15,58,39,48;3,12,13,49,8,59,46,55;10,19,6,57,16,52,53,62;17,26,7,50,23,45,60,63;25,33,14,43,30,38,61,56;18,41,21,36,37,31,54,64];
ptr_backup=ptr;
zzmatris=zeros(8);
% -------------------------DC COMPONENT------------------------------------
[is_valid,int_code,ptr,cnt,nextbyte,is_dec_diff]=decodeCW(last_bound(1),chunk,dc_map,ptr,cnt,nextbyte);
if ~is_valid
    return;
end

[ zrl,decval_skip, is_valid ] = eu_validate_int_code( int_code);

if is_valid
    if zrl==64
        zrl=0;
        decvalue=0;
    else
        [cnt,nextbyte,is_valid,ptr,decvalue]=findBinValue(cnt,ptr,chunk,decval_skip,nextbyte);
        if ~is_valid
            return;
        end
    end
    
    % if a restart marker does not seen,is_diff=1, add diff value to previous dc,
    % otherwise,is_diff=0, just set to decvalue
    zzmatris(1)=is_dec_diff*dc_previous+decvalue;
    stream_index=1;
    
    %     -------------------------AC COMPONENT--------------------------------
    while stream_index<64
        stream_index=stream_index+1;
        [is_valid,int_code,ptr,cnt,nextbyte,is_dec_diff]=decodeCW(last_bound(2),chunk,ac_map,ptr,cnt,nextbyte);
        if ~is_valid
            return;
        end
        [ zrl,decval_skip, is_valid ] = eu_validate_int_code( int_code);
        
        if is_valid==0
            ptr=ptr_backup;
            break;
        end
        
        if zrl==64
            % EOB, rest is zero.
            break;
        elseif zrl>0
            % skips the index of next first non-zero value
            stream_index=stream_index+zrl;
            if stream_index>64
                overflow=overflow+1;
                break;
            end
        end
        
        if decval_skip>0
            % Not a ZRL or EOB comes
            [cnt,nextbyte,is_valid,ptr,decvalue]=findBinValue(cnt,ptr,chunk,decval_skip,nextbyte);
            if ~is_valid
                return;
            end
            zzmatris(zzmask(stream_index))=decvalue;
        end
        
    end
else
    ptr=ptr_backup;
end

end

function [is_valid,int_code,ptr,cnt,nextbyte,is_dec_diff]=decodeCW(last_bound,chunk,cw_map,ptr,cnt,nextbyte)
int_code='null';
is_dec_diff=1;
is_valid=1;
if cnt==0
    [is_diff,cnt,nextbyte,is_valid,ptr]=readNextByte(ptr,chunk);
    if ~is_valid
        return;
    end
    is_dec_diff=is_dec_diff && is_diff;
end

cw='0000000000000000';
cw_cnt=1;
cw(1)=nextbyte(numel(nextbyte)-cnt+1);
cnt=cnt-1;
while cw_cnt<last_bound
    cw_cnt=cw_cnt+1;
    if cnt==0
        [is_diff,cnt,nextbyte,is_valid,ptr]=readNextByte(ptr,chunk);
        if ~is_valid
            break;
        end
        is_dec_diff=is_dec_diff && is_diff;
        % If restart marker encountered flush the byte buffer
        if ~is_dec_diff
            cw(1)=nextbyte(1);
            cnt=7;
            cw_cnt=2;
        end
    end
    
    cw(cw_cnt)=nextbyte(numel(nextbyte)-cnt+1);
    cnt=cnt-1;
    
    
    if cw_map.isKey(cw(1:cw_cnt))
        int_code=cw_map(cw(1:cw_cnt));
        break;
    end
end

end

function [is_diff,cnt,nextbyte,is_valid,ptr]=readNextByte(ptr,chunk)

is_diff=1;
cnt=8;
nextbyte='';
is_valid=1;
try
    nextbyte=chunk(ptr:ptr+7);
catch me
    is_valid=0;
    return;
end
ptr=ptr+8;
if strcmp(nextbyte,'11111111')
    nextnextbyte=chunk(ptr:ptr+7);
    if strcmp(nextnextbyte,'00000000')
        % stuff byte
        ptr=ptr+8;
    elseif eu_is_rst_marker(nextnextbyte)
        % marker
        is_diff=0;
        ptr=ptr+8;
        nextbyte=chunk(ptr:ptr+7);
        ptr=ptr+8;
    end
end
end

function [ zrl,decval_skip, is_valid ] = eu_validate_int_code( int_code)
is_valid=1;
zrl=0;
decval_skip=-1;
if strcmp(int_code,'null')
    is_valid=0;
elseif strcmp(int_code,'00')
    zrl=64;
    decval_skip=0;
else
    zrl=eu_hex2dec(int_code(1));
    decval_skip=eu_hex2dec(int_code(2));
end

end

function [cnt,nextbyte,is_valid,ptr,decvalue]=findBinValue(cnt,ptr,chunk,decval_skip,nextbyte)
is_valid=1;
decvalue=0;
try
    if cnt>=decval_skip
        sbin=nextbyte(numel(nextbyte)-cnt+1:numel(nextbyte)-cnt+decval_skip);
    else
        sbin='';
        while decval_skip>cnt
            sbin=[sbin nextbyte(numel(nextbyte)-cnt+1:numel(nextbyte))];
            decval_skip=decval_skip-cnt;
            [~,cnt,nextbyte,is_valid,ptr]=readNextByte(ptr,chunk);
        end
        sbin=[sbin nextbyte(1:decval_skip)];
    end
    cnt=cnt-decval_skip;
    decvalue=eu_sbin2sdec(sbin);
catch me
    is_valid=0;
end
end

function out = eu_is_full_decoded( int_code,ptr,chunk_size )
if strcmp(int_code,'null')
    if abs(ptr-chunk_size)<=16
        out=1;
    else
        out=0;
    end
else
    out=1;
end

end

function decval=eu_hex2dec(hexval)

if hexval+0<65
    decval=hexval-48;
else
    decval=hexval-55;
end
end

function is_rst=eu_is_rst_marker(binvalue)

dec=bin2dec(binvalue);
if 215<=dec || dec<=208
    is_rst=0;
else
    is_rst=1;
end

end

function [ sdec ] = eu_sbin2sdec( sbin )
% Converts signed binary string to signed decimal int
% sbin: signed binary string
% sdec: signed decimal
% Erkam Uzun
sdec=bin2dec(sbin)-(2^numel(sbin)-1)*(1-sbin(1)+48);
end

function [chr_sub,y_array,cb_array,cr_array]=eu_detect_chr_sub(invalid_cw,overflow,y1_array,cb1_array,cr1_array,y2_array,cb2_array,cr2_array,y3_array,cb3_array,cr3_array)

%-------------------DETECT MCU STRUCTURE-----------------------------------
of=find(overflow<2);
iv=find(invalid_cw<2);
if numel(of)==1 && ismember(of,iv)
    chr_sub=of;
else
    chr_can=zeros(5,1);
    chr_can(1)= eu_ac_mean_det(cb1_array,cr1_array,cb2_array,cr2_array,cb3_array,cr3_array);
    chr_can(2)= eu_dc_mean_det(y1_array,cb1_array,cr1_array,y2_array,cb2_array,cr2_array,y3_array,cb3_array,cr3_array);
    chr_can(3)= eu_coeff_mean_det(y1_array,cb1_array,cr1_array,y2_array,cb2_array,cr2_array,y3_array,cb3_array,cr3_array);
    [chr_can(4), chr_can(5)]= eu_ac_var_det(cb1_array,cr1_array,cb2_array,cr2_array,cb3_array,cr3_array);
    
    chr_sub=mode(chr_can);
end
%--------------------------------------------------------------------------
% chr_sub=3;
%-----------------DETECT CHROMA SUBSAMPLING----------------------------------
if chr_sub==1
    %     DETECT LUMINANCE LAYOUT
    chr_sub = eu_detect_lum_layout( y1_array,cb1_array,cr1_array );
    y_array=y1_array;
    cb_array=cb1_array;
    cr_array=cr1_array;
elseif chr_sub==2
    chr_sub='4:2:0';
    y_array=y2_array;
    cb_array=cb2_array;
    cr_array=cr2_array;
else
    chr_sub='4:4:4';
    y_array=y3_array;
    cb_array=cb3_array;
    cr_array=cr3_array;
end
%--------------------------------------------------------------------------
end

function out = eu_select_chroma( x )
if x(1)==min(x) && numel(min(x))==1
    out=1;
elseif x(2)==min(x) && numel(min(x))==1
    out=2;
elseif x(3)==min(x) && numel(min(x))==1
    out=3;
end
end

function out= eu_ac_mean_det(cb1_array,cr1_array,cb2_array,cr2_array,cb3_array,cr3_array)

cb_ac_mean=[comp_ac_mean(cb1_array) comp_ac_mean(cb2_array) comp_ac_mean(cb3_array)];
cr_ac_mean=[comp_ac_mean(cr1_array) comp_ac_mean(cr2_array) comp_ac_mean(cr3_array)];

out = eu_select_chroma( cb_ac_mean+cr_ac_mean );
end

function ac_mean=comp_ac_mean(comp_array)
x=comp_array(setdiff(1:end,1:64:end));
ac_mean=sum(abs(x))/numel(find(x~=0));
end

function out= eu_dc_mean_det(y1_array,cb1_array,cr1_array,y2_array,cb2_array,cr2_array,y3_array,cb3_array,cr3_array)

y_dc_mean=[comp_dc_mean(y1_array) comp_dc_mean(y2_array) comp_dc_mean(y3_array)];
cb_dc_mean=[comp_dc_mean(cb1_array) comp_dc_mean(cb2_array) comp_dc_mean(cb3_array)];
cr_dc_mean=[comp_dc_mean(cr1_array) comp_dc_mean(cr2_array) comp_dc_mean(cr3_array)];

out = eu_select_chroma(y_dc_mean+ cb_dc_mean+cr_dc_mean );
end

function dc_mean=comp_dc_mean(comp_array)
dc_mean=mean(abs(comp_array(1:64:end)));
end

function out= eu_coeff_mean_det(y1_array,cb1_array,cr1_array,y2_array,cb2_array,cr2_array,y3_array,cb3_array,cr3_array)

y_coeff_mean=[comp_coeff_mean(y1_array) comp_coeff_mean(y2_array) comp_coeff_mean(y3_array)];
cb_coeff_mean=[comp_coeff_mean(cb1_array) comp_coeff_mean(cb2_array) comp_coeff_mean(cb3_array)];
cr_coeff_mean=[comp_coeff_mean(cr1_array) comp_coeff_mean(cr2_array) comp_coeff_mean(cr3_array)];

out = eu_select_chroma(y_coeff_mean+ cb_coeff_mean+cr_coeff_mean );
end

function coeff_mean=comp_coeff_mean(comp_array)
coeff_mean=sum(sum(abs(comp_array)))/numel(find(comp_array~=0));
end

function [out1, out2]= eu_ac_var_det(cb1_array,cr1_array,cb2_array,cr2_array,cb3_array,cr3_array)


cb_ac_var_mean=[comp_ac_var_mean(cb1_array) comp_ac_var_mean(cb2_array) comp_ac_var_mean(cb3_array)];
cr_ac_var_mean=[comp_ac_var_mean(cr1_array) comp_ac_var_mean(cr2_array) comp_ac_var_mean(cr3_array)];

cb_ac_var_max=[comp_ac_var_max(cb1_array) comp_ac_var_max(cb2_array) comp_ac_var_max(cb3_array)];
cr_ac_var_max=[comp_ac_var_max(cr1_array) comp_ac_var_max(cr2_array) comp_ac_var_max(cr3_array)];

out1 = eu_select_chroma(cb_ac_var_mean+ cr_ac_var_mean);
out2 = eu_select_chroma(cb_ac_var_max+ cr_ac_var_max);
end

function ac_var_mean=comp_ac_var_mean(comp_array)
x=comp_array(setdiff(1:end,1:64:end));
ac_var=[];
for i=1:63:numel(x)
    ac_var=[ac_var;var(x(i:i+62))];
end
ac_var_mean=mean(ac_var);
end

function ac_var_max=comp_ac_var_max(comp_array)
x=comp_array(setdiff(1:end,1:64:end));
ac_var=[];
for i=1:63:numel(x)
    ac_var=[ac_var;var(x(i:i+62))];
end
ac_var_max=max(ac_var);
end

function lum_lay = eu_detect_lum_layout( y1_array,cb1_array,cr1_array )
pre_width=size(y1_array,2);

% if pre_width>64000
%     pre_width=64000;
%     rgb= eu_build_jobj(y1_array(:,1:pre_width),cb1_array(:,1:pre_width/2),cr1_array(:,1:pre_width/2),pre_width,'4:2:2');
% else
%     rgb= eu_build_jobj(y1_array,cb1_array,cr1_array,pre_width,'4:2:2');
% end

rgb= eu_build_jobj(y1_array,cb1_array,cr1_array,pre_width,'4:2:2');
im_width=size(rgb,2);

diff_vert=[];% for 422
diff_horz=[];% for 440

first=8;
for i=1:im_width/16-1
    diff_vert=[diff_vert mean( sqrt( ( rgb(:,first,1)-rgb(:,first+1,1) ).^2 +( rgb(:,first,2)-rgb(:,first+1,2) ).^2+( rgb(:,first,3)-rgb(:,first+1,3) ).^2) )];
    diff_horz=[diff_horz mean(sqrt( ( rgb(8,(i-1)*16+1:(i-1)*16+8,1)' - rgb(1,(i-1)*16+9:(i-1)*16+16,1)' ).^2 +( rgb(8,(i-1)*16+1:(i-1)*16+8,2)' - rgb(1,(i-1)*16+9:(i-1)*16+16,2)' ).^2 +( rgb(8,(i-1)*16+1:(i-1)*16+8,3)' - rgb(1,(i-1)*16+9:(i-1)*16+16,3)' ).^2) )];
    first=first+16;
end

if mean(diff_vert)<=mean(diff_horz)
    out=eu_422_check(rgb);
else
    out=0;
end

if out
    lum_lay='4:2:2';
else
    lum_lay='4:4:0';
end

end

function out=eu_422_check(rgb)
diff_422=mean( mean( sqrt( (rgb(:,9:8:end,1)-rgb(:,8:8:end-1,1)).^2 + (rgb(:,9:8:end,2)-rgb(:,8:8:end-1,2)).^2 + (rgb(:,9:8:end,3)-rgb(:,8:8:end-1,3)).^2) ) );
diff_440=mean( mean( sqrt( (rgb(:,17:8:end,1)-rgb(:,8:8:end-9,1)).^2 + (rgb(:,17:8:end,2)-rgb(:,8:8:end-9,2)).^2 + (rgb(:,17:8:end,3)-rgb(:,8:8:end-9,3)).^2) ) );
out=diff_422<diff_440;
end

function [wfinal,offset_bound] = eu_detect_width( chr_sub,y_array,cb_array,cr_array )
switch chr_sub
    case '4:2:2'
        pre_width=size(y_array,2);
    case '4:4:0'
        pre_width=size(y_array,2)/2;
    case '4:2:0'
        pre_width=size(y_array,2)/2;
    case '4:4:4'
        pre_width=size(y_array,2);
        
end
rgb= eu_build_jobj(y_array,cb_array,cr_array,pre_width,chr_sub );
pre_height=size(rgb,1);
pre_width=size(rgb,2);
%------------------VERTICAL BORDER DIFFERENCE------------------------------
e_mean=[];
for i=1:pre_width/pre_height-1
    e_mean=[e_mean mean( sqrt((rgb(:,i*8,1)-rgb(:,i*8+1,1)).^2 + (rgb(:,i*8,2)-rgb(:,i*8+1,2)).^2 + (rgb(:,i*8,3)-rgb(:,i*8+1,3)).^2) )];
end
%--------------------------------------------------------------------------

w1=eu_width_det1(e_mean);
[w2,offset_bound]=eu_width_det2(e_mean);
w3=eu_width_det3(rgb,pre_width,pre_height);
w4=eu_width_det4( chr_sub,y_array,cb_array,cr_array );
w5=eu_width_det5(rgb,pre_height);

wfinal=eu_width_fus(rgb,w1,w2,w3,w4,w5);
if wfinal<200,wfinal=200; end
end

function w1=eu_width_det1(e_mean)

try
    %Method 1
    x=abs(fft(e_mean,2*numel(e_mean)-1));
    y=fftshift(abs(ifft(x.^2)));
    bound=peakfinder(y)';
    %     peakfinder(y)
    if numel(bound)<4
        bound=peakfinder(y,(max(y)-min(y))/8)';
        %         peakfinder(y,(max(y)-min(y))/8)
        if numel(bound)<4
            bound=peakfinder(y,(max(y)-min(y))/16)';
        end
    end
    
    diff=[];
    for i=1:numel(bound)-1
        d=(bound(i+1:end)-bound(i));
        d=reshape(d,[1 numel(d)]);
        diff=[diff d];
    end
    diff(diff==1)=[];
    w1=8*mode(diff);
catch
    w1=0;
end

end

function [w2,bound]=eu_width_det2(e_mean)
try
    bound=[];
    [IDX,~,~,~] = kmeans(e_mean,2,'distance','sqEuclidean');
    bound=find(IDX~=mode(IDX));
    index=numel(bound);
    if index>1000
        [IDX2,~,~,~] = kmeans(e_mean(bound),2,'distance','sqEuclidean');
        bound2=find(IDX2~=mode(IDX2));
        bound=bound(bound2);
    end
    
    diff=[];
    for i=1:numel(bound)-1
        d=(bound(i+1:end)-bound(i));
        d=reshape(d,[1 numel(d)]);
        diff=[diff d];
    end
    diff(diff==1)=[];
    w2=8*mode(diff);
catch
    w2=0;
end

end

function w3=eu_width_det3(rgb,pre_width,pre_height)
w_cand=[];
mask_offset=32;
mask_size=320;
window_size=min(640*16,pre_width);

if pre_width>=mask_offset+2*mask_size
    while mask_offset+mask_size<=400
        
        mask=rgb(:,mask_offset+1:mask_offset+mask_size,:);
        window=rgb(:,mask_offset+mask_size+1:window_size,:);
        
        horz_bndry=zeros((size(window,2)-size(mask,2))/8 +1,1);
        
        for i=1:numel(horz_bndry)
            diff=mask(pre_height,:,:)-window(1,(i-1)*8+1:(i-1)*8+mask_size,:);
            diff=shiftdim(diff,1).^2;%R,G,B differences
            diff=sqrt(sum(diff')');
            horz_bndry(i)=mean(diff);
        end
        w_temp=mask_size+(find(horz_bndry==min(horz_bndry))-1)*8;
        if isempty(w_temp)
            w_cand=[w_cand round(rand*198)];
        else
            w_cand=[w_cand w_temp(1)];
        end
        mask_offset=mask_offset+8;
    end
    w3=mode(w_cand);
else
    w3=0;
end
end

function  w4  = eu_width_det4( chr_sub,y_array,cb_array,cr_array )
y_peak=eu_find_dc_peaks(y_array(1,1:8:end));
cb_peak=eu_find_dc_peaks(cb_array(1,1:8:end));
cr_peak=eu_find_dc_peaks(cr_array(1,1:8:end));

switch chr_sub
    case '4:2:2'
        total_peak=[y_peak 2*cb_peak 2*cr_peak];
        im_width=round(mode(total_peak));
    case '4:4:0'
        total_peak=[y_peak/2 cb_peak cr_peak];
        im_width=round(mode(total_peak));
    case '4:2:0'
        total_peak=[y_peak 4*cb_peak 4*cr_peak];
        im_width=round(mode(total_peak)/2);
    case '4:4:4'
        total_peak=[y_peak cb_peak cr_peak];
        im_width=mode(total_peak);
    otherwise
        im_width=0;
end
w4=8*im_width;
end

function peak=eu_find_dc_peaks(comp_dc)
x=abs(fft(comp_dc,2*numel(comp_dc)-1));
y=fftshift(abs(ifft(x.^2)));
a=peakfinder(y);
% peakfinder(y)
if numel(a)<4
    a=peakfinder(y,(max(y)-min(y))/8)';
    %     peakfinder(y,(max(y)-min(y))/8)
    if numel(a)<4
        a=peakfinder(y,(max(y)-min(y))/16)';
    end
end
peak=[];
for i=1:numel(a)-1
    d=(a(i+1:end)-a(i));
    d=reshape(d,[1 numel(d)]);
    peak=[peak d];
end
peak(peak==1)=[];
end

function w5=eu_width_det5(rgb,pre_height)
try
    r=rgb(:,:,1);
    g=rgb(:,:,2);
    b=rgb(:,:,3);
    
    sub_diff=[];
    for i=40:size(r,2)/8
        sub_diff=[sub_diff mean( sqrt((r(pre_height,1:i*8)-r(1,end-i*8+1:end)).^2 + (g(pre_height,1:i*8)-g(1,end-i*8+1:end)).^2 + (b(pre_height,1:i*8)-b(1,end-i*8+1:end)).^2) )];
    end
    w5=(numel(sub_diff)-find(sub_diff==min(sub_diff)))*8;
    if isempty(w5)
        w5=0;
    end
catch
    w5=0;
end
end

function wfinal=eu_width_fus(rgb,w1,w2,w3,w4,w5)
wcand_pre=[w1 w2 w3 w4 w5];
wcand=unique(wcand_pre);% filtering repeating elements


if numel(wcand)>1
    if numel(find(wcand_pre==mode(wcand_pre)))>2
        wfinal=mode(wcand_pre);
    else
        wcand=wcand(wcand>100);% filtering NaN elements
        if numel(wcand)==0
            wfinal=0;
        else
            gd=[];
            for i=1:numel(wcand)
                gd=[gd eu_grid_diff(rgb,wcand(i))];
                wfinal=wcand(find(gd==min(gd)));
                if numel(wfinal)==0
                    wfinal=0;
                else
                    wfinal=wfinal(1);
                end
            end
        end
    end
else
    wfinal=wcand;
end
end

function gd=eu_grid_diff(rgb,w)

r=eu_reshape_component(rgb(:,:,1),w);
g=eu_reshape_component(rgb(:,:,2),w);
b=eu_reshape_component(rgb(:,:,3),w);

r_v=( r(:,9:8:end)-r(:,8:8:end-1) ).^2;
r_h=( r(9:8:end,:)-r(8:8:end-1,:) ).^2;

g_v=( g(:,9:8:end)-g(:,8:8:end-1) ).^2;
g_h=( g(9:8:end,:)-g(8:8:end-1,:) ).^2;

b_v=( b(:,9:8:end)-b(:,8:8:end-1) ).^2;
b_h=( b(9:8:end,:)-b(8:8:end-1,:) ).^2;


gd=mean( mean(mean(sqrt(r_v+g_v+b_v)))+mean(mean(sqrt(r_h+g_h+b_h))) );

end

function [rgb_after_offset,offset,tail]= eu_detect_offset(y_array,cb_array,cr_array,wfinal,chr_sub )

switch chr_sub
    case '4:2:2'
        mcu_w=16;
    case '4:4:0'
        mcu_w=8;
    case '4:2:0'
        mcu_w=16;
    case '4:4:4'
        mcu_w=8;
end

rgb=eu_build_jobj(y_array,cb_array,cr_array,wfinal,chr_sub );

% imtool(rgb);
im_height=size(rgb,1);
im_width=size(rgb,2);
e_mean=[];
for i=1:im_width/mcu_w-1
    e_mean=[e_mean mean( sqrt((rgb(9:end,i*mcu_w,1)-rgb(9:end,i*mcu_w+1,1)).^2 + (rgb(9:end,i*mcu_w,2)-rgb(9:end,i*mcu_w+1,2)).^2 + (rgb(9:end,i*mcu_w,3)-rgb(9:end,i*mcu_w+1,3)).^2) )];
end
index_val=find(e_mean==max(e_mean));
offset=mod((wfinal/mcu_w- index_val)*mcu_w,wfinal);
if isnan(offset)
    offset=0;
end
tail=0;
%----------------CALIBRATE CONTRAST----------------------------------------
y_array=eu_calibrate_dc(y_array);
cb_array=eu_calibrate_dc(cb_array);
cr_array=eu_calibrate_dc(cr_array);
%--------------------------------------------------------------------------

%---------------APPLY OFFSET-----------------------------------------------
switch chr_sub
    case '4:2:2'
        offset=ceil(offset/16)*16;
        tail=wfinal-mod(size(y_array,2)+offset,wfinal);
        y_array=[zeros(8,offset) y_array zeros(8,tail)];
        cb_array=[zeros(8,offset/2) cb_array zeros(8,tail/2)];
        cr_array=[zeros(8,offset/2) cr_array zeros(8,tail/2)];
    case '4:4:0'
        offset=ceil(offset/8)*8;
        tail=wfinal-mod(size(y_array,2)/2+offset,wfinal);
        y_array=[zeros(8,2*offset) y_array zeros(8,2*tail)];
        cb_array=[zeros(8,offset) cb_array zeros(8,tail)];
        cr_array=[zeros(8,offset) cr_array zeros(8,tail)];
    case '4:2:0'
        offset=ceil(offset/16)*16;
        tail=wfinal-mod(size(y_array,2)/2+offset,wfinal);
        y_array=[zeros(8,2*offset) y_array zeros(8,2*tail)];
        cb_array=[zeros(8,offset/2) cb_array zeros(8,tail/2)];
        cr_array=[zeros(8,offset/2) cr_array zeros(8,tail/2)];
    case '4:4:4'
        offset=ceil(offset/8)*8;
        tail=wfinal-mod(size(y_array,2)+offset,wfinal);
        y_array=[zeros(8,offset) y_array zeros(8,tail)];
        cb_array=[zeros(8,offset) cb_array zeros(8,tail)];
        cr_array=[zeros(8,offset) cr_array zeros(8,tail)];
end

rgb_after_offset=eu_build_jobj(y_array,cb_array,cr_array,wfinal,chr_sub );
%--------------------------------------------------------------------------
end

function coef = eu_reshape_component( array,width )
a_height=size(array,1);
row_count=floor(size(array,2)/width);
coef=zeros(a_height*row_count,width);

for i=1:row_count
    coef((i-1)*a_height+1:i*a_height,:)=array(:,(i-1)*width+1:i*width);
end

end

function rgb= eu_build_jobj(y_array,cb_array,cr_array,width,sub_samp )

switch sub_samp
    case '4:4:0'
        y_coef=[];
        residue=y_array;
        width=ceil(width/8)*8;
        for i=1:size(residue,2)/(2*width)
            coef=[];
            temp=residue(:,1:2*width);
            for j=1:size(temp,2)/16
                coef=[coef temp(:,8*(j-1)+1:8*j)];
                temp(:,8*(j-1)+1:8*j)=[];
            end
            coef=[coef;temp];
            y_coef=[y_coef;coef];
            residue(:,1:2*width)=[];
        end
        cb_coef=eu_reshape_component( cb_array,width );
        cr_coef=eu_reshape_component( cr_array,width );
        load('obj.mat','jobj_440');
        jobj=jobj_440;
    case '4:2:2'
        width=ceil(width/16)*16;
        y_coef=eu_reshape_component( y_array,width);
        c_width=width/2;
        cb_coef=eu_reshape_component( cb_array,c_width );
        cr_coef=eu_reshape_component( cr_array,c_width );
        load('obj.mat','jobj_422');
        jobj=jobj_422;
    case '4:2:0'
        y_coef=[];
        residue=y_array;
        width=ceil(width/16)*16;
        for i=1:size(residue,2)/(2*width)
            coef=[];
            temp=residue(:,1:2*width);
            for j=1:size(temp,2)/32
                coef=[coef temp(:,16*(j-1)+1:16*j)];
                temp(:,16*(j-1)+1:16*j)=[];
            end
            coef=[coef;temp];
            
            y_coef=[y_coef;coef];
            residue(:,1:2*width)=[];
        end
        c_width=width/2;
        cb_coef=eu_reshape_component( cb_array,c_width );
        cr_coef=eu_reshape_component( cr_array,c_width );
        load('obj.mat','jobj_420');
        jobj=jobj_420;
        
    case '4:4:4'
        y_coef=eu_reshape_component( y_array,width );
        cb_coef=eu_reshape_component( cb_array,width );
        cr_coef=eu_reshape_component( cr_array,width );
        load('obj.mat','jobj_444');
        jobj=jobj_444;
    otherwise
        display('unresolved chroma sub-sampling');
        
end

coef_arrays={y_coef,cb_coef,cr_coef};
jobj.coef_arrays=coef_arrays;
jobj.image_height=size(y_coef,1);
jobj.image_width=width;

rgb=im2double(eu_ycbcr2rgb(jobj));
% rgb=eu_ycbcr2rgb(jobj);
end

function [comp_out] = eu_calibrate_dc( comp_in )
dc_in=[];
for i=1:size(comp_in,1)/8
    dc_in=[dc_in comp_in((i-1)*8+1,1:8:end)];
end
dc_mean=round((max(dc_in)+min(dc_in))/2);
dc_diff=dc_in-dc_in(1);
for i=1:size(comp_in,1)/8
    comp_in((i-1)*8+1,1:8:end)=dc_diff(1:size(comp_in,2)/8)-dc_mean;
    dc_diff(1:size(comp_in,2)/8)=[];
end
% comp_out=[min(dc_diff) max(dc_diff)];
comp_out=comp_in;
end

function RGB=eu_ycbcr2rgb(jobj)
dY=eu_idct_comp(jobj.coef_arrays{1,1},jobj.quant_tables{1,1});
dCb=eu_idct_comp(jobj.coef_arrays{1,2},jobj.quant_tables{1,2});
dCr=eu_idct_comp(jobj.coef_arrays{1,3},jobj.quant_tables{1,2});

%---------------UPSAMPLING-------------------------------------------------
switch jobj.chr_sub
    case 1  %4:2:2
        fun=@(block_struct) [block_struct.data block_struct.data];
    case 2  %4:4:0
        fun=@(block_struct) [block_struct.data;block_struct.data];
    case 3  %4:2:0
        fun=@(block_struct) [block_struct.data block_struct.data;block_struct.data block_struct.data];
    otherwise
end

if jobj.chr_sub~=4
    uCb=blockproc(dCb,[8 8],fun);
    uCr=blockproc(dCr,[8 8],fun);
else
    uCb=dCb;
    uCr=dCr;
end

YCC=zeros(size(dY,1),size(dY,2),3);
YCC(:,:,1)=dY;
YCC(:,:,2)=uCb;
YCC(:,:,3)=uCr;

RGB=ycbcr2rgb(uint8(YCC));
% RGB=uint8(YCC);

end

function dX=eu_idct_comp(x,qt)
% dX=x;
x_q=repmat(qt,[size(x,1)/8 size(x,2)/8]);
x = x.*x_q;
t=dctmtx(8);
fun=@(block_struct) t'*block_struct.data*t;
x = blockproc(x,[8 8],fun);
dX = uint8(x + 128);
end

function varargout = peakfinder(x0, sel, thresh, extrema, include_endpoints)
%PEAKFINDER Noise tolerant fast peak finding algorithm
%   INPUTS:
%       x0 - A real vector from the maxima will be found (required)
%       sel - The amount above surrounding data for a peak to be
%           identified (default = (max(x0)-min(x0))/4). Larger values mean
%           the algorithm is more selective in finding peaks.
%       thresh - A threshold value which peaks must be larger than to be
%           maxima or smaller than to be minima.
%       extrema - 1 if maxima are desired, -1 if minima are desired
%           (default = maxima, 1)
%       include_endpoints - If true the endpoints will be included as
%           possible extrema otherwise they will not be included
%           (default = true)
%   OUTPUTS:
%       peakLoc - The indicies of the identified peaks in x0
%       peakMag - The magnitude of the identified peaks
%
%   [peakLoc] = peakfinder(x0) returns the indicies of local maxima that
%       are at least 1/4 the range of the data above surrounding data.
%
%   [peakLoc] = peakfinder(x0,sel) returns the indicies of local maxima
%       that are at least sel above surrounding data.
%
%   [peakLoc] = peakfinder(x0,sel,thresh) returns the indicies of local
%       maxima that are at least sel above surrounding data and larger
%       (smaller) than thresh if you are finding maxima (minima).
%
%   [peakLoc] = peakfinder(x0,sel,thresh,extrema) returns the maxima of the
%       data if extrema > 0 and the minima of the data if extrema < 0
%
%   [peakLoc, peakMag] = peakfinder(x0,...) returns the indicies of the
%       local maxima as well as the magnitudes of those maxima
%
%   If called with no output the identified maxima will be plotted along
%       with the input data.
%
%   Note: If repeated values are found the first is identified as the peak
%
% Ex:
% t = 0:.0001:10;
% x = 12*sin(10*2*pi*t)-3*sin(.1*2*pi*t)+randn(1,numel(t));
% x(1250:1255) = max(x);
% peakfinder(x)
%
% Copyright Nathanael C. Yoder 2011 (nyoder@gmail.com)

% Perform error checking and set defaults if not passed in
error(nargchk(1,5,nargin,'struct'));
error(nargoutchk(0,2,nargout,'struct'));

s = size(x0);
flipData =  s(1) < s(2);
len0 = numel(x0);
if len0 ~= s(1) && len0 ~= s(2)
    error('PEAKFINDER:Input','The input data must be a vector')
elseif isempty(x0)
    varargout = {[],[]};
    return;
end
if ~isreal(x0)
    warning('PEAKFINDER:NotReal','Absolute value of data will be used')
    x0 = abs(x0);
end

if nargin < 2 || isempty(sel)
    sel = (max(x0)-min(x0))/4;
elseif ~isnumeric(sel) || ~isreal(sel)
    sel = (max(x0)-min(x0))/4;
    warning('PEAKFINDER:InvalidSel',...
        'The selectivity must be a real scalar.  A selectivity of %.4g will be used',sel)
elseif numel(sel) > 1
    warning('PEAKFINDER:InvalidSel',...
        'The selectivity must be a scalar.  The first selectivity value in the vector will be used.')
    sel = sel(1);
end

if nargin < 3 || isempty(thresh)
    thresh = [];
elseif ~isnumeric(thresh) || ~isreal(thresh)
    thresh = [];
    warning('PEAKFINDER:InvalidThreshold',...
        'The threshold must be a real scalar. No threshold will be used.')
elseif numel(thresh) > 1
    thresh = thresh(1);
    warning('PEAKFINDER:InvalidThreshold',...
        'The threshold must be a scalar.  The first threshold value in the vector will be used.')
end

if nargin < 4 || isempty(extrema)
    extrema = 1;
else
    extrema = sign(extrema(1)); % Should only be 1 or -1 but make sure
    if extrema == 0
        error('PEAKFINDER:ZeroMaxima','Either 1 (for maxima) or -1 (for minima) must be input for extrema');
    end
end

if nargin < 5 || isempty(include_endpoints)
    include_endpoints = true;
else
    include_endpoints = boolean(include_endpoints);
end

x0 = extrema*x0(:); % Make it so we are finding maxima regardless
thresh = thresh*extrema; % Adjust threshold according to extrema.
dx0 = diff(x0); % Find derivative
dx0(dx0 == 0) = -eps; % This is so we find the first of repeated values
ind = find(dx0(1:end-1).*dx0(2:end) < 0)+1; % Find where the derivative changes sign

% Include endpoints in potential peaks and valleys is desired
if include_endpoints
    x = [x0(1);x0(ind);x0(end)];
    ind = [1;ind;len0];
else
    x = x0(ind);
end

% x only has the peaks, valleys, and possibly endpoints
len = numel(x);
minMag = min(x);


if len > 2 % Function with peaks and valleys
    
    % Set initial parameters for loop
    tempMag = minMag;
    foundPeak = false;
    leftMin = minMag;
    
    if include_endpoints
        % Deal with first point a little differently since tacked it on
        % Calculate the sign of the derivative since we taked the first point
        %  on it does not neccessarily alternate like the rest.
        signDx = sign(diff(x(1:3)));
        if signDx(1) <= 0 % The first point is larger or equal to the second
            if signDx(1) == signDx(2) % Want alternating signs
                x(2) = [];
                ind(2) = [];
                len = len-1;
            end
        else % First point is smaller than the second
            if signDx(1) == signDx(2) % Want alternating signs
                x(1) = [];
                ind(1) = [];
                len = len-1;
            end
        end
    end
    
    % Skip the first point if it is smaller so we always start on a
    %   maxima
    if x(1) > x(2)
        ii = 0;
    else
        ii = 1;
    end
    
    % Preallocate max number of maxima
    maxPeaks = ceil(len/2);
    peakLoc = zeros(maxPeaks,1);
    peakMag = zeros(maxPeaks,1);
    cInd = 1;
    % Loop through extrema which should be peaks and then valleys
    while ii < len
        ii = ii+1; % This is a peak
        % Reset peak finding if we had a peak and the next peak is bigger
        %   than the last or the left min was small enough to reset.
        if foundPeak
            tempMag = minMag;
            foundPeak = false;
        end
        
        % Make sure we don't iterate past the length of our vector
        if ii == len
            break; % We assign the last point differently out of the loop
        end
        
        % Found new peak that was lager than temp mag and selectivity larger
        %   than the minimum to its left.
        if x(ii) > tempMag && x(ii) > leftMin + sel
            tempLoc = ii;
            tempMag = x(ii);
        end
        
        ii = ii+1; % Move onto the valley
        % Come down at least sel from peak
        if ~foundPeak && tempMag > sel + x(ii)
            foundPeak = true; % We have found a peak
            leftMin = x(ii);
            peakLoc(cInd) = tempLoc; % Add peak to index
            peakMag(cInd) = tempMag;
            cInd = cInd+1;
        elseif x(ii) < leftMin % New left minima
            leftMin = x(ii);
        end
    end
    
    % Check end point
    if x(end) > tempMag && x(end) > leftMin + sel
        peakLoc(cInd) = len;
        peakMag(cInd) = x(end);
        cInd = cInd + 1;
    elseif ~foundPeak && tempMag > minMag % Check if we still need to add the last point
        peakLoc(cInd) = tempLoc;
        peakMag(cInd) = tempMag;
        cInd = cInd + 1;
    end
    
    % Create output
    peakInds = ind(peakLoc(1:cInd-1));
    peakMags = peakMag(1:cInd-1);
else % This is a monotone function where an endpoint is the only peak
    [peakMags,xInd] = max(x);
    if peakMags > minMag + sel
        peakInds = ind(xInd);
    else
        peakMags = [];
        peakInds = [];
    end
end

% Apply threshold value.  Since always finding maxima it will always be
%   larger than the thresh.
if ~isempty(thresh)
    m = peakMags>thresh;
    peakInds = peakInds(m);
    peakMags = peakMags(m);
end

% Rotate data if needed
if flipData
    peakMags = peakMags.';
    peakInds = peakInds.';
end

% Change sign of data if was finding minima
if extrema < 0
    peakMags = -peakMags;
    x0 = -x0;
end

% Plot if no output desired
if nargout == 0
    if isempty(peakInds)
        disp('No significant peaks found')
    else
        figure;
        plot(1:len0,x0,'.-',peakInds,peakMags,'ro','linewidth',2);
    end
else
    varargout = {peakInds,peakMags};
end

end

