# jpeg-carver-matlab

* This repo includes matlab toolbox for the orphaned jpeg fragment recovery method presented in following paper.
* This is a pure Matlab implemented code.
* **WARNING**: Do NOT remove dht.mat and obj.mat files from the directory has eu_ofd.p file. 

## Usage
* Example Usage: rgb=eu_ofd(fragment_string);
* Input: fragment_string -> Binary or Hexadecimal String
* Binary: 101101010101010....
* Hexadecimal: e2 94 af dd ca 95 ...
* Output: rgb -> uint8 rgb matrix
* **NOTE 1**: dht.mat includes recommended Huffman table set key,value pairs. You can modify it accordingly for different Huffman settings.
* Example: Current mappings for Y DC component.
* dht.h1.y_dc_map.keys 	 -> '00'    '010'    '011'    '100'    '101'    '110'    '1110' ...
* dht.h1.y_dc_map.values -> '00'    '01'    '02'    '03'    '04'    '05'    '06' ...
* **NOTE 2**: example.m includes a sample recovery. When you directly run example.m, fragment_string in test_hex_data.txt will be recovered and return test_hex_data.jpg RGB image.
* **NOTE 3**: [PeakFinder function](https://www.mathworks.com/matlabcentral/fileexchange/25500-peakfinder-x0--sel--thresh--extrema--includeendpoints--interpolate-) by Nathanael Yoder has been adapted as part of some functions.

## Citation
* Please cite to following paper(s) if you use these tools for academic purpose;

* E. Uzun and H. T. Sencar, “[Carving orphaned jpeg file fragments](https://www.researchgate.net/publication/275044127_Carving_Orphaned_JPEG_File_Fragments)”, IEEETransactions on Information Forensics and Security, vol. 10, no. 8, pp.1549–1563, 2015.
