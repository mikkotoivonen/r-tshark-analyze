# r-tshark-analyze
Analyzing Tshark output using R

Output from tshark with command:
sudo tshark -i wlan0 subtype probereq -c 60 -n -T fields -e frame.time -e wlan.sa -e wlan_mgt.ssid -e radiotap.dbm_antsignal -E separator=, -E quote=d


Change the command line arguments to suit your needs (for example the interface). The R script is for analyzing WLAN activity from wlan0.

The R script needs to be modified to point to the correct file (variable called URL).
Also, if you wish to highlight some know mac addresses, be sure to populate the "knownmacs" data frame.

Further, the mactomanufacture.txt file may need periodic updating (you can find it here: https://code.wireshark.org/review/gitweb?p=wireshark.git;a=blob_plain;f=manuf). 
