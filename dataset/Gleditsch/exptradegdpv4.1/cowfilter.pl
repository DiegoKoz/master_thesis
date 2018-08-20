# 14.04.2001 Kristian Skrede Gleditsch kgleditsch@ucsd.edu
#
# This file limits the data to observations in COW system membership list
# 
# It creates two new _cow files limited to COW observations
#
# Revised on 8 October 2002 for version 2.1 of data
# Revised on 22 September 2003 for version 3.0 of data
#   and to conform with latest version of COW system
#   membership data
# Revised on 13 May 2004 for version 4.0 of data 
###################################

open(IN,"trade_udd.asc") || die;
open(OUT,">uddtrade_cow.asc") || die;

while(<IN>){
    
    # Consistent label substituions
    $_ =~ s/RUM 360/ROM 360/g;    #Substitute globally (g)
    $_ =~ s/FJI 950/FIJ 950/g;    #Substitute globally (g)
    $_ =~ s/YEM 678/YEM 679/g;    #Substitute globally (g)

    $info = $_;
    chop($info); 
    ($acra,$numa,$acrb,$numb,$year,$expab,$eabo,$impab,$iabo,
	$expba,$ebao,$impba,$ibao) = split(/\s+/,$_);
    $valid = 'T';

    # States that are never in COW
    if($acra eq "KBI" | $acrb eq 'KBI'){$valid = 'F';}
    elsif($acra eq "NAU" | $acrb eq 'NAU'){$valid = 'F';}
    elsif($acra eq "TBT" | $acrb eq 'TBT'){$valid = 'F';}
    elsif($acra eq "TON" | $acrb eq 'TON'){$valid = 'F';}
    elsif($acra eq "TUV" | $acrb eq 'TUV'){$valid = 'F';}

    # Conditional exclusions

    # 221   MNC only in COW from 05/28/1993
    elsif(($acra eq "MNC" | $acrb eq 'MNC') & $year<1993){$valid = 'F';}

    # 223 LIE Only in COW from 09/18/1990 
    elsif(($acra eq "LIE" | $acrb eq 'LIE') & $year<1990){$valid = 'F';}

    # 232 AND only in COW from 07/28/1993 
    elsif(($acra eq "AND" | $acrb eq 'AND') & $year<1993){$valid = 'F';}

    # 260 GFR Only in COW from 05/05/1955
    elsif(($acra eq "GFR" | $acrb eq 'GFR') & $year<1955){$valid = 'F';}

    # 265 GDR only in COW from 03/25/1954
    elsif(($acra eq "GDR" | $acrb eq 'GDR') & $year<1954){$valid = 'F';}

    # 305 AUS only in COW from 07/27/1955 
    elsif(($acra eq "AUS" | $acrb eq 'AUS') & $year<1955){$valid = 'F';}

    # 331 SNM Only in COW from 03/02/1992 
    elsif(($acra eq "SNM" | $acrb eq 'SNM') & $year<1992){$valid = 'F';}

    # 343 MAC Only in COW from 04/08/1993 
    elsif(($acra eq "MAC" | $acrb eq 'MAC') & $year<1993){$valid = 'F';}

    # 344 CRO Only in COW from 01/15/1992 
    elsif(($acra eq "CRO" | $acrb eq 'CRO') & $year<1992){$valid = 'F';}

    # 349 SLV Only in COW from 01/15/1992 
    elsif(($acra eq "SLV" | $acrb eq 'SLV') & $year<1992){$valid = 'F';}

    # 88 411 EQG Only in COW from 10/12/1968 
    elsif(($acra eq "EQG" | $acrb eq 'EQG') & $year<1968){$valid = 'F';}

    # 143 660 LEB Only in COW from 03/10/1946 
    elsif(($acra eq "LEB" | $acrb eq 'LEB') & $year<1946){$valid = 'F';}

    # 154 698 OMA Only in COW from 10/07/1971 
    elsif(($acra eq "OMA" | $acrb eq 'OMA') & $year<1971){$valid = 'F';}

    # 166 732 ROK Only in COW from 06/29/1949 
    elsif(($acra eq "ROK" | $acrb eq 'ROK') & $year<1949){$valid = 'F';}

    # 167 740 JPN Only in COW from 04/28/1952 
    elsif(($acra eq "JPN" | $acrb eq 'JPN') & $year<1952){$valid = 'F';}

    # 169 760 BHU Only in COW from 09/21/1971 
    elsif(($acra eq "BHU" | $acrb eq 'BHU') & $year<1971){$valid = 'F';}

    # 185 850 INS Only in COW from 12/27/1949 
    elsif(($acra eq "INS" | $acrb eq 'INS') & $year<1949){$valid = 'F';}

    # 189 935 VAN only in COW from 09/15/1981 
    elsif(($acra eq "VAN" | $acrb eq 'VAN') & $year<1981){$valid = 'F';}

    # 196 983 MSI Only in COW from 09/17/1991 
    elsif(($acra eq "MSI" | $acrb eq 'MSI') & $year<1991){$valid = 'F';}

    # 198 987 FSM Only in COW from 09/17/1991 
    elsif(($acra eq "FSM" | $acrb eq 'FSM') & $year<1971){$valid = 'F';}

    # 199 990 WSM only in COW from 12/15/1976
    elsif(($acra eq "WSM" | $acrb eq 'WSM') & $year<1976){$valid = 'F';}

    # A COW oddity
    # 652 Syria dissapears for years 1959 and 1960  
    elsif(($acra eq "SYR" | $acrb eq "SYR") & $year>=1959 & $year<=1960){$valid = 'F';}


    # A conditional rename
    # Beginning 1991, use 255 GMY instead of 260 GFR label 
    if($acra eq "GFR" & $year>=1991){
	$acra = "GMY";
	$numa = 255;}

    if($acrb eq "GFR" & $year>=1991){
	$acrb = "GMY";
	$numb = 255;}

    # Before 1991, use 678 YAR instead of 679 YEM  
    if($acra eq "YEM" & $year<1991){
	$acra = "YAR";
	$numa = 678;}

    if($acrb eq "YEM" & $year<1991){
	$acrb = "YAR";
	$numb = 678;}

 
    # Print out all observations deemed valid
    if($valid eq 'T'){
	print OUT join(" ", ($acra,$numa,$acrb,$numb,$year,
	$expab,$eabo,$impab,$iabo,$expba,$ebao,$impba,$ibao))."\n";
    }
}
close(OUT);
close(IN);


# Fit the GDP data to the COW conversion

open(IN,"pwt_v61.asc") || die;
open(OUT,">pwt_cow.asc") || die;

while(<IN>){
    
    # Consistent label substituions
    # Consistent label substituions

    $_ =~ s/360 RUM/360 360/g;    #Substitute globally (g)
    $_ =~ s/678 YEM/679 YEM/g;    #Substitute globally (g)
    $_ =~ s/950 FIJ/950 FJI/g;    #Substitute globally (g)

    $info = $_;
    chop($info); 
    ($statenum,$stateid,$year,$pop,$rgdp96pc,$gdppc,$origin) = split(/\s+/,$_);
    $valid = 'T';

    # States that are never in COW
    if($stateid eq 'KBI'){$valid = 'F';}
    elsif($stateid eq 'NAU'){$valid = 'F';}
    elsif($stateid eq 'TBT'){$valid = 'F';}
    elsif($stateid eq 'TON'){$valid = 'F';}
    elsif($stateid eq 'TUV'){$valid = 'F';}

    # Conditional exclusions

    # 221   MNC only in COW from 05/28/1993
    elsif($stateid eq 'MNC' & $year<1993){$valid = 'F';}

    # 223 LIE Only in COW from 09/18/1990 
    elsif($stateid eq 'LIE' & $year<1990){$valid = 'F';}

    # 232 AND only in COW from 07/28/1993 
    elsif($stateid eq 'AND' & $year<1993){$valid = 'F';}

    # 260 GFR Only in COW from 05/05/1955
    elsif($stateid eq 'GFR' & $year<1955){$valid = 'F';}

    # 265 GDR only in COW from 03/25/1954
    elsif($stateid eq 'GDR' & $year<1954){$valid = 'F';}

    # 305 AUS only in COW from 07/27/1955 
    elsif($stateid eq 'AUS' & $year<1955){$valid = 'F';}

    # 331 SNM Only in COW from 03/02/1992 
    elsif($stateid eq 'SNM' & $year<1992){$valid = 'F';}

    # 343 MAC Only in COW from 04/08/1993 
    elsif($stateid eq 'MAC' & $year<1993){$valid = 'F';}

    # 344 CRO Only in COW from 01/15/1992 
    elsif($stateid eq 'CRO' & $year<1992){$valid = 'F';}

    # 349 SLV Only in COW from 01/15/1992 
    elsif($stateid eq 'SLV' & $year<1992){$valid = 'F';}

    # 88 411 EQG Only in COW from 10/12/1968 
    elsif($stateid eq 'EQG' & $year<1968){$valid = 'F';}

    # 143 660 LEB Only in COW from 03/10/1946 
    elsif($stateid eq 'LEB' & $year<1946){$valid = 'F';}

    # 154 698 OMA Only in COW from 10/07/1971 
    elsif($stateid eq 'OMA' & $year<1971){$valid = 'F';}

    # 166 732 ROK Only in COW from 06/29/1949 
    elsif($stateid eq 'ROK' & $year<1949){$valid = 'F';}

    # 167 740 JPN Only in COW from 04/28/1952 
    elsif($stateid eq 'JPN' & $year<1952){$valid = 'F';}

    # 169 760 BHU Only in COW from 09/21/1971 
    elsif($stateid eq 'BHU' & $year<1971){$valid = 'F';}

    # 185 850 INS Only in COW from 12/27/1949 
    elsif($stateid eq 'INS' & $year<1949){$valid = 'F';}

    # 189 935 VAN only in COW from 09/15/1981 
    elsif($stateid eq 'VAN' & $year<1981){$valid = 'F';}

    # 196 983 MSI Only in COW from 09/17/1991 
    elsif($stateid eq 'MSI' & $year<1991){$valid = 'F';}

    # 198 987 FSM Only in COW from 09/17/1991 
    elsif($stateid eq 'FSM' & $year<1971){$valid = 'F';}

    # 199 990 WSM only in COW from 12/15/1976
    elsif($stateid eq 'WSM' & $year<1976){$valid = 'F';}

    # A COW oddity
    # 652 Syria dissapears for years 1959 and 1960  
    elsif($stateid eq "SYR" & $year>=1959 & $year<=1960){$valid = 'F';}


    # A conditional rename
    # Beginning 1991, use 255 GMY instead of 260 GFR label 
    if($stateid eq "GFR" & $year>=1991){
	$stateid = "GMY";
	$statenum = 255;}

    # Before 1991, replace 679 YEM with 678 YAR label 
    if($stateid eq "YEM" & $year<1991){
	$stateid = "YAR";
	$statenum = 678;}


 
    # Print out all observations deemed valid
    if($valid eq 'T'){
	print OUT join(" ",($statenum,$stateid,$year,
	      $pop,$rgdp96pc,$gdppc,$origin))."\n";
    }
}






