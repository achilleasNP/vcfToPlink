vcfToPlink
==========

Script to transform vcf genotypes to a tped plink file

Usage
-----

    vcfToPlink file.vcf pedigreeFile plink
    
file.vcf is the vcf file,  pedigree file is a csv with a header and columns
FID, IID, FA, MO, SEX, STATUS where

* FID is the family id
* IID is the individual id
* FA is IID of the Father,  0 for unknown
* MO is the IID of the Mother 0 for unknown
* SEX 1 for Males and 2 for females 
* STATUS 0 or 1, -9 for missing
