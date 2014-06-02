\usepackage{times}
 
This code formats a vcf file to plink format. The input is the 
vcf file and a ile with the pedigree information.

\section{VCF file description}

\subsection{Meta information lines}
The lines of the form 
\#\# string 
where string is usually of the form key=value pair
The line is ##fileformat=VCFv4.0 where VCFv is the vcf version
is required.

\subsection{header line syntax}
The header line names the 8 fixed, mandatory columns. These columns are as follows:

    #CHROM
    POS
    ID
    REF
    ALT
    QUAL
    FILTER
    INFO

If genotype data is present in the file, these are followed by a FORMAT column header, then an arbitrary number of sample IDs. The header line is tab-delimited.
\subsection{data lines}


 
\begin{code}
module VcfToPlink

where

import qualified Data.ByteString.Lazy.Char8 as BS

--The split lines are lists of ByteStrings
type SplitLine = [BS.ByteString]
type Line = BS.ByteString
type Lines = [Line]

data FileType = FileType { metaData :: Lines,
                           header :: Line,
                           content :: Lines
                         }
                
              

-- Get field this covers the fact that haskell
-- lists are 0 based and provides some abstraction  
getField :: Int -> SplitLine -> BS.ByteString
getField n l = l !! (n-1)
 
-- Returns reference allele string
getRef :: SplitLine -> BS.ByteString
getRef = getField 4

-- Returns a list of alternative alleles
getAltList :: SplitLine -> [BS.ByteString]
getAltList l = BS.split ',' altField 
           where altField = getField 5 l

-- Return format string
getFormatString :: SplitLine -> BS.ByteString
getFormatString = getField 9


-- Return delimiter
getFormatDelim :: BS.ByteString -> Char
getFormatDelim _ = ':'

-- Return GT position in format string 
getGenotypePosition :: BS.ByteString -> Int
getGenotypePosition _ = 1
 

-- Map "./." or ".|." to (BS.ByteString,BS.ByteString)
vcfStringToTuple :: BS.ByteString -> [BS.ByteString]
vcfStringToTuple s 
                 | BS.elem '|' s = BS.split '|' s
                 | BS.elem '/' s = BS.split '/' s
                 | otherwise = undefined

-- Tuple to genotype string
tupleToGenotypeString :: [BS.ByteString] -> [BS.ByteString] -> BS.ByteString 
tupleToGenotypeString lst t
                      | BS.singleton '.' `elem` t = BS.pack "0 0"
                      | otherwise = BS.intercalate (BS.pack " ")$ map (lst !!) tInt               
                       where tInt = map getInt t ::[Int]

getInt :: BS.ByteString -> Int
getInt l= let res=(fmap fst $ BS.readInt l) in case res of
          Just x ->  x  
          Nothing  -> undefined  

-- Get genotype list 0 element is the refence allele and 
-- the alternative alleles are indexed by 1 and above in
-- the order they appear.
getGenotypeLst::SplitLine -> [BS.ByteString]
getGenotypeLst l = getRef l:getAltList l

-- splitLine --
splitLine:: BS.ByteString -> SplitLine 
splitLine = BS.split '\t'


-- getFile -- 
getFile :: BS.ByteString -> FileType
getFile l =   FileType { metaData = meta,
                         header = head rest,
                         content = tail rest 
                       } where contLines = BS.lines l
                               (meta, rest) = span (BS.isPrefixOf$BS.pack "##") contLines

-- toOutputString
toOutputString :: SplitLine -> Line                                
toOutputString l =  BS.intercalate (BS.pack "\t") $! [chrom, snpid, BS.pack "0", pos] ++ map mapf genoStrings
                   where
                       genolst = getGenotypeLst l
                       mapf = tupleToGenotypeString genolst . vcfStringToTuple . toVcfGenotypeString
                       (prefix, rest) = splitAt 3 l
                       chrom = head prefix
                       pos = (head.tail) prefix
                       snpid = (head . tail .tail) prefix 
                       genoStrings = drop 6 rest

toVcfGenotypeString :: BS.ByteString -> BS.ByteString
toVcfGenotypeString l = head $ BS.split ':'  l

\end{code}


