# RISingVIS <img src='man/figures/logo.png' align="right" height="250" />

The R package <b>RISingVIS</b> was developed to (1) <i>identify</i>, (2) <i>analyze</i>, 
through the use of different functions that compute statistics to assess 
and quantify their presence, and (3) <i>remove</i> index switching events 
in vector integration sites, that can be misleading for further analysis.

In the field of gene therapy <i>vector integration sites</i> (IS) are used as 
molecular barcodes for clonal tracking in vivo. IS analysis is required 
to assess the safety and efficacy of the treatment, as well for tracking 
studies in which they are fundamental to make various calculations. 
These IS studies rely on high-throughput sequencing of DNA fragments and 
they can be affected by the phenomenon called <b>Index Switching</b>: a molecular 
“error” during the sequencing process that may cause mis-assignment 
of a fragment to the wrong sample and thus creating false positives. 
