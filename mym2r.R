# purpose: general purpose functions to communicate between MyM and R
# author: David L. Bijl: d.l.bijl@uu.nl or dlbijl@gmail.com

library(plyr)
library(stringr)
library(reshape2)

# READ THIS:
# In general, use read.mym2r.nice because it adds text labels to the data, e.g. regions and techs
# if you want the data without labels, or you don't know the dimensions of the file, use read.mym2r
# read.mym2r.many also gives data without labels, but puts all the dataframes in a list
# the r2mym functions are at the end of this file.

read.mym2r.dims = function(path.to.mym.file){
  # utility function to extract only the dimensions from the header in a mym file
  # input:  path to MyM output file
  # output: vector of dimensions as listed in the header / first line of the mym file
  
  # skip any commented lines at the start
  con = file(path.to.mym.file, open = 'r')
  oneline = readLines(con, n = 1)
  while (grepl(pattern='[:blank:]*[!]',oneline))
    oneline = readLines(con, n = 1)  # read next line
  close(con)
  
  # extract the dimensions
  dims = numeric(0)
  if (grepl(pattern='[[]+.+[]]+',oneline)){
    dimstring = substr(x=oneline, start=regexpr('[',oneline,fixed=TRUE)[1]+1, stop=regexpr(']',oneline,fixed=TRUE)[1]-1)
    dims      = as.numeric(strsplit(dimstring,',')[[1]])
  }
  return(dims)
}

read.mym2r.varname = function(path.to.mym.file){
  # utility function to extract only the varname from the header in a mym file
  # input:  path to MyM output file
  # output: vector of dimensions as listed in the header / first line of the mym file
  
  # skip any commented lines at the start
  con = file(path.to.mym.file, open = 'r')
  oneline = readLines(con, n = 1)
  while (grepl(pattern='[:blank:]*[!]',oneline))
    oneline = readLines(con, n = 1)  # read next line
  close(con)
  
  varname = NA
  # extract variable name used in MyM
  if (grepl(pattern='\\s.+[[(]',oneline)) 
    varname = substr(x=oneline, start=regexpr('\\s\\S+[[(]',oneline)[1]+1, stop=regexpr('[[(]',oneline)[1]-1)
  return(varname)
}

read.mym2r = function(path.to.mym.file, yearheader='year', yearsrun=NULL){
  # input:  path to MyM output file
  # output: R dataframe in 'long' format (many rows, each dimension as a 'factor' in its column, and only one column with actual values)
  
  varname = read.mym2r.varname(path.to.mym.file)
  dims    = read.mym2r.dims(path.to.mym.file)
  
  # read all the data
  line      = readLines(path.to.mym.file, warn = FALSE)
  
  # skip any commented lines at the start
  while (1 < length(line)){
    if (grepl(pattern='[:blank:]*[!]',line[1])){
      line = line[2:length(line)]   # this might not be very efficient but it is clear: we discard the first line if it is only a comment
    } else {
      break
    }
  }
  
  # extract whether there is a time dimension or not
  has.time.dim = grepl(pattern='(t)',line[1],fixed=TRUE)
  
  # convert all lines to one long vector of numbers, including years
  string = paste0(line, collapse = ',')
  string = strsplit(string, split='=', fixed=TRUE) # now a list of 1 header and 1 content string
  content = string[[1]][2]
  # remove [ ] ;
  content = gsub('[','',content, fixed=TRUE)  
  content = gsub(']','',content, fixed=TRUE)  
  content = gsub(';','',content, fixed=TRUE)  
  # convert to vector of values
  content = strsplit(content, '[[:space:],]')[[1]]
  content = content[content != '']  # remove empty strings
  content = as.numeric(content)
  
  # create a dataframe that exactly matches the format of the MyM file
  ret = expand.grid( lapply(rev(dims), FUN=function(x){return(1:x)}) )   # note: last MyM iterator comes first with rev(dims)
  if (length(dims)>=1) 
    dim.dummies = paste0('dim',rev(1:length(dims)))                          # here again we must reverse, to stay consistent with dim1 = first one listed in MyM args
  
  if (has.time.dim && length(dims)>=1){
    
    # add one column of numbers for each year
    ncols  = length(content)/(nrow(ret)+1)   # +1 for the year numbers themselves
    m      = matrix(content, nrow=nrow(ret)+1, ncol=ncols)
    years  = m[1,]
    values = m[2:nrow(m),]
    ret    = cbind(ret, values)
    
    colnames(ret) = c(dim.dummies,years)
    
    ret = reshape2::melt(ret, id.vars=dim.dummies, value.name = 'value', variable.name = yearheader)      # not needed if you wanted wide format output, but default is long format
    ret = ret[c((length(dims)+1):1, ncol(ret))]                                                           # rearrange columns to match the order in MyM
    
  } else if (has.time.dim && length(dims)==0){
    
    m   = matrix(content, ncol = 2, byrow = TRUE)
    ret = data.frame(m)
    colnames(ret) = c(yearheader,'value')
    
  } else {  # timeless
    
    ret$value     = content                                                # join the values from the MyM file to the prepared dataframe
    colnames(ret) = c(dim.dummies,'value')            
    ret           = ret[c(length(dims):1, ncol(ret))]                  # rearrange columns to match the order in MyM
    
  }
  
  # additional info:
  ret$varname = varname
  if (has.time.dim) ret[yearheader] = as.numeric(as.character(ret[[yearheader]]))  # make sure years are numeric
  return(ret)
}

read.mym2r.many = function(filenames=c(), path.to.folder='', yearheader='year', yearsrun=1971:2100){
  # purpose: read many MyM files at once
  # input:
  #   filenames are either variable names or full file names (.out extension is added if necessary) that you want to read
  #   path.to.folder is the full path to the folder where the .out files are. A slash is added if omitted. 
  # note:   extension '.out' is added if omitted
  # output: list containing one named dataframe for each variable/file read. returns nothing if something went wrong
  
  if (!is.null(path.to.folder)){
    results = list()
    
    if (substr(path.to.folder, nchar(path.to.folder), nchar(path.to.folder))!='/')
      path.to.folder = paste0(path.to.folder,'/')
    
    for (filename in filenames){
      cat('reading ',filename,'\n')
      
      if (substr(filename, nchar(filename)-3, nchar(filename))!='.out'){
        path.to.mym.file    = paste0(path.to.folder,filename,'.out')
      } else {
        path.to.mym.file    = paste0(path.to.folder,filename)
      }
      
      results[[filename]] = read.mym2r(path.to.mym.file, yearheader=yearheader, yearsrun=yearsrun)      # store the returned dataframe in a list for future reference
    }
    return(results)  
  } else {
    print('Error in read.mym2r: must supply path.to.folder')    
  }
}

lookup.mym.dimlabels = function(varname){
  # retrieve default collist and namecols arguments, based on varname.
  
  regions28 = c("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF","WEU","CEU","TUR","UKR","STAN","RUS","ME","INDIA","KOR","CHN","SEAS","INDO","JAP","OCE","RSAS","RSAF","dummy","TOTAL")
  sectors   = c('Elec','Ind','Muni','Total non-agri')
  
  # please keep this list in alphabetical order!
  # do include the submodel e.g. main.em.h2o.etc.
  return( switch(varname, 
    'main.AllWCS'        = list(collist=list(regions28,sectors), namecols=c('region','sector')),
    'main.em.h2o.AllWCS' = list(collist=list(regions28,sectors), namecols=c('region','sector')),
    NULL ) )
}

read.mym2r.nice = function(mym.folder='', scen.econ='', filename=NULL, varname=NULL, collist=NULL, namecols=NULL, yearheader='year', yearsrun=1971:2100, novarname=FALSE) {
  # assumes you know the exact structure of the MyM file you are going to read
  # usage example:
  # mym.folder = "input/water model/mym output/"   # note: the exact file path would be: "input/water model/mym output/SSP2/ElecWWD_3_Int.out"
  # regions28  = c("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF","WEU","CEU","TUR","UKR","STAN","RUS","ME","INDIA","KOR","CHN","SEAS","INDO","JAP","OCE","RSAS","RSAF","dummy","TOTAL")
  # cooling5   = c("dry","once","sea","pond","tower")
  # minmedmax  = c('max','med','min')  # yes, in that reverse order
  # read.mym2r.nice(mym.folder = mym.folder, scen.econ = 'SSP2', filename = 'ElecWWD_3_Int', collist = list(regions28,1:29,cooling5,minmedmax), namecols = c('region','tech','cool','est'))
  
  # note: argument varname was replaced by filename as of 4 feb 2015. The actual variable name is extracted automatically by read.mym2r. The following line ensures backwards compatibility:
  if (is.null(filename) && !is.null(varname)) filename = varname
  
  if (!is.null(filename))
  {
    # add slashes if necessary
    if (mym.folder!='' && substr(mym.folder, start=nchar(mym.folder), stop=nchar(mym.folder))!='/')
      mym.folder = paste0(mym.folder,'/')
    if (scen.econ!='' && substr(scen.econ, start=nchar(scen.econ), stop=nchar(scen.econ))!='/')
      scen.econ = paste0(scen.econ,'/')
    
    # construct path.to.mym.file
    mypath = paste0(mym.folder, scen.econ, filename, '.out');
    if (!file.exists(mypath)) 
      mypath = paste0(mym.folder, scen.econ, filename, '.OUT');  # PWB: added check for upper/lowercase extension
    if (!file.exists(mypath)) 
      mypath = paste0(mym.folder, scen.econ, filename, '.dat');  # DB: or try .dat files
    if (!file.exists(mypath)) 
      mypath = paste0(mym.folder, scen.econ, filename, '.DAT');
    if (!file.exists(mypath)) 
      mypath = paste0(mym.folder, scen.econ, filename);          # DB: or maybe the extension was already part of the filename
    cat(sprintf("read.mym2r(): Using path: %s\n", mypath))
    
    # if no labels are supplied, look them up in the dictionary
    if (is.null(collist) || is.null(namecols)){
      varname  = read.mym2r.varname(path.to.mym.file = mypath)      
      lookedup = lookup.mym.dimlabels(varname = varname)
      if (!is.null(lookedup)){
        if (is.null(collist))
          collist = lookedup$collist
        if (is.null(namecols))
          namecols = lookedup$namecols        
      }
    }
    
    if (is.null(collist) || is.null(namecols)){
      print('Error in read.mym2r.nice: collist and/or namecols not supplied and could not be found in lookup.mym.dimlabels(). Returning result without labels')
      result = read.mym2r(path.to.mym.file=mypath, yearheader=yearheader, yearsrun=yearsrun)
      if (novarname){
        result$varname = NULL
      }
      return(result)
      
    } else {
      # check length of supplied labels
      dims = read.mym2r.dims(path.to.mym.file = mypath)
      mismatch = FALSE
      for (i in 1:length(dims)){
        if (length(collist[[i]]) != dims[i]){
          mismatch = TRUE
          print(paste0('Error in read.mym2r.nice: dimension listed in header (',dims[i],') does not match number of labels supplied in collist (',namecols[i],': ',length(collist[[i]]),')'))
        }
      }
      if (!mismatch){
        result = read.mym2r(path.to.mym.file=mypath, yearheader=yearheader, yearsrun=yearsrun)
        
        if (!is.null(collist) && !is.null(namecols) && length(collist)==length(namecols))  # skip this when you have a value per year, but no dims
        {
          for (d in 1:length(collist))  #d=1 d=2
          {
            dimcol           = paste0('dim',d)                                                              #the column of result as named by read.mym2r()
            result[[dimcol]] = factor(result[[dimcol]], levels=1:length(collist[[d]]), labels=collist[[d]]) #apply correct factors
            colnames(result) = gsub(dimcol, namecols[[d]], colnames(result), fixed=TRUE)                    #apply new name for a column
          }
        }
        
        if (novarname){
          result$varname = NULL
        }
        return(result)
      }
    }
    
  } else {
    print('Error in read.mym2r.nice: must supply filename')
  }
}

read.store.mym2r = function(run.scens, run.vars, mym.folder=project.folder, novarname=FALSE){
  # purpose: read data with read.mym2r.nice, then store it in nested list: data[[scenario]][[varname]] (so you can easily iterate through scenarios)
  # mym.folder = 'data/'  # relative path from working directory to where the scenario folders are
  # run.vars must be a list of instructions, like this:
  # run.vars = list(
  #   list(varname='AllWWD',   collist=list(NRCT,sectors4), namecols=list('region','sector'))
  #   , list(varname='AllWCS',   collist=list(NRCT,sectors4), namecols=list('region','sector'))
  # )
  data = list()
  for (s in run.scens){   #s=run.scens[1]
    for (v in run.vars){  #v=run.vars[[2]]
      varname = v[['varname']]
      cat('mym2r reading ',mym.folder,'  ',s,'  ',varname,'\n')
      data[[s]][[varname]] = read.mym2r.nice(mym.folder = mym.folder, scen.econ = s, filename = varname, collist = v[['collist']], namecols = v[['namecols']], novarname = novarname)
    }
  }  
  return(data)
}

prepare.r2mym <- function(data, stub, value.var, NaN.value = -9999) {
  # stub:      must have correct factor orderings, and correct order of columns. 
  #            year (if applicable) should be the first column, then the MyM dimensions.
  #            for example: stub = expand.grid(year=1970:2100,use=NUFPT,animal=NAPT,region=NR26T) 
  # data:      must contain at least the same columns as stub
  # value.var: string indicating the column containing the numeric values
  
  if (length(setdiff(colnames(stub),colnames(data))) > 0){
    cat('Error in prepare.r2mym: data must contain at least the same columns as stub\n')
    return(NULL)
  }
  
  data = data[c(colnames(stub),value.var)] # alleen relevante kolommen houden
  
  # stub factor levels defini??ren, anders blijven ze niet in de juiste volgorde maar worden alfabetisch (NB: alleen voor character kolommen, niet numeric)
  for (c in 1:ncol(stub)){
    if (!is.null(levels(stub[,c])))
      stub[,c] = factor(as.character(stub[,c]), levels=levels(stub[,c]), ordered = T)
  }
  
  # Data matchen met de stub / mal
  data = merge(stub, data, by=colnames(stub), all.x=TRUE)  # all.x=TRUE zorgt dat er NAs verschijnen waar jouw data ontbreekt
  
  # Replace NA, NaN, Inf, etc. by NaN.value, e.g. -9999
  data[!is.finite(unlist(data[value.var])), value.var] = NaN.value
  
  # First column should iterate the slowest, etc. Equivalent to doing: data = arrange(data, year, use, animal, region)
  data = data[do.call("order", data[colnames(stub)]), ]
  
  return(data)
}

write.r2mym <- function(data, outputfile, value.var, MyM.vartype, MyM.varname, time.dependent = TRUE, sep.years = ',', sep.data = ',\t', 
  matrix.format = TRUE, sprintf.format = NULL, comment.line = NULL, header.line = NULL) {
  # data:           should include only the data to be written, (no extra columns), e.g. as made with prepare.r2mym()
  # outputfile:     includes full path (if necessary) and file extension (e.g. ".dat")
  # value.var:      string indicating the column containing the numeric values
  # MyM.vartype:    the MyM variable type, e.g. 'REAL'
  # MyM.varname:    the full name for the variable in MyM, e.g. 'main.submodel.childunderweight_deaths'
  # time.dependent: whether the variable is time dependent and the first column of data indicates the year
  # sep.years:      what characters to print after each year. '\n' is added automatically.
  # sep.data:       what characters to print after each regular number.
  # matrix.format:  default output is all the numbers for year X on one row. This setting makes multiple rows, leaving only the last dimension on one row (e.g. region) (matters for IMAGE input files!) (makes it slow, though)
  # comment.line:   optional comment line to add at the top, crucial for IMAGE input files
  # header.line:    optional, specifiy the exact header line yourself (replaces MyM.vartype, MyM.varname) - beware: more scope for human error!
  # returns 0 when finished
  
  dimcols = setdiff(colnames(data),value.var)
  
  if (time.dependent){
    dimcols.noyr = dimcols[-1]  # the first one should always be the year
    time.text    = '(t)'
  } else {
    dimcols.noyr = dimcols
    time.text    = ''    
  }
  
  dim.noyr.sizes = unlist( lapply(X = data[dimcols.noyr], FUN = function(x){   #x=data[4]
    n = length( levels(unlist(x)) )
    if (n == 0)      # if there are no levels, it must be numeric
      n = length( unique(unlist(x)) );
    return(n)
  }) )
  
  if (time.dependent){
    dim.year.values = levels(unlist(data[1]))
    if (is.null(dim.year.values))
      dim.year.values = unique(unlist(data[1]))
    dim.year.size = length(dim.year.values)
  } else {
    dim.year.values = NULL
    dim.year.size = 1    # put 1 so the for-loop below runs only once
  }    
  
  if (is.null(header.line)){
    header = paste0(MyM.vartype,' ',MyM.varname,'[',paste(dim.noyr.sizes, collapse=','),']',time.text,' = [')
  } else {
    header = header.line
  }
  
  outvector = unlist( data[value.var] )
  
  nrow.for.1.year = prod(dim.noyr.sizes)
  
  cat('', file = outputfile)                          # if anything was there, delete it
  
  if (!is.null(comment.line)){
    cat(comment.line, '\n', file = outputfile, append = TRUE)     # optional comment line
  }
  
  cat(header, '\n', file = outputfile, append = TRUE)
  
  # bij het wegschrijven moeten de jaartallen er nog tussen gevoegd worden
  
  for (i in 1:dim.year.size){  #i=1
    
    if (time.dependent){
      cat(dim.year.values[i], file = outputfile, append = TRUE)
      cat(sep.years, '\n', sep = '', file = outputfile, append = TRUE)
    }

    out = outvector[((i-1)*nrow.for.1.year+1):(i*nrow.for.1.year)]
    
    if (!is.null(sprintf.format)){
      out = sprintf(sprintf.format,out)
    } 
    
    if (!matrix.format){
      cat( paste0(out, sep=paste0(sep.data)), '\n', file = outputfile, append = TRUE)
    } else {
      # insert newline after every <last_dim> values
      nvalues.per.row = unname(dim.noyr.sizes[length(dim.noyr.sizes)])
      for (j in 1:(length(out)/nvalues.per.row)){  #j=1
        out.sub = out[((j-1)*nvalues.per.row+1):(j*nvalues.per.row)]
        cat( paste0(out.sub, sep=paste0(sep.data)) , file = outputfile, append = TRUE)  # slow, but essential...
        cat( '\n' , file = outputfile, append = TRUE)
        # Note: you can't first build output string, then write everything at once (for higher speed)
        # out.str = paste0(out.str, paste0(out.sub, sep=paste0(sep.data)), '\n')  
        # this is fast, but wrong: does not create true matrix format in the text file
      }
    }
      
    if (i == dim.year.size)
      cat('];', file = outputfile, append = TRUE)
    
  }
  
  return(0)
}
