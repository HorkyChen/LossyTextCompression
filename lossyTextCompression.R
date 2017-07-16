dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(coreNLP)
initCoreNLP(type='chinese')

parse2graph <- function(ptext, leaf.color='chartreuse4', label.color='blue4',
                        title=NULL, cex.main=.9, ...) {
    stopifnot(require(NLP) && require(igraph))

    ## Replace words with unique versions
    ms <- gregexpr("[^() ]+", ptext)                                      # just ignoring spaces and brackets?
    words <- regmatches(ptext, ms)[[1]]                                   # just words
    regmatches(ptext, ms) <- list(paste0(words, seq.int(length(words))))  # add id to words

    ## Going to construct an edgelist and pass that to igraph
    ## allocate here since we know the size (number of nodes - 1) and -1 more to exclude 'TOP'
    edgelist <- matrix('', nrow=length(words)-2, ncol=2)

    ## Function to fill in edgelist in place
    edgemaker <- (function() {
        i <- 0                                       # row counter
        g <- function(node) {                        # the recursive function
            if (inherits(node, "Tree")) {            # only recurse subtrees
                if ((val <- node$value) != 'TOP1') { # skip 'TOP' node (added '1' above)
                    for (child in node$children) {
                        childval <- if(inherits(child, "Tree")) child$value else child
                        i <<- i+1
                        edgelist[i,1:2] <<- c(val, childval)
                    }
                }
                invisible(lapply(node$children, g))
            }
        }
    })()

    ## Create the edgelist from the parse tree
    edgemaker(Tree_parse(ptext))

    ## Make the graph, add options for coloring leaves separately
    g <- graph_from_edgelist(edgelist)
    vertex_attr(g, 'label.color') <- label.color  # non-leaf colors
    vertex_attr(g, 'label.color', V(g)[!degree(g, mode='out')]) <- leaf.color
    V(g)$label <- sub("\\d+", '', V(g)$name)      # remove the numbers for labels
    plot(g, layout=layout.reingold.tilford, ...)
    if (!missing(title)) title(title, cex.main=cex.main)
}

parseStatment <-function(text) {
  output = annotateString(text)
  tokens = getToken(output)
  deps = getDependency(output)
  parser_res <- getParse(output)
  cat(parser_res)

  parse_tree <- parser_res
  parse_tree <- gsub("[\r\n]", "", parse_tree)
  parse_tree <- gsub("ROOT", "TOP", parse_tree)

  par(family='STKaiti')
  parse2graph(parse_tree,
               title = sprintf("'%s'", x), margin=-0.05,
               vertex.color=NA, vertex.frame.color=NA,
               vertex.label.cex=1, asp=0.5,
               vertex.label.font=2, vertex.label.family='STKaiti',
               edge.width=2.5, edge.color='black', edge.arrow.size=0)
}

text="从乐视危机、易到事件，再到资金冻结、机构撤资，最近半年，乐视就像一部跌宕起伏的电视剧，作为乐视的创始人，贾跃亭一次又一次被推上风口浪尖。"
text2="作为乐视的创始人，贾跃亭一次又一次被推上风口浪尖。"

parseStatment(text)
parseStatment(text2)