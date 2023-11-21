################################################################################
### Exported Generics
################################################################################

#-------------------------------------------------------------------------------
setGeneric('calld', signature = 'obj', function(obj, ...)
    standardGeneric('calld'), package = 'RedeR')
setGeneric('resetd', signature = 'obj', function(obj)
    standardGeneric('resetd'), package = 'RedeR')
setGeneric('exitd', signature = 'obj', function(obj)
    standardGeneric('exitd'), package = 'RedeR')
setGeneric('ping', signature = 'obj', function(obj)
    standardGeneric('ping'), package = 'RedeR')
setGeneric('version', signature = 'obj', function(obj)
    standardGeneric('version'), package = 'RedeR')
setGeneric('relax', signature = 'obj', function(obj, ...)
    standardGeneric('relax'), package = 'RedeR')
#-------------------------------------------------------------------------------
setGeneric('addGraph', signature = 'obj', function(obj, ...)
    standardGeneric('addGraph'), package = 'RedeR')
setGeneric('getGraph', signature = 'obj', function(obj, ...)
    standardGeneric('getGraph'), package = 'RedeR')
#-------------------------------------------------------------------------------
setGeneric('nestNodes', signature = 'nodes', function(nodes, ...)
    standardGeneric('nestNodes'), package = 'RedeR')
#-------------------------------------------------------------------------------
setGeneric('addNodes', signature = 'nodes', function(nodes, ...)
    standardGeneric('addNodes'), package = 'RedeR')
setGeneric('addEdges', signature = 'edges', function(edges, ...)
    standardGeneric('addEdges'), package = 'RedeR')
#-------------------------------------------------------------------------------
setGeneric('selectNodes', signature = 'nodes', function(nodes, ...)
    standardGeneric('selectNodes'), package = 'RedeR')
setGeneric('selectEdges', signature = 'edges', function(edges, ...)
    standardGeneric('selectEdges'), package = 'RedeR')
#-------------------------------------------------------------------------------
setGeneric('deleteNodes', signature = 'nodes', function(nodes, ...)
    standardGeneric('deleteNodes'), package = 'RedeR')
setGeneric('deleteEdges', signature = 'edges', function(edges, ...)
    standardGeneric('deleteEdges'), package = 'RedeR')

################################################################################
### Internal Methods (not exported generics)
################################################################################
setGeneric('isRelaxActive', signature = 'obj', function(obj)
    standardGeneric('isRelaxActive'), package = 'RedeR')
setGeneric('rederpost', signature = 'obj',
    function(obj, method, ..., gdata = list(...))
        standardGeneric('rederpost'), package = 'RedeR')
