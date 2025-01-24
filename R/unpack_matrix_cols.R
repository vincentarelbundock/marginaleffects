unpack_matrix_cols <- function(x) {
    data.table::setDF(x)

    # what data types are we working with
    dcs <- sapply(x, function(x) class(x)[1])

    # do something if we have matrix columns
    if (any(mcols <- dcs == "matrix")) {
        # assume that any matrix columns are of the same number of cols
        nc <- ncol(x[mcols][[1L]])
        out <- lapply(x[mcols], as.vector)
        other <- lapply(x[!mcols], function(y, nc) rep(y, times = nc), nc = nc)
        out <- as.data.frame(out)
        other <- as.data.frame(other)
        out <- cbind(out, other)
        # put order back as it was
        out <- out[names(x)]
    } else {
        return(x)
    }

    data.table::setDT(out)

    out
}


# The content of this file was adapted from the `gratia` package
# https://github.com/gavinsimpson/gratia

# The MIT License (MIT)
#
# Copyright (c) 2013-2020 Gavin L. Simpson
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
