"""
This Julia module interfaces with the R package AdhereR.
Copyright (C) 2022  Dan Dediu (ddediu@gmail.com)

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
"""

"""
This module allows the transparent use of the `R` package `AdhereR` from `Julia`.
It exports a set of classes, datasets and methods that mirror in the best way 
possible those from `R`.
Internally, it uses `RCall` (https://juliainterop.github.io/RCall.jl/stable/) to
"talk" to `R`.
""" 
module AdhereR

# The AdhereR R package minimum required versions:
rpkgnameminver = Dict("AdhereR"=>"0.8.1", "AdhereRViz"=>"0.2.1") # minimum required versions

# Error messages:
errmsgs = Dict("UnknownError"=>"Error using RCall to talk to R or AdhereR: please check the details below.",
               "AllGood"=>"AdhereR: all seems good to go!",
               "RError"=>"The RCall package is required: please make sure RCall and R are correctly installed, and that RCall can 'see' R "*
                            "(if there are multiple versions of R, please make sure RCall 'sees' the correct one). "*
                            "In particular, make sure that on Apple Sillicon machines Julia and R are compiled for the same architecture. "*
                            "Please see the manual for more info.",
               "AdhereRNotInstalled"=>"The AdhereR package seems to not be installed in R: "*
                            "please make sure it is corretly installed in the R version that RCall uses. "*
                            "Please see the manual for more info.",
               "AdhereRVersion"=>"The AdhereR package seems to be installed in R, "*
                                 "but it needs to be at least version '"*rpkgnameminver["AdhereR"]*"'.",
               "AdhereRVizNotInstalled"=>"The AdhereRViz package seems to not be installed in R: "*
                                 "while not a show stopper, it means that you cannot do interactive (Shiny) plots. "*
                                 "If do want to do so, please make sure it is corretly installed in the R version that RCall uses. "*
                                 "Please see the manual for more info.",
               "AdhereRVizVersion"=>"The AdhereRViz package seems to be installed in R, "*
                                      "but it needs to be at least version '"*rpkgnameminver["AdhereRViz"]*"'."*
                                      "While not a show stopper, it means that you cannot do interactive (Shiny) plots. "*
                                      "If do want to do so, please make sure it is corretly installed in the R version that RCall uses. ")


# Try to load RCall:
try
    using RCall
catch
    error(errmsgs["RError"])
end

"""
Various initialization stuff, in particular checking if RCall, 
R and the R package AdhereR work as intended, and, if not, 
guide the user on how to install it effectively.
"""
function __init__()
    # Check if RCall works as intended:
    x = missing

    # 1. Basic checs that RCall talks to R:
    try
        x = reval("1.0")
        if rcopy(x) != 1
            error(errmsgs["RError"])
        end
    catch
        error(errmsgs["RError"])
    end

    # 2. Try to load a base library and do some fancier stuff:
    try
        R"library(stats)"
        R"library(datasets)"
        x = R"lm(Sepal.Length ~ Petal.Length, data=iris)"
        xa = rcopy(R"$x$coefficients['(Intercept)']")
        xb = rcopy(R"$x$coefficients['Petal.Length']")
        if round(xa,digits=2) != 4.31 || round(xb,digits=2) != 0.41
            error(errmsgs["RError"])
        end
    catch
        error(errmsgs["RError"])
    end

    # 3. Check AdhereR:
    try
        R"library(AdhereR)"
    catch
        error(errmsgs["AdhereRNotInstalled"])
    end
    try
        rscript = "if(!require(AdhereR)) {return(-1)} else {if( compareVersion('" * rpkgnameminver["AdhereR"] * "', as.character(packageVersion('AdhereR'))) > 0 ) {return (-2)} else {return (0)}}"
        x = rcopy(reval(rscript))
        if x == (-1)
            error(errmsgs["AdhereRNotInstalled"])
        elseif x == (-2)
            error(errmsgs["AdhereRVersion"])
        elseif x != 0
            error(errmsgs["UnknownError"])
        end
    catch
    end

    # 3. Check AdhereRViz:
    try
        R"library(AdhereRViz)"
    catch
        warn(errmsgs["AdhereVizRNotInstalled"])
    end
    try
        rscript = "if(!require(AdhereRViz)) {return(-1)} else {if( compareVersion('" * rpkgnameminver["AdhereRViz"] * "', as.character(packageVersion('AdhereRViz'))) > 0 ) {return (-2)} else {return (0)}}"
        x = rcopy(reval(rscript))
        if x == (-1)
            error(errmsgs["AdhereRVizNotInstalled"])
        elseif x == (-2)
            error(errmsgs["AdhereRVizVersion"])
        elseif x != 0
            error(errmsgs["UnknownError"])
        end
    catch
    end

    # All looks good: let the user know:
    println(errmsgs["AllGood"])
end

# The CMA hierarchy:  

end

