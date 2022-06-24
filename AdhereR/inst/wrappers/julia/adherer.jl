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

errormsgrcall = "The RCall package is required: please make sure RCall and R are correctly installed, and that RCall can 'see' R "*
                "(if there are multiple versions of R, please make sure RCall 'sees' the correct one). "*
                "In particular, make sure that on Apple Sillicon machines Julia and R are compiled for the same architecture. "*
                "Please see the manual for more info."

try
    using RCall
catch
    error(errormsgrcall)
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
            error(errormsgrcall)
        end
    catch
        error(errormsgrcall)
    end

    # 2. Try to load a base library and do some fancier stuff:
    try
        R"library(stats)"
        R"library(datasets)"
        x = R"lm(Sepal.Length ~ Petal.Length, data=iris)"
        xa = rcopy(R"$x$coefficients['(Intercept)']")
        xb = rcopy(R"$x$coefficients['Petal.Length']")
        if round(xa,digits=2) != 4.31 || round(xb,digits=2) != 0.41
            error(errormsgrcall)
        end
    catch
        error(errormsgrcall)
    end

    # 3. Check AdhereR:
    try
        
    catch
    end

    # All looks good: let the user know:
    println("AdhereR: all seems good to go!")
end

# See if 

end

