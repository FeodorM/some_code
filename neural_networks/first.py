#!/usr/bin/env python3
import numpy as np
from numpy.linalg import inv
from urllib.request import urlopen


def data_from_url(url, skiprows=1, delimiter=',' **kwargs):
    return laodtxt(
        urlopen(url),
        skiprows=skiprows,
        delimiter=delimiter,
        **kwargs
    )

def coeffs(x, y):
    return inv(x.T.dot(x)).dot(x.T).dot(y)
