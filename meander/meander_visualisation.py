
from riverrem.REMMaker import REMMaker
# provide the DEM file path and desired output directory
rem_maker = REMMaker(dem="DEM_AMAZON/DEM_AMAZONAS_v2.tif", out_dir='output/')
# create an REM
rem_maker.make_rem()
# create an REM visualization with the given colormap
rem_maker.make_rem_viz(cmap='Blues', z=10)
