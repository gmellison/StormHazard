
# %% imports
import pynldas2 as nldas
import pandas as pd

# %% read landfall data
df = pd.read_csv("landfalls.csv")
df.head()

# %%
res=nldas.get_bycoords([( -86.9375, 33.937)], "2019-01-01", "2020-01-01")
print(res)

# %%
df.shape
pd.to_datetime(df.datetime[0])


# %% get nldas2
#for i in range(df.shape[0]):
#    coords=[(df.lat[i], df.lon[i])]
#    date=pd.to_datetime(df.datetime[i])
#    start_time=date 
#    df1 = nldas.get_by_coords(, start)

# %% 
i = 1
coords=(df.lon[i], df.lat[i])
print(coords)
# lon.min, lat.min, lon.max, lat.max
coord_box=(coords[0]-0.1, coords[1]-0.1, coords[0]+0.1, coords[1]+0.1)
print(coord_box)
date=pd.to_datetime(df.datetime[i])
start_time=date 
end_time=date+pd.Timedelta("1 day")

print(start_time)
print(end_time)
df2=nldas.get_bygeom(coord_box,start_time,end_time,4326,variables='prcp')
print(df2)


## %% 
#from pathlib import Path
#import pynldas2 as nldas
#from pygeohydro import WBD
#grid = nldas.get_grid_mask()
#grid
#ax =rgrid.NLDAS_veg.plot()
## ax.figure.savefig(Path("_static", "nldas_grid.png"), facecolor="w", bbox_inches="tight")
#huc8 = WBD("huc8")
#geometry = huc8.byids("huc8", "13060003").geometry[0]
#clm = nldas.get_bygeom(geometry, "2010-01-08", "2010-01-08", 4326, variables="prcp", snow=True)
#ax = clm.snow.sel(time=slice("2010-01-08T05:00:00", "2010-01-08T010:00:00")).plot(
#    col="time", col_wrap=3
#)
#ax.fig.savefig(Path("_static", "nldas_snow.png"), facecolor="w", bbox_inches="tight")










import requests
import pandas as pd
import matplotlib.pyplot as plt
sns.set_theme(style="darkgrid")
import urllib
import urllib.parse as urlp
import io
import warnings
warnings.filterwarnings("ignore")
