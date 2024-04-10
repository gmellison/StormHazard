
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








# %%

import requests
import pandas as pd
import matplotlib.pyplot as plt
import urllib
import urllib.parse as urlp
import io
import warnings
warnings.filterwarnings("ignore")


def get_time_series(start_date,end_date,latitude,longitude,variable):
    """
    Calls the data rods service to get a time series
    """
    base_url = "https://hydro1.gesdisc.eosdis.nasa.gov/daac-bin/access/timeseries.cgi"
    query_parameters = {
        "variable": variable,
        "type": "asc2",
        "location": f"GEOM:POINT({longitude}, {latitude})",
        "startDate": start_date,
        "endDate": end_date,
    }
    full_url = base_url+"?"+ \
         "&".join(["{}={}".format(key,urlp.quote(query_parameters[key])) for key in query_parameters])
    print(full_url)
    iteration = 0
    done = False
    while not done and iteration < 5:
        r=requests.get(full_url)
        if r.status_code == 200:
            done = True
        else:
            iteration +=1
    
    if not done:
        raise Exception(f"Error code {r.status_code} from url {full_url} : {r.text}")
    
    return r.text

def parse_time_series(ts_str):
    """
    Parses the response from data rods.
    """
    lines = ts_str.split("\n")
    parameters = {}
    for line in lines[2:11]:
        key,value = line.split("=")
        parameters[key] = value
    
    
    df = pd.read_table(io.StringIO(ts_str),sep="\t",
                       names=["time","data"],
                       header=10,parse_dates=["time"])
    return parameters, df



df_precip = parse_time_series(
            get_time_series(
                start_date="2022-07-01T00",
                end_date="2022-09-01T00",
                latitude=38.89,
                longitude=-88.18,
                variable="NLDAS2:NLDAS_FORA0125_H_v2.0:Rainf"
            )
        )

# 0.125 spatial resolution
for (lat_inc in range(8))
    for (lon_inc in range(8))
       lat_shift = lat + 0.125*lat_inc  
       lon_shift = lon + 0.125*lon_inc
       # now get rainfall at closest grid point

d = {'time': pd.to_datetime(df_precip[1]['time'], unit='s'), 
    'Rainf': df_precip[1]['data']}

df = pd.DataFrame(data=d)
df.head()
