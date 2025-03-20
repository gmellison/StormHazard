
# %% read landfall data

# %%
import pandas as pd
import requests
import matplotlib.pyplot as plt
import urllib
import urllib.parse as urlp
import io
import warnings
warnings.filterwarnings("ignore")

# hurdat_temp.csv is the file the rainfalls will be joined into. 
# This file is created (in the hurdat script) as a copy of the hurdat csv 
# with an empty column, "precip", where the rainfalls 
# will be populated. The code below will load the 
# temp dataset and ignore rows with "precip" already populated, since
# the data collection may take a while and may fail if the 
# connection is interrupted.

hurdat = pd.read_csv("data/hurdat_temp.csv") 

# %%
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


# %%
for i in range(hurdat.shape[0]): #range(df.shape[0]):
    if hurdat['precip'][i] >= 0:
        continue

    coords=(hurdat.lat[i], hurdat.lon[i])
    date=pd.to_datetime(hurdat.date[i])
    start_time=date 
    end_time=date+pd.Timedelta("2 days")
  
    tot_rf=0
    for lat_inc in range(-4,4):
        for lon_inc in range(-4,4):
           # 0.125 spatial resolution
           lat_shift = coords[0] + 0.125*lat_inc  
           lon_shift = coords[1] + 0.125*lon_inc

           # now get rainfall at closest grid point
           df_precip = parse_time_series(
              get_time_series(
                start_date=start_time.strftime("%Y-%m-%dT00"),#"2022-07-01T00",
                end_date= end_time.strftime("%Y-%m-%dT00"),#"2022-09-01T00",
                latitude= lat_shift,#38.89,
                longitude= lon_shift,#-88.18,
                variable="NLDAS2:NLDAS_FORA0125_H_v2.0:Rainf"
              )
           )
           d = {'time': pd.to_datetime(df_precip[1]['time'], unit='s'), 
                 'Rainf': df_precip[1]['data']}
           df = pd.DataFrame(data=d)
           tot_rf = tot_rf + df.where(df['Rainf']>0)['Rainf'].sum()

    hurdat['precip'][i] = tot_rf
    hurdat.to_csv("data/hurdat_temp.csv")

hurdat.to_csv("data/landfalls_precip.csv")
