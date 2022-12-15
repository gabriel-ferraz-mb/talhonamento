# -*- coding: utf-8 -*-
"""
gabriel.ferraz

"""

import ee
import geopandas as gpd
import pandas as pd
import json
import folium

#ee.Authenticate()
ee.Initialize()

aoi = gpd.read_file(r'C:\Projetos\talhonamento\sorriso-mt\sorriso-mt.shp')
gj = json.loads(aoi.to_json())['features'][0]['geometry']

fb = ee.Geometry(gj)

start = ee.Date('2022-09-01')
end = ee.Date('2022-09-30')

S2_ts = (ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
        .filterBounds(fb)
        .filterDate(start, end)
        #.maskClouds()
        #.scaleAndOffset()
        .select(['NDVI']))

ndviParams = {'min': -1, 'max': 1, 'palette': ['blue', 'white', 'green']}

image = S2_ts.first().clip(fb)
Map = folium.Map(location=[aoi.centroid.y, aoi.centroid.x], zoom_start=8)
Map
Map.addLayer(image, ndviParams, 'NDVI image')

    