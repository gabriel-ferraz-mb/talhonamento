# -*- coding: utf-8 -*-
"""
Created on Thu Dec 15 06:27:37 2022

@author: gabriel.ferraz
"""
#Import libraries

import ee
import geemap
import os, glob
import rasterio
import rpy2.robjects as robjects
import sys
import pandas as pd
import sqlalchemy
import geopandas as gpd
import json    
from sqlalchemy import create_engine
import time

#Initiate GEE

#ee.Authenticate()
ee.Initialize()


class TalhonamentoAutomatico:
    def __init__(self, start, end, cod):
        self.start = ee.Date(start)
        self.end = ee.Date(end)
        self.path = r'C:\Projetos\monitoramento\pipeline_cadastro'
        self.cod = cod
        
        uf = cod[0:2].lower()
        
        user = 'ferraz'
        password = '3ino^Vq3^R1!'
        host = 'vps40890.publiccloud.com.br'
        port = 5432
        database = 'carbon'
        
        engine = create_engine(
                url="postgresql+psycopg2://{0}:{1}@{2}:{3}/{4}".format(
                    user, password, host, port, database
                )
            )
        
        q = "select st_asgeojson(geom) as geom from car.area_imovel_{0} where cod_imovel  = '{1}'".format(uf, cod)
        
        geom = pd.read_sql_query(q,con=engine)
        gdf = gpd.read_file(geom['geom'][0], driver='GeoJSON')
        geo_json = gdf.to_json()
        j = json.loads(geo_json)
        self.CAR  = ee.FeatureCollection(j)
        
        #self.CAR = ee.FeatureCollection('projects/ee-carbonei/assets/area_imovel/cars_all_ufs').filter(ee.Filter.eq('cod_imovel', cod))
        #self.CAR = ee.FeatureCollection('projects/ee-carbonei/assets/MT-5102686-235C60F3726D4809B96D1F6A5B4B731E')
    
    def getMasks(self):
        # Mask
        dynamicWorld = ee.ImageCollection('GOOGLE/DYNAMICWORLD/V1')\
                       .filterDate(self.start, self.end)
        # merx_to_2022 = ee.Image('projects/ee-carbonei/assets/mapeamento/Remap_SOJA_TO_2022_Filtered_05_ha_v1')
        
        def create_mask(image, mask, value):
            return image.updateMask(mask.eq(value))
        
        
        # def mask_image(image): 
        #      return create_mask(image, merx_to_2022, 1)
        
        built = dynamicWorld.select('built').mosaic().gte(0.5)
        def built_mask (image):
            return create_mask(image, built, 0)
        
        built_mask2 = built.eq(0).selfMask()
        
        water = dynamicWorld.select('water').mosaic().gte(0.5)
        def water_mask(image):
            return create_mask(image, water, 0)
        
        water_mask2 = water.eq(0).selfMask()
        
        crop = dynamicWorld.select('crops').mosaic().gte(0.5)
        def crop_mask(image):
             return create_mask(image, crop, 1)
            
        trees = dynamicWorld.select('trees').mosaic().gte(0.5)
        def trees_mask(image):
             return create_mask(image, trees, 0)
        
        trees_mask2 = trees.eq(0).selfMask()
        
        # All masks
        mask_all = trees_mask2.add(water_mask2.add(built_mask2))#.clip(UF)
        
        # Functions: mask cloud
        def maskS2clouds(image):
            qa = image.select('QA60')
            cloudBitMask = 1 << 10 # Clouds
            cirrusBitMask = 1 << 11 # Cirrus
            mask = qa.bitwiseAnd(cloudBitMask).eq(0)
            mask = mask.bitwiseAnd(cirrusBitMask).eq(0)
            return image.updateMask(mask).divide(10000)
        
        return mask_all
        
    def get_dataset(self):  
        
        soja_br = ee.FeatureCollection('projects/ee-carbonei/assets/mapeamento/merx_soja_br_2022_grid5x5')
        fb = self.CAR
        
        S2_ts = (ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
                .filterBounds(fb)
                .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE',20))
                .sort("CLOUD_COVER", True)
                .select('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12', 'QA60')
                .filterDate(self.start, self.end)
                .first())
    
        crs = S2_ts.select('B2').projection().getInfo()['crs']
        
                
        ndvi = S2_ts.normalizedDifference(['B8','B4']).rename("NDVI")
        S2_ts = S2_ts.addBands(ndvi)
        image = S2_ts.clip(fb)
        
        nameOfBands = image.bandNames().getInfo()
        nameOfBands.remove("QA60")
        image = image.select(nameOfBands) 
        
        srtm = ee.Image('USGS/SRTMGL1_003').rename('DSM')     
        slope = ee.Terrain.slope(srtm).rename('SLOPE')
        
        mask_all = self.getMasks()
        
        elevationImage = srtm.addBands(slope).mask(mask_all).clip(fb)
        
        soil_property = "projects/soilgrids-isric/clay_mean"
        layers = ["clay_0-5cm_mean", "clay_5-15cm_mean", "clay_15-30cm_mean", "clay_30-60cm_mean"]
        soil_property = ee.Image(soil_property).select(layers).mask(mask_all).reduce(ee.Reducer.mean()).divide(10).clip(fb).rename('clay')
        soil_property_areia = "projects/soilgrids-isric/sand_mean"
        layers_areia = ["sand_0-5cm_mean", "sand_5-15cm_mean", "sand_15-30cm_mean", "sand_30-60cm_mean"]
        soil_property_areia = ee.Image(soil_property_areia).select(layers_areia).mask(mask_all).reduce(ee.Reducer.mean()).divide(10).clip(fb).rename('sand')
    
        dataset = image.addBands([elevationImage, soil_property, soil_property_areia])
    
        dataset = dataset.resample('bilinear').reproject(crs=crs, scale=10)
        dataset = dataset.mask(mask_all).clip(fb)
        try:
            soy_masked = dataset.clip(soja_br)
            
            stat_soy = soy_masked.select("B1").reduceRegion (
              reducer= ee.Reducer.count(),
              geometry= fb,
              scale= 10,
              maxPixels= 1e9
            )
            count_soy = stat_soy.getInfo()["B1"]
            
            stat_whole = dataset.select("B1").reduceRegion (
              reducer= ee.Reducer.count(),
              geometry= fb,
              scale= 10,
              maxPixels= 1e9
            )
            count_whole = stat_whole.getInfo()["B1"]
            
            proportion = count_soy/count_whole
            
            if(proportion > 0.10):
                result = soy_masked
            else:
                result = dataset
                
            print("Overlay between CAR and soy mask detected")
        except:
            result = dataset
            print("No overlay between CAR and soy mask")
        finally:
            return result
       
    
    def export_dataset(self, dataset):
        
        fb = self.CAR
        vectors = dataset.select('B2').reduceToVectors(**{
          'geometry': fb,
          'crs': dataset.select('B2').projection(),
          'scale': 20,
          'geometryType': 'polygon',
          'eightConnected': False,
          #'labelProperty': 'zone'#,
          #'reducer': ee.Reducer.mean()
        })
        
        out_dir = os.path.join(self.path + "\\input")
        
        if not os.path.isdir(out_dir):
            os.makedirs(out_dir)
        
        #filename = os.path.join(self.path, '_')
        
        for name in dataset.bandNames().getInfo():
            
            fname = out_dir + '\\' + name + ".tif"
            bfile = os.path.isfile(fname)
            
            if not bfile:
                geemap.ee_export_image(dataset.select(name), filename=fname, scale=10, region=fb.geometry(), file_per_band=False)
        
        vname =  out_dir + '\\' + 'vector.shp'
        bvector = os.path.isfile(vname)
        
        if not bvector:
            geemap.ee_export_vector(vectors,vname, verbose=True)
    
        return out_dir
    
    def get_stack(self, out_dir, doa):
        fl = []
        os.chdir(out_dir)
        for file in glob.glob("*.tif"):
            fl.append(file)
            
        stack_path = out_dir + '\\' +self.cod + "_" + doa + "_" + "stack.tif"
        test = rasterio.open(fl[0])

        with rasterio.Env():
        
            # Write an array as a raster band to a new 8-bit file. For
            # the new file's profile, we start with the profile of the source
            profile = test.profile
        
            # And then change the band count to 1, set the
            # dtype to uint8, and specify LZW compression.
            profile.update(
                dtype=rasterio.float32,
                count=len(fl),
                compress='lzw',
                tiled=True,
                driver="GTiff"
            )
        
        if not os.path.isfile(stack_path):
            # Read each layer, convert to float and write it to stack
            with rasterio.open(stack_path, 'w', **profile) as dst:
                for id, layer in enumerate(fl, start=0):
                    with rasterio.open(layer) as src1:
                        #layer = np.where(layer==0,0,layer)
                        dst.write_band(id + 1, src1.read(1).astype('float32'))
    
    def executeR(self):
        r_source = robjects.r['source']
        r_source(self.path + '\\execute.R')

    
    def execute(self):
        
        im = self.get_dataset()
        doa = ee.Date(im.get('system:time_start')).format('YYYY-MM-dd').getInfo()
        out_dir = self.export_dataset(im)
        self.get_stack(out_dir, doa)
        self.executeR()
        
        
        
if __name__ == '__main__':
    t = TalhonamentoAutomatico(sys.argv[1], sys.argv[2], sys.argv[3])
    t.execute()
    




