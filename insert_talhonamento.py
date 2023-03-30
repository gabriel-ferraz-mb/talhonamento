# -*- coding: utf-8 -*-
"""
Created on Thu Feb 16 10:12:20 2023

@author: gabri
"""


import sqlalchemy    
from sqlalchemy import create_engine   
import geojson
import glob
import psycopg2

#car = './/TO-1703073-EBCD76CF655D4AFA9158BCE8C3429892'
geojson_list =  glob.glob('./*.GeoJSON')
#geojson_list =  glob.glob(car + '*')


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


if not engine.dialect.has_schema(engine, 'monitoramento'):
    engine.execute(sqlalchemy.schema.CreateSchema('monitoramento'))

for path in geojson_list: 
    conn = psycopg2.connect("host='{0}' port='{1}' dbname='{2}' user='{3}' password='{4}'".format(
        host, port, database, user, password))
    cur = conn.cursor()
    cod = path.split('_')[0][2:]
    with open(path) as f:
        gj = geojson.load(f)
    features = gj['features']
    
    t = cod.replace('-', '').lower()
    tableName =  "{0}_parcels".format(t)
    
    cur.execute("select exists(select * from information_schema.tables where table_name=%s)", (tableName,))
    b = cur.fetchone()[0]
    
    if not b:
        ct_query = "CREATE TABLE monitoramento.{0} (talhao integer,  classe varchar,   geom geometry(Polygon, 4326));"\
                .format(tableName)
        cur.execute(ct_query.lower())
        qlist = []
        for feature in features:
            geom = '{\
            "type":"TYPE",\
            "coordinates":COORDINATES,\
            "crs":{"type":"name","properties":{"name":"EPSG:4326"}}}'\
    .replace('TYPE',str(feature['geometry']['type'])).replace('COORDINATES',str(feature['geometry']['coordinates']))
    
            q = "(" + str(feature['properties']['id_talhao']) +", '" +  str(feature['properties']['class']) +\
                "', '" + geom + "')"
            qlist.append(q)
            #q = q.replace('\\', '')
            
        query = ", ".join(qlist)
        
        c_query ="INSERT INTO monitoramento.{0} (talhao, classe, geom) VALUES "\
             .format(tableName) + query
        
        
        cur.execute(c_query)
        # cur.execute(i_query)
        conn.commit()
        conn.close()
        print(cod + ' parcels registered successfully.')
    else:
        print('Farm is already registered.')