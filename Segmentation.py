import dask.dataframe as dd


def particion_operadores(operador, nombre):

    df = dd.read_parquet(
            "./parquets/", \
            columns=['client_latitude', 'client_longitude', 'network_operator_name', 'rsrp', 'result_date'] 
        )

    df = df[(df['rsrp']>=-121) & (df['rsrp']<=-44)]

    if(operador != 'todos'):
        df = df[df['network_operator_name']==nombre]
    else:
        df
    
    df = df.repartition(npartitions=df.npartitions // 100)

    # Guardamos la base filtrada en archivos parquet
    name_function = lambda x: f"{operador}-AcapulcoOtis-{x}.parquet"
    df.to_parquet(f'./operador_parquet/{operador}', name_function=name_function)

#particion_operadores('att', 'AT&T')
#particion_operadores('telcel', 'Telcel')
#particion_operadores('altan', 'ALTAN Redes')

particion_operadores('todos', 'Todos')
