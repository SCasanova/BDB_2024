import dask.dataframe as dd
import pandas as pd


def partition(week):

    df = dd.read_parquet(
            f"./parquet_pc/{week}_pc.parquet"
        )
    
    cols =df.game.unique().compute() 
    n = len(cols)
    df = df.set_index('game').repartition(divisions = [cols[0], cols[n//4],cols[2*n//4],cols[3*n//4],cols[4*n//4 -1]])


    # Guardamos la base filtrada en archivos parquet
    name_function = lambda x: f"{week}-{x}.parquet"
    df.to_parquet(f'./partitions/{week}', name_function=name_function)

partition('week1')
partition('week2')
partition('week3')
partition('week4')
partition('week5')
partition('week6')
partition('week7')
partition('week8')
partition('week9')


