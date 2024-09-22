#-*- coding : utf-8 -*-

import os
import pandas as pd
from dbfread import DBF

if __name__ == '__main__':
    folder = 'I:/sspCurrent_project/dbf_current/current'
    head = ['ID_1','COUNT','AREA','MIN','MAX','RANGE','MEAN','STD','SUM','FILENAME']
    dataset = []
    filenames = os.listdir(folder)
    for file in filenames:
        if not file.__contains__('.dbf'):
            continue

        filename = file[:-4]
        table = DBF(f'{folder}/{file}')
        # 遍历数据表
        for record in table:
            row = []
            for field in record:
                row.append(record[field])
            row.append(filename)
            dataset.append(row)
            print(file, row)

    #with pd.ExcelWriter(f'I:/sspCurrent_project/dbf_current/current.csv') as writer:
    #    df = pd.DataFrame(data=dataset, columns=head)
     #   df.to_csv(excel_writer=writer, index=False)
        
    df = pd.DataFrame(data=dataset)
    df.to_csv('I:/sspCurrent_project/dbf_current/current.csv', header=head, index=False)
