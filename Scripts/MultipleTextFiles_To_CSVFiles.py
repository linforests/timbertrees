import pandas as pd
import os,sys
import time,datetime
from functools import reduce

def findAllFilesWithSpecifiedSuffix(target_dir, target_suffix="txt"):#找到所有txt后缀文件
    find_res = []
    target_suffix_dot = "." + target_suffix
    walk_generator = os.walk(target_dir)
    for root_path, dirs, files in walk_generator:
        if len(files) < 1:
            continue
        for file in files:
            file_name, suffix_name = os.path.splitext(file)
            if suffix_name == target_suffix_dot:
                find_res.append(os.path.join(root_path, file))
    return find_res

pwd='C:\\Users\\测试txt'

dir_list=findAllFilesWithSpecifiedSuffix(pwd)
len_dir_list=len(dir_list)
print('一共【{}】个文件：'.format(len_dir_list))
lfname=[]
for fname in dir_list:
    index1 = len(fname) - fname[::-1].index("\\");
    index2 = fname.index(".txt")
    fn=fname[index1:index2+4]
    lfname.append(fn)
  

    
dfl=[]
for i in range(0,len(dir_list)):
    df1=pd.read_csv(dir_list[i],sep='\t')
    df1['add']=lfname[i]
    dfl.append(df1)


final=reduce(lambda x, y: x.append(y), dfl)
final.to_csv('C:\\Users\\测试txt\\result1.csv')


