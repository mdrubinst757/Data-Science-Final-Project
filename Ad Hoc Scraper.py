import urllib
import re
import csv
import numpy
import pandas

urls = []
http = []
correcturl = []

f = open('C:\Users\mdrub_000\Desktop\Data Science Project\\train.csv') ##Change path
data = csv.reader(f)

urls = []

for row in data:
    urls.append(row[4]) ##Change column depending on where it is

f_urls = []
for i in urls:
    if i == '':
        pass
    else:
        f_urls.append('http://' + str(i)) ##make sure are consistent in original file
f_urls.pop(0)
n_urls = filter(None, f_urls)

a = []
b = []
c = []
d = []
e = []
f = []
g = []
h = []

##Change depending on keywords you want
health = []
dental = []
optical = []
medic = []
diagn = []
treatment = []
physician = []
quality = []
 
for i in n_urls:
    try:
        htmlfile = urllib.urlopen(i)
        htmltext = htmlfile.read()
        a = str(htmltext.count('health'))
        b = str(htmltext.count('dental'))
        c = str(htmltext.count('optical'))
        d = str(htmltext.count('medic'))
        e = str(htmltext.count('diagn'))
        f = str(htmltext.count('treatment'))
        g = str(htmltext.count('physician'))
        h = str(htmltext.count('quality of care'))
        health.append(a)
        dental.append(b)
        optical.append(c)
        medic.append(d)
        diagn.append(e)
        treatment.append(f)
        physician.append(g)
        quality.append(h)
    except IOError:
        health.append('NA')
        dental.append('NA')
        optical.append('NA')
        medic.append('NA')
        diagn.append('NA')
        treatment.append('NA')
        physician.append('NA')
        quality.append('NA')

df = pandas.DataFrame()
df['Website'] = n_urls
df['Health'] = health
df['Dental Count'] = dental
df['Optical'] = optical
df['Medic'] = medic
df['Treatment'] = treatment
df['Diagnosis'] = diagn
df['Physician'] = physician
df['Quality'] = quality
n
df['WebAddress'] = df['Website'].replace(to_replace = 'http://', value = '')

##Output
df.to_csv(path_or_buf = "C:\Users\mdrub_000\Desktop\Brookings Data\scrape2.csv", sep = ",") 