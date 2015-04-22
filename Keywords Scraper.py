import urllib
import re
import csv
import numpy
import pandas
import bs4
import nltk

urls = []
http = []
correcturl = []

f = open('C:\Users\mdrub_000\Desktop\Data Science Project\\scrapersample.csv') ##Change path
data = csv.reader(f)

urls = []

for row in data:
    urls.append(row[2]) ##Change column depending on where it is

f_urls = []
for i in urls:
    if i == '':
        pass
    else:
        f_urls.append('http://' + str(i)) ##make sure are consistent in original file
f_urls.pop(0)
n_urls = filter(None, f_urls)

print n_urls

for i in n_urls:
    html = urllib.urlopen(i).read()
    soup = bs4.BeautifulSoup(html)
    print soup.get_text()

        