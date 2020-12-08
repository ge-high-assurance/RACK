import xml.etree.ElementTree as ET
import csv
import os 
import os.path
'''
==================================================
Read the xsd file to create the export dictionary.
==================================================
'''
def getXsd():
    rackData = {}
    xsd = ET.parse("RACK-DATA.xsd")
    root = xsd.getroot()
    for c in list(root):
        things = c.find('{http://www.w3.org/2001/XMLSchema}complexType')\
              .find('{http://www.w3.org/2001/XMLSchema}sequence')
        for d in list(things):
            properties = d.find('{http://www.w3.org/2001/XMLSchema}complexType')\
              .find('{http://www.w3.org/2001/XMLSchema}sequence')
            headers = list()
            for e in list(properties):
                headers.append(e.attrib['name'])
            rackData[d.attrib['name']]=headers
    return rackData
        
'''
==================================================
Export the CSV files from the input RACK-DATA xml.
==================================================
'''
def createCSV(filePath):
    xsdSpec = getXsd()
    # Create e
    outputDir = filePath.replace(".xml","")
    if not os.path.exists(outputDir):
        os.makedirs(outputDir)
    for thing in xsdSpec:
        headers = xsdSpec[thing] 
        with open(outputDir+"/"+thing+".csv", 'w') as outFile:
            outwriter = csv.writer(outFile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
            outwriter.writerow(headers)
            data = ET.parse(filePath)
            root = data.getroot()
            for c in root.iter(thing):
                rowData = list()
                thingData = {}
                for k in headers:
                    thingData[k] = c.find(k)
                dataString = ""        
                for k in headers:
                    if thingData[k] is not None:
                        rowData.append(thingData[k].text)
                    else:
                        rowData.append("")
                outwriter.writerow(rowData)
        
if __name__ == "__main__":
    createCSV('./Temp/RACK-DATA.xml')
    
    
