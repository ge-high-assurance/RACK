if __name__ == "__main__":
    queries = list()
    imports = list()
    with open("QueryStrings.txt", "r") as QueryStrings:
        for l in QueryStrings.readlines():
            if l.startswith('"zzzWrite'):
                queries.append(l.replace('"zzz',"").replace('zzz"',"").replace('yyy','"'))
    with open("PropertyDefinitionInference.sadl","r") as propertyInferenceSadl:
        for l in propertyInferenceSadl.readlines():
            if l.startswith("import") and l.find("rdf-schema")==-1:
                imports.append(l)
    with open("RunQueryString.sadl", "w") as Outfile:
        Outfile.write('uri "http://Turnstile/RunQueries".\n\n')
        for l in imports:
            Outfile.write(l)
        Outfile.write("\n")
        for l in queries:
            Outfile.write(l)
    print("Done")