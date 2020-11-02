if __name__ == "__main__":
    queries = list()
    imports = list()
    with open("QueryStrings.txt", "r") as QueryStrings:
        for l in QueryStrings.readlines():
            if l.startswith('"zzzWrite'):
                queries.append(l.replace('"zzz',"").replace('zzz"',"").replace('yyy','"'))
    with open("RunQueryString.sadl", "w") as Outfile:
        Outfile.write('uri "http://Turnstile/RunQueries".\n\n')
        Outfile.write('import "http://Turnstile/All".\n\n')
        for l in queries:
            Outfile.write(l)
    print("Done")
