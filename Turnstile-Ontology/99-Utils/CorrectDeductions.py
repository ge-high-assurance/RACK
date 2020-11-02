with open("../Temp/Deductions.sadl.Deductions.owl.txt","r") as infile:
    with open("../Temp/Deductions.sadl.Deductions.owl","w") as outfile:
        bigString = infile.read()
        outfile.write(bigString.replace('xmlns:xsd="http://www.w3.org/2001/XMLSchema#">', 'xmlns:xsd="http://www.w3.org/2001/XMLSchema#"\n  xml:base="http://Turnstile/DeductionsInferred">'))