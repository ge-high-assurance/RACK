from tkinter import ttk
from tkinter import filedialog
from tkinter import *

from nltk.tokenize import word_tokenize
from nltk.tokenize import sent_tokenize

import json
from DocText import *
from DocTree import *
import Logger
import subprocess

class DocProcessor:
    # Document
    docPath = ""

    # Tk Elements
    mainWindow = None
    detailsWindow = None
    docText = None
    useOcr = None
    useOcrCheckbutton = None

    openButton = None

    documentOutline =None


    def docTextSelectionChange(self, event):
        '''----------------------------------------------------------
            handler for any time the selection has changed for the 
            text box that displays the pdf text.
        ----------------------------------------------------------'''
        
        Logger.write("DocProcessorGui.docTextSelectionChange")
        if self.docText.text.tag_ranges(SEL):
            Logger.write(len(self.docText.text.tag_ranges(SEL)))
            selectedText = list()
            index = 0
            ranges = self.docText.text.tag_ranges(SEL)
            while index < len(self.docText.text.tag_ranges(SEL)):
                selectedText.append(self.docText.text.get(ranges[index], ranges[index+1]))
                index+=2
            
            self.docTree.selectedText = selectedText
        else:
            self.docTree.selectedText = [self.docPath.split("/")[-1]]
        

            
        
    def openFile(self):
        '''----------------------------------------------------------
            handler for the open file button, launches a dialog
            to get the file path for the pdf the user wants to 
            process then initializes the GUI based on that pdf        
        ----------------------------------------------------------'''
        
        
        
        
        Logger.write("DocProcessorGui.openFile")
        filePath = filedialog.askopenfilename(initialdir = "../",title="Select PDF file to open",\
                                          filetypes=(("pdf files","*.pdf"),("all files","*.*")))

        if filePath:
            self.docPath = filePath 
        
            self.mainWindow.title("ARCOS DocProcessor - "+self.docPath)
            
            if self.useOcr.get():
                Logger.write("Using OCR on file")
                Logger.write('ocrmypdf', self.docPath, self.docPath.replace(".pdf","-OCR.pdf"))
                returnCode = subprocess.call(['/usr/bin/ocrmypdf', self.docPath, self.docPath.replace(".pdf","-OCR.pdf")])
                Logger.write("OCR Return Code:",returnCode)
                self.docPath = self.docPath.replace(".pdf","-OCR.pdf")
            else:
                Logger.write("Not using OCR on file")        
    
    
            # Intialize Document
            fileName = self.docPath.split("/")[-1]
            self.docTree.initializeDoc(fileName)
            self.docText.loadDocument(self.docPath)
        else:
            Logger.write("No File Chosen")

        

   
    def __init__(self):
        '''----------------------------------------------
            DocProcessor is the main window for processing 
            a PDF into an RACK Ontology SADL File.        
        ----------------------------------------------'''                 
        
        # Base Window
        self.mainWindow = Tk()
        self.mainWindow.title("ARCOS DocProcessor - "+self.docPath)
        self.mainWindow.geometry("1200x800")
        self.mainWindow.columnconfigure(5,weight=1)
        self.mainWindow.columnconfigure(7,weight=1)
        self.mainWindow.rowconfigure(14,weight=1)

        # openButton
        self.openButton = Button(self.mainWindow, command = self.openFile, text="Open PDF")
        self.openButton.grid(row = 1, column = 1,\
                             columnspan= 1, rowspan = 1,\
                             sticky="NSEW")
        # useOcrCheckbutton
        self.useOcr = IntVar()
        self.useOcrCheckbutton = Checkbutton(self.mainWindow,\
                                             text="Use OCR",\
                                             variable = self.useOcr,\
                                             onvalue = 1,\
                                             offvalue = 0)
        self.useOcrCheckbutton.grid(row = 1, column = 2,\
                             columnspan= 1, rowspan = 1,\
                             sticky="NSEW")
        # docTree
        self.docTree = DocTree(self.mainWindow)
        self.docTree.grid(row = 1, column = 7,\
                                  columnspan= 1, rowspan = 15,\
                                  sticky="NSEW")
        # docText
        self.docText = DocText(self.mainWindow)
        self.docText.grid(row = 2, column = 1,\
                          columnspan= 6, rowspan = 15,\
                          sticky="NSEW")
        self.docText.text.bind("<<Selection>>",self.docTextSelectionChange)
        
        self.mainWindow.bind("<Prior>", self.docText.prevPage)
        self.mainWindow.bind("<Next>", self.docText.nextPage)
        
        self.mainWindow.mainloop()

        
if __name__ == '__main__':
    mainWindow = DocProcessor()
