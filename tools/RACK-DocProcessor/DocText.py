from tkinter import *
from tkinter import ttk
import re

import pdftotext
import Logger
class DocText(Frame):

    
    def __init__(self,parent, *args, **kwargs):
        self.callbacks = {}
        Frame.__init__(self, parent)
        self.columnconfigure(2,weight=1)
        self.rowconfigure(2,weight=1)
        self.currentPage = 1
        self.pdfDoc = None
        self.currentMatch = -1

        #RegexLabel
        Label(self, text= "Search", justify="left").grid(row = 1, column = 1,\
                             columnspan= 1, rowspan = 1,\
                             sticky="NSEW") 

        # searchEntry
        self.searchEntry = Entry(self)
        self.searchEntry.bind('<Return>', self.searchValidation)
        self.searchEntry.bind('<Enter>', self.searchValidation)
        self.searchEntry.bind('<FocusOut>', self.searchValidation)
        self.searchEntry.grid(row = 1, column = 2,\
                             columnspan= 2, rowspan = 1,\
                             sticky="NSEW")
        #RegexLabel
        Label(self, text= "Highlight", justify="right").grid(row = 0, column = 1,\
                             columnspan= 1, rowspan = 1,\
                             sticky="NSEW") 
        # highlighEntry
        self.highlightEntry = Entry(self)
        self.highlightEntry.bind('<Return>', self.highlightValidation)
        self.highlightEntry.bind('<Enter>', self.highlightValidation)
        self.highlightEntry.bind('<FocusOut>', self.highlightValidation)
        self.highlightEntry.grid(row = 0, column = 2,\
                             columnspan= 2, rowspan = 1,\
                             sticky="NSEW")
        
        #SelectButton
        self.findButton = Button(self, command = self.selectNext, text="Select Next")
        self.findButton.grid(row = 1, column = 4,\
                             columnspan= 1, rowspan = 1,\
                             sticky="NSEW")

        #SelectAllButton
        self.findButton = Button(self, command = self.selectAll, text="Select All")
        self.findButton.grid(row = 1, column = 5,\
                             columnspan= 2, rowspan = 1,\
                             sticky="NSEW")
        # Text Box
        self.text = Text(self)
        self.text.grid(row = 2, column = 1,\
                             columnspan= 5, rowspan = 1,\
                             sticky="NSEW")
        # Scroll Bar
        self.scroll = ttk.Scrollbar(self,orient="vertical",command=self.text.yview)
        self.scroll.grid(row = 2, column = 6,
                          columnspan= 1, rowspan = 1,\
                          sticky="NSEW")
        self.text.configure(yscrollcommand=self.scroll.set)


        #prevButton
        self.prevButton = Button(self, command = self.prevPage, text="Previous")
        self.prevButton.grid(row = 3, column = 1,\
                             columnspan= 1, rowspan = 1,\
                             sticky="NSEW")


        #currentPageEntry
        self.currentPageEntry = Entry(self)
        self.currentPageEntry.bind('<Return>', self.enteredCurrentPage)
        self.currentPageEntry.grid(row = 3, column = 3,\
                             columnspan= 2, rowspan = 1,\
                             sticky="NSEW")
        #totalPageButtons
        self.totalPageButtons = Label(self, text="/", justify="left")
        self.totalPageButtons.grid(row = 3, column = 4,\
                             columnspan= 1, rowspan = 1,\
                             sticky="NSEW")        

        #nextButton
        self.nextButton = Button(self, command = self.nextPage, text="Next")
        self.nextButton.grid(row = 3, column = 5,\
                             columnspan= 2, rowspan = 1,\
                             sticky="NSEW")
    def searchValidation(self,event=None):
        '''----------------------------------------------
            Updates the text box to highlight the regex 
            entered into the search field. Search 
            differs from the highlight because of the 
            select next and select all buttons that allow 
            selected text to be set       
        ----------------------------------------------'''     
        Logger.write("DocText.searchValidation")
        regexp = self.searchEntry.get()
        start = self.text.index("1.0")
        end = self.text.index(END)
        
        self.text.mark_set("matchStart", start)
        self.text.mark_set("matchEnd", start)
        self.text.mark_set("searchLimit", end)
        #Clear Existing Tags
        self.text.tag_delete("Search")
            
        self.text.tag_config("Search", background="yellow", foreground="black")
        self.text.tag_lower("Search")
        count = IntVar()
        self.matches = list()
        while True:
            index = self.text.search(regexp, "matchEnd","searchLimit",
                                count=count, regexp=True)
            if index == "": break
            if count.get() == 0: break # degenerate pattern which matches zero-length strings
            self.text.mark_set("matchStart", index)
            self.text.mark_set("matchEnd", "%s+%sc" % (index, count.get()))
            self.matches.append((index, "%s+%sc" % (index, count.get())))
            self.text.tag_add("Search", "matchStart", "matchEnd")

    def highlightValidation(self,event=None):
        '''----------------------------------------------
            Updates the text box to highlight the regex 
            entered into the highlight field.        
        ----------------------------------------------'''  
        Logger.write("DocText.highlightValidation")
        regexp = self.highlightEntry.get()
        start = self.text.index("1.0")
        end = self.text.index(END)
        
        self.text.mark_set("matchStart", start)
        self.text.mark_set("matchEnd", start)
        self.text.mark_set("searchLimit", end)
        #Clear Existing Tags
        self.text.tag_delete("highlight")
            
        self.text.tag_config("highlight", background="orange", foreground="black")
        self.text.tag_lower("highlight")
        count = IntVar()
        while True:
            index = self.text.search(regexp, "matchEnd","searchLimit",
                                count=count, regexp=True)
            if index == "": break
            if count.get() == 0: break # degenerate pattern which matches zero-length strings
            self.text.mark_set("matchStart", index)
            self.text.mark_set("matchEnd", "%s+%sc" % (index, count.get()))
            self.text.tag_add("highlight", "matchStart", "matchEnd")                          
        
    def selectAll(self,event=None):
        '''----------------------------------------------
            Handler for the select all button that 
            updates the selected text to be each of the 
            found search strings   
        ----------------------------------------------'''   
        Logger.write("DocText.findNext")
        if self.currentMatch>-1:
            self.text.tag_remove("sel",  self.matches[self.currentMatch][0],self.matches[self.currentMatch][1])

        self.currentMatch+=1
        if self.currentMatch >len(self.matches):
            self.currentMatch = 0
        for a in self.matches:
            self.text.tag_add("sel", a[0],a[1])
        
                    

    def selectNext(self,event=None):
        '''----------------------------------------------
            Handler for the select next button that 
            updates the selected text to be the 
            found search strings
        ----------------------------------------------'''  
        Logger.write("DocText.findNext")
        if self.currentMatch>-1:
            self.text.tag_remove("sel",  self.matches[self.currentMatch][0],self.matches[self.currentMatch][1])

        self.currentMatch+=1
        if self.currentMatch >=len(self.matches):
            self.currentMatch = 0
            
        self.text.tag_add("sel", self.matches[self.currentMatch][0],self.matches[self.currentMatch][1])
        
    def enteredCurrentPage(self,event):
        '''----------------------------------------------
            Handler for the entry of a page number
        ----------------------------------------------'''  
        Logger.write("DocText.enteredCurrentPage")
        enteredValue = None
        try:
            enteredValue = int(self.currentPageEntry.get())
        except:
            pass
        if enteredValue != None:
            if enteredValue > 0 and enteredValue <= len(self.pdfDoc):
                self.currentPage = enteredValue
        self.pageUpdate()
            
    def pageUpdate(self):
        '''----------------------------------------------
            Page display updates
        ----------------------------------------------'''  
        Logger.write("DocText.pageUpdate")
        if self.currentPage!=None:            
            self.currentPageEntry.delete(0,END)
            self.currentPageEntry.insert(END, str(self.currentPage))
            self.setText(self.pdfDoc[self.currentPage-1])
            self.currentMatch = -1
            self.searchValidation()
            self.highlightValidation()
        else:
            self.currentPageEntry.delete(0,END)
            self.setText("")

    def nextPage(self,event=None):
        '''----------------------------------------------
            handler for next page button press
        ----------------------------------------------'''  
        Logger.write("DocText.nextPage")
        if self.currentPage < len(self.pdfDoc):
            self.currentPage +=1
        self.pageUpdate()
        
    def prevPage(self,event=None):
        '''----------------------------------------------
            handler for previous page button press
        ----------------------------------------------'''  
        Logger.write("DocText.prevPage")
        if self.currentPage > 1:
            self.currentPage -=1
        self.pageUpdate()

    def setText(self, text):
        self.text.delete(1.0,END)
        self.text.insert(END, text)
        
    def loadDocument(self, docPath):
        '''----------------------------------------------
            takes a file path and processes it as a pdf to display
        ----------------------------------------------'''  
        
        with open(docPath, "rb") as f:
            self.pdfDoc = pdftotext.PDF(f)
        self.totalPageButtons['text'] = "/" + str(len(self.pdfDoc))
        self.currentPage = 1
        self.pageUpdate()
        
    
if __name__ == '__main__':
    def addNewReqCallback(self, text):
        Logger.write("callBack", text)
    Test = Tk()
    Test.title("DocText Test")
    #rd = Frame(Test)
    DTM = DocText(Test)
    DTM.grid(row = 1, column = 1)
    DTM.loadDocument("/home/arcos/doc-in/V133_SRS_SP_SBVT_Final-NAV.pdf")
    
    Test.mainloop()
                                
