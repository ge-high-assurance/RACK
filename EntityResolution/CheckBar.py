#!/usr/bin/python3

from tkinter import *

class Checkbar(Frame):
    def __init__(self, parent=None, picks=[], side=LEFT, anchor=W,  command=None):
        Frame.__init__(self, parent)
        self.vars = {}
        for pick in picks:
            var = IntVar()
            chk = Checkbutton(self, text=pick, variable=var,  command=command)
            chk.pack(side=side, anchor=anchor, expand=YES)
            self.vars[pick]=var
            self.vars[pick].set(1)
    def state(self):
        r = {}
        for v in self.vars:
           r[v] = self.vars[v].get()
        return r
