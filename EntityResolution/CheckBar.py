#!/usr/bin/python3

from tkinter import *

class Checkbar(Frame):
    def __init__(self, parent=None, picks=[], side=LEFT, anchor=W,  command=None):
        Frame.__init__(self, parent)
        self.command = command
        self.vars = {}
        self.buttons = []
        for pick in picks:
            self.buttons.append(Checkbutton(self, text=pick, command=lambda pick=pick: self.callback(pick)))
            self.buttons[-1].pack(side=side, anchor=anchor, expand=YES)
            self.vars[pick] = True
            self.buttons[-1].select()
    def callback(self,  pick):
        self.vars[pick] = not self.vars[pick]
        self.command()
    def state(self):
        return self.vars
