# This makefile describes this sample sofware, which consists of some
# sources and associated tools to process those sources and make both
# an executable and a tar-file to distribute the software.

build: sample2

sample2: sample2.c libhelper.a
	cc -o sample2 sample2.c -lhelper

libhelper.a: help.o assist.o
	ar -c libhelper.a help.o assist.o

help.o: help.c help.h
	cc -o help.o help.c

assist.o: assist_gen.c
	cc -o assist.o assist_gen.c

assist_gen.c: assist.txt mkassist.py
	python mkassist.py assist.txt assist_gen.c

dist: sample2dist.tbz2 sample2dist.rpm

sample2dist.tbz2: sample2 libhelper.a help.c help.h assist.txt mkassist.py LICENSE.txt
	tar jcvf sample2dist.tbz2 $*

sample2dist.rpm: sample2 libhelper.a sample2.rpminfo
	rpm_create -i sample2.rpminfo -o sample2.rpm


