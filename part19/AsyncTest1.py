#!/usr/bin/env python3
# countasync.py

import asyncio
import threading
import time

tm=threading.Event()
t1=threading.Event()

class count():
    def __init__(self):
        self.count = 0

    def add(self):
        time.sleep(1)
        self.count = self.count+1

    def get(self):
        print(f'Counter: {self.count}')

class testjob1(threading.Thread):
    def __init__(self):
        pass

class testjob(testjob1):

    def __init__(self):
        pass

    def jbm(self, npk):
        print(f"JBM Numurs:{npk}")
        tm.wait() # return immediately when it is True, block until the internal flag is True when it is False
        print("tm1")
        Counter.add()        
        tm.clear()
        t1.set()
        tm.wait()
        print("tm2")
        Counter.add()        
        tm.clear()
        t1.set()
        tm.wait()
        print("tm3")
        Counter.add()        
        tm.clear()
        t1.set()

    def jbs(self, npk):
        print(f"JBS Numurs:{npk}")
        tm.set()
        t1.wait() # return immediately when it is True, block until the internal flag is True when it is False
        print("t11")
        Counter.add()
        t1.clear()
        tm.set()
        t1.wait()
        print("t12")
        Counter.add()
        t1.clear()
        tm.set()
        t1.wait()
        print("t13")
        Counter.add()        
        t1.clear()
        tm.set()


def main():


    global Counter
    Counter=count()

    testjb = testjob()
    # tm.set()

    jm=threading.Thread(target=testjb.jbm, args=(1,))
    j1=threading.Thread(target=testjb.jbs, args=(2,))
    print("1")
    jm.start()
    j1.start()
    jm.join()
    j1.join()
    Counter.get()
    print("2")




if __name__ == "__main__":
    print("01")
    main()
    print("02")

