#!/usr/bin/env python
# -*- coding: utf-8 -*-  

HOST = "localhost"
PORT = 4223
UID = "6K8nHd" # Change to your UID

from tinkerforge.ip_connection import IPConnection
from tinkerforge.brick_imu import IMU
import time
import csv
import pickle

# Quaternion callback
def cb_all_data(a, b, c, d, e, f, g, h, i, j):
    global row
    row = []
    row = row + [a,b,c,d,e,f,g,h,i,j]
    

def cb_orientation_data(roll, pitch, yaw):
    global row
    row = row + [roll,pitch,yaw]

def cb_quaternion_data(x,y,z,w):
    global row
    global w1
    row = row + [x,y,z,w]
    w1.writerow(row)

def collect_data(suffix):
    global w1
    global row
    print "Now recording " + suffix

    ipcon = IPConnection() # Create IP connection
    imu = IMU(UID, ipcon) # Create device object

    ipcon.connect(HOST, PORT) # Connect to brickd
    # Don't use device before ipcon is connected

    # Set period for quaternion callback to 1s
    imu.set_all_data_period(10)
    imu.set_orientation_period(10)
    imu.set_quaternion_period(10)    
   
    f1 = open('data/letters/all_data_'+suffix+'.csv', 'wb')
    w1 = csv.writer(f1)
    row = []
    # Register quaternion callback
    imu.register_callback(imu.CALLBACK_ALL_DATA, cb_all_data)
    imu.register_callback(imu.CALLBACK_ORIENTATION, cb_orientation_data)
    imu.register_callback(imu.CALLBACK_QUATERNION, cb_quaternion_data)   
  
    
    raw_input('Press key to quit recording ' + suffix + ' \n') # Use input() in Python 3
    ipcon.disconnect()

if __name__ == "__main__":
    try:
        letter_dict = pickle.load(open('data/letter_dict.p', 'rb'))
    except:
        letter_dict = {}
    try:
        while True:
            w1 = []
            row = []

    	    letter = raw_input('Press any key to start recording\n')
            if letter in letter_dict.keys():
                letter_dict[letter] = letter_dict[letter] + 1    	        		   
            else:
                letter_dict[letter] = 1
  	    collect_data(letter + "_" + str(letter_dict[letter]))
            pickle.dump(letter_dict, open('data/letter_dict.p', 'wb'))
    except KeyboardInterrupt:
        print "Quitting Training..."
        pass

