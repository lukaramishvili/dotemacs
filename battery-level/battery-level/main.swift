//
//  main.swift
//  battery-level
//
//  Created by Luka Ramishvili on 7/20/19.
//  Copyright © 2019 Luka Ramishvili. All rights reserved.
//

import Foundation

import IOKit.ps


// Take a snapshot of all the power source info
let snapshot = IOPSCopyPowerSourcesInfo().takeRetainedValue()

// Pull out a list of power sources
let sources = IOPSCopyPowerSourcesList(snapshot).takeRetainedValue() as Array

let totalBars:Int = 10

// For each power source...
for ps in sources {
    // Fetch the information for a given power source out of our snapshot
    let info = IOPSGetPowerSourceDescription(snapshot, ps).takeUnretainedValue() as! [String: AnyObject]
    
    // Pull out the name and capacity
    if let name = info[kIOPSNameKey] as? String,
        let capacity = info[kIOPSCurrentCapacityKey] as? Int,
        let max = info[kIOPSMaxCapacityKey] as? Int {
        let percent:Float = Float(capacity)//round(Float(capacity)/10)*10
        let remainingFraction = Int(round(percent*Float(totalBars)/100.0))// e.g. 3 for ~60% battery
        let drainedFraction = totalBars - remainingFraction// 2/5 battery bars drained
        let output:String = String(repeating: "▓", count: remainingFraction) + String(repeating: "░", count: drainedFraction)
        /*for barRemaining in 0..<remainingFraction {
         print("█")
         }
         for barDrained in 0..<drainedFraction {
         print("░")
         }*/
        print(output)
    }
}

