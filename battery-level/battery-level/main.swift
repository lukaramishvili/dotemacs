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

let totalBars:Int = 5

let barFull = "█"
let barMedium = "▓"
let barLight = "▒"// very light: "░"

let lightningSymbol = "⚡"

let space = " "

// For each power source...
for ps in sources {
    // Fetch the information for a given power source out of our snapshot
    let info = IOPSGetPowerSourceDescription(snapshot, ps).takeUnretainedValue() as! [String: AnyObject]
    
    // Pull out the name and capacity
    //if
       
        //let max = info[kIOPSMaxCapacityKey] as? Int
    if let capacity = info[kIOPSCurrentCapacityKey] as? Int {
        let PowerSource = info[kIOPSPowerSourceStateKey] as! String
        let isCharging = PowerSource == "AC Power"//or != "Battery Power"
        let remainingBarWhileCharging:String = isCharging ? barMedium : barFull
        let percent:Float = Float(capacity)//round(Float(capacity)/10)*10
        let remainingFraction = Int(round(percent*Float(totalBars)/100.0))// e.g. 3 for ~60% battery
        let drainedFraction = totalBars - remainingFraction// 2/5 battery bars drained
        let batteryBars = String(repeating: remainingBarWhileCharging, count: remainingFraction) + String(repeating: barLight, count: drainedFraction)
        let percentString = String(Int(percent)) + "%"
        let lightningString = (isCharging ? lightningSymbol : "")
        // e.g. ▓▓▓▒▒ 48% ⚡
        //let output:String = batteryBars + space + percentString + space + lightningString
        // only 48% ⚡(to save space)
        let output: String = percentString + space + lightningString
        
        //print(info)
        print(output)
    }
}

