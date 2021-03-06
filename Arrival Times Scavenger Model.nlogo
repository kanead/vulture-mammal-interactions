;;------------------------------------------------------------------------------------------------------;;
;;                                             FORAGING MODEL                                           ;;
;;------------------------------------------------------------------------------------------------------;;
globals [day x y]
;; This section creates the species and gives them some variables
;; I have 3 avian species and 2 terrestrial mammals and a carcass agent
breed [carcasses carcass]
breed [vultures vulture] ;; White-backed Vulture
breed [lappets lappet] ;; Lappet-faced Vulture
breed [eagles eagle] ;; Tawny and Bateleur
breed [hyenas hyena] ;; Spotted hyena
breed [jackals jackal] ;; Jackals
;; dominance hierarchy from strongest to weakest is:
;; hyena > jackal > lappet > vulture > eagles
;; .34 > .33 > .32 > .31 > .3

vultures-own [ mycarcass food-energy capacity  meat fitness got-here homex homey]
lappets-own [  mycarcass food-energy capacity  meat fitness got-here]
eagles-own [  mycarcass food-energy capacity  meat fitness got-here]
hyenas-own [   mycarcass food-energy capacity fitness got-here homex homey]
jackals-own [ mycarcass food-energy capacity fitness got-here homex homey]
carcasses-own [decomp food-energy mycarcass meat capacity]
patches-own [status]
;;------------------------------------------------------------------------------------------------------;;
;;                                          SETUP COMMANDS                                              ;;
;;------------------------------------------------------------------------------------------------------;;
;; Here all of the species are randomly distributed in the enviornment
;; Their capacity is the amount of food they can eat in a day and is in kg
to setup

  clear-all


   ifelse clumped? [

     ask one-of patches
     [
     sprout-vultures N-vultures
        [
        set xcor xcor + random 6
        set ycor ycor + random 6
        set homex xcor
        set homey ycor
  ifelse competition?  [set size 0.31][set size 0.3]
        set color grey
        set food-energy 0
        set capacity 1.5
        ]
     ]

        create-lappets N-lappets
  ask lappets [
ifelse competition?  [set size 0.32][set size 0.3]
  set color grey
    set food-energy 0
    set capacity 1.5
   setxy random-xcor random-ycor
  ]

         create-eagles N-eagles
  ask eagles [
ifelse competition?  [set size 0.3][set size 0.3]
  set color grey
    set food-energy 0
    set capacity 0.5
  setxy random-xcor random-ycor
  ]

ask one-of patches
     [
     sprout-hyenas N-hyenas
        [
        set xcor xcor + random 6
        set ycor ycor + random 6
         set homex xcor
        set homey ycor
ifelse competition?  [set size 0.34][set size 0.3]
        set color grey
        set food-energy 0
        set capacity 6
        ]
     ]

ask one-of patches
    [
     sprout-jackals N-jackals
        [
        set xcor xcor + random 6
        set ycor ycor + random 6
         set homex xcor
        set homey ycor
ifelse competition?  [set size 0.33][set size 0.3]
        set color grey
        set food-energy 0
        set capacity 1
        ]
     ]
   ]

   [


 create-vultures N-vultures
  ask vultures [
ifelse competition?  [set size 0.31][set size 0.3]
  set color grey
    set food-energy 0
    set capacity 1.5
   setxy  random-xcor random-ycor  ;random 4 random 4
  ]

    create-hyenas N-hyenas
  ask hyenas [
ifelse competition?  [set size 0.34][set size 0.3]
  set color grey
    set food-energy 0
    set capacity 6
  setxy random-xcor random-ycor
  ]

  create-jackals N-jackals
 ask jackals [
ifelse competition?  [set size 0.33][set size 0.3]
  set color grey
    set food-energy 0
    set capacity 1
  setxy random-xcor random-ycor
  ]

       create-lappets N-lappets
  ask lappets [
ifelse competition?  [set size 0.32][set size 0.3]
  set color grey
    set food-energy 0
    set capacity 1.5
   setxy random-xcor random-ycor
  ]

         create-eagles N-eagles
  ask eagles [
ifelse competition?  [set size 0.3][set size 0.3]
  set color grey
    set food-energy 0
    set capacity 0.5
  setxy random-xcor random-ycor
  ]
   ]

  reset-ticks
end

;;------------------------------------------------------------------------------------------------------;;
;;                                            GO COMMANDS                                               ;;
;;------------------------------------------------------------------------------------------------------;;
to go
  if ticks = day-length  [set day day + 1 create-next-day]
  if day = 100 [stop] ;; the simulation stops at day 100

  ;; the results are recorded at the end of each day and are saved as resultsx.csv where x is some random float number
  ;; to ensure the files aren't overwritten

if ticks = day-length - 5 [export-world (word "results" random-float 1.0 ".csv")]


;; The carcasses decay every day if not eaten and reappear the following day
;; They are randomly distributed

if ticks = 0 [create-carcasses N-carcasses
 [  ; setxy random 5  random 5
    setxy random-xcor random-ycor
    set color yellow
    set shape "target"
    set food-energy 0
    ask carcasses with [shape = "square"] [die]
    ]]





  ask carcasses [
  if ticks = 100  [set shape "circle"]
check
set decomp decomp + 1
if decomp = day-length  [die]
if color = yellow [check]
if color = orange [decay]
set size 0.2
if meat <= 0 [set shape "triangle"]

]



  ask vultures
 [
  if color = grey [forage-vul]
  if color = yellow [feed-vul]
  if color = blue [consume-vul]
  if color = red [full-vul]
 ]

  ask lappets
 [
  if color = grey [forage-lappet]
  if color = yellow [feed-lappet]
  if color = blue [consume-lappet]
  if color = red [full-lappet]
 ]


  ask eagles
 [
  if color = grey [forage-eagle]
  if color = yellow [feed-eagle]
  if color = blue [consume-eagle]
  if color = red [full-eagle]
 ]


ifelse nocturnal? [

if ticks > time
[
  ask hyenas
 [
  if color = grey [forage-hyena]
  if color = yellow [feed-hyena]
  if color = blue [consume-hyena]
  if color = red [full-hyena]
 ]

   ask jackals
 [
  if color = grey [forage-jackal]
  if color = yellow [feed-jackal]
  if color = blue [consume-jackal]
  if color = red [full-jackal]
  ]
]
]

  [
  ask hyenas
 [
  if color = grey [forage-hyena]
  if color = yellow [feed-hyena]
  if color = blue [consume-hyena]
  if color = red [full-hyena]
 ]

   ask jackals
 [
  if color = grey [forage-jackal]
  if color = yellow [feed-jackal]
  if color = blue [consume-jackal]
  if color = red [full-jackal]
  ]]



 ask turtles with [shape = "default"] [if ticks = 0  [calculate-fitness]]

 tick
end
;;---------------------------------------------------------------------------------------------;;
;;                                 VULTURE COMMANDS                                            ;;
;;---------------------------------------------------------------------------------------------;;
;; All of the species move around at their velocity in their search for carrion, vision represents
;; the visual radius of the birds.
;; All of the animals have a size value which determines their competitive ability, at the moment if
;; there is a species larger than themselves on a carcass they won't move towards it

to forage-vul
  fd v-vulture
   if random 200 = 1
  [ ifelse random 2 = 0
    [ rt 45 ]
    [ lt 45 ]]

  if any? turtles with [shape = "circle"] in-cone vul-vis 286 and not any? turtles in-cone 1 286 with [ size > [ size ] of myself and color != white]
  [ face min-one-of turtles with [shape = "circle"] in-cone vul-vis 286 [distance myself]
  ifelse info-transfer? and any? turtles with [shape = "circle"] in-cone 1 286 [set shape "star"] [set shape "default"] ;; improved following by hyenas requires this shape change
  ]
  if any? turtles with [shape = "circle"] in-radius 0.1
   [set color yellow
     set shape "default" ;; improved following by hyenas requires this shape change
   ]
 end

to feed-vul
   move-to min-one-of turtles with [shape = "circle"] in-radius vul-vis [distance myself]
      set mycarcass min-one-of turtles with [shape = "circle"] in-radius vul-vis [distance myself]
    set color blue
    set got-here ticks
end


to consume-vul
  ask vultures with [color = blue] [
    set color red
    ]
end

to full-vul
ifelse (food-energy / capacity) < 1 and [meat] of mycarcass > capacity [
  set food-energy  1.5] [set food-energy [meat] of mycarcass
  ask mycarcass [set shape "square"]]
if (food-energy / capacity) = 1 [
  set color white]
if (food-energy > 0 and food-energy / capacity < 1)
[ set color white ]
  end



;;---------------------------------------------------------------------------------------------;;
;;                                 LAPPET COMMANDS                                             ;;
;;---------------------------------------------------------------------------------------------;;
to forage-lappet
  fd v-lappet
   if random 200 = 1
  [ ifelse random 2 = 0
    [ rt 45 ]
    [ lt 45 ]]

  if any? turtles with [shape = "circle"] in-cone vul-vis 286 and not any? turtles in-cone 1 286 with [ size > [ size ] of myself and color != white]
  [ face min-one-of turtles with [shape = "circle"] in-cone vul-vis 286 [distance myself]
  ifelse info-transfer? and any? turtles with [shape = "circle"] in-cone 1 286 [set shape "star"] [set shape "default"] ;; improved following by hyenas requires this shape change
  ]
  if any? turtles with [shape = "circle"] in-radius 0.1
   [set color yellow
     set shape "default" ;; improved following by hyenas requires this shape change
   ]
 end

to feed-lappet
   move-to min-one-of turtles with [shape = "circle"] in-radius lap-vis [distance myself]
      set mycarcass min-one-of turtles with [shape = "circle"] in-radius lap-vis [distance myself]
    set color blue
    set got-here ticks
end

to consume-lappet
  ask lappets with [color = blue] [
    set color red
    ]
end

to full-lappet
ifelse (food-energy / capacity) < 1 and [meat] of mycarcass > capacity [
  set food-energy  1.5] [set food-energy [meat] of mycarcass
  ask mycarcass [set shape "square"]]
if (food-energy / capacity) = 1 [
  set color white]
if (food-energy > 0 and food-energy / capacity < 1)
[ set color white ]
  end


;;---------------------------------------------------------------------------------------------;;
;;                                   EAGLE COMMANDS                                            ;;
;;---------------------------------------------------------------------------------------------;;
to forage-eagle
  fd v-eagle
   if random 200 = 1
  [ ifelse random 2 = 0
    [ rt 45 ]
    [ lt 45 ]]

  if any? turtles with [shape = "circle"] in-cone vul-vis 286 and not any? turtles in-cone 1 286 with [ size > [ size ] of myself and color != white]
  [ face min-one-of turtles with [shape = "circle"] in-cone vul-vis 286 [distance myself]
  ifelse info-transfer? and any? turtles with [shape = "circle"] in-cone 1 286 [set shape "star"] [set shape "default"] ;; improved following by hyenas requires this shape change
  ]
  if any? turtles with [shape = "circle"] in-radius 0.1
   [set color yellow
     set shape "default" ;; improved following by hyenas requires this shape change
   ]
 end

to feed-eagle
   move-to min-one-of turtles with [shape = "circle"] in-radius eagle-vis [distance myself]
      set mycarcass min-one-of turtles with [shape = "circle"] in-radius eagle-vis [distance myself]
    set color blue
    set got-here ticks
end

to consume-eagle
  ask eagles with [color = blue] [
    set color red
    ]
end

to full-eagle
ifelse (food-energy / capacity) < 1 and [meat] of mycarcass > capacity [
  set food-energy  0.5] [set food-energy [meat] of mycarcass
  ask mycarcass [set shape "square"]]
if (food-energy / capacity) = 1 [
  set color white]
if (food-energy > 0 and food-energy / capacity < 1)
[ set color white ]
  end

;;---------------------------------------------------------------------------------------------;;
;;                                   HYENA COMMANDS                                            ;;
;;---------------------------------------------------------------------------------------------;;
to forage-hyena
  fd v-hyena
   if random 500 = 1
  [ ifelse random 2 = 0
    [ rt 45 ]
    [ lt 45 ]]

if any? turtles with [shape = "star"] in-cone 1 250
[ face min-one-of turtles with [shape = "star"] in-cone 1 250 [distance myself] ;; added this to improve hyena local enhancement 19/05/2016
  ]

  if any? turtles with [shape = "circle"] in-radius hy-vis
  [ face min-one-of turtles with [shape = "circle"] in-radius hy-vis [distance myself]
  ]
  if any? turtles with [shape = "circle"] in-radius 0.1
   [set color yellow
   ]
 end

to feed-hyena
   move-to min-one-of turtles with [shape = "circle"] in-radius hy-vis [distance myself]
      set mycarcass min-one-of turtles with [shape = "circle"] in-radius hy-vis [distance myself]
    set color blue
    set got-here ticks
end

to consume-hyena
  ask hyenas with [color = blue] [
    set color red
    ]
end

to full-hyena
ifelse (food-energy / capacity) < 1 and [meat] of mycarcass > capacity [
  set food-energy  6] [set food-energy [meat] of mycarcass
  ask mycarcass [set shape "square"]]
if (food-energy / capacity) = 1 [
  set color white]
if (food-energy > 0 and food-energy / capacity < 1)
[ set color white ]
  end

;;---------------------------------------------------------------------------------------------;;
;;                                  JACKAL COMMANDS                                            ;;
;;---------------------------------------------------------------------------------------------;;
to forage-jackal
  fd v-jackal
   if random 500 = 1
  [ ifelse random 2 = 0
    [ rt 45 ]
    [ lt 45 ]]

if any? turtles with [shape = "star"] in-cone 1 250
[ face min-one-of turtles with [shape = "star"] in-cone 1 250 [distance myself] ;; added this to improve jackal local enhancement 29/11/2016
  ]

  if any? turtles with [shape = "circle"] in-cone jack-vis 250 and not any? turtles in-cone 1 250 with [ size > [ size ] of myself and color != white ]
  [ face min-one-of turtles with [shape = "circle"] in-cone jack-vis 250 [distance myself]
  ]
  if any? turtles with [shape = "circle"] in-radius 0.1
   [set color yellow
   ]
 end

to feed-jackal
   move-to min-one-of turtles with [shape = "circle"] in-radius jack-vis [distance myself]
      set mycarcass min-one-of turtles with [shape = "circle"] in-radius jack-vis [distance myself]
    set color blue
    set got-here ticks
end

to consume-jackal
  ask jackals with [color = blue] [
    set color red
    ]
end

to full-jackal
ifelse (food-energy / capacity) < 1 and [meat] of mycarcass > capacity [
  set food-energy  1] [set food-energy [meat] of mycarcass
  ask mycarcass [set shape "square"]]
if (food-energy / capacity) = 1 [
  set color white]
if (food-energy > 0 and food-energy / capacity < 1)
[ set color white ]
  end
;;------------------------------------------------------------------------------------------------------;;
;;                                        CARCASS COMMANDS                                              ;;
;;------------------------------------------------------------------------------------------------------;;
to check
ifelse any? turtles-here with [shape = "default"]
[set color orange]
[set color yellow]
ask carcasses [set meat 100 - food-energy]
end

to decay
  if any? turtles-here [

    set food-energy
    (1.5 * count vultures-here with  [ color = white])
    + (6 * count hyenas-here with  [color = white])
    + (1.5 * count lappets-here with  [color = white])
    + (0.5 * count eagles-here with  [color = white])
    + (1 * count jackals-here with  [color = white])]

 end

;;------------------------------------------------------------------------------------------------------;;
;;                                        RESET COMMANDS                                                ;;
;;------------------------------------------------------------------------------------------------------;;
;; This calculates the amount of energy the scavengers acquired
;; over the course of the day and resets the day

to calculate-fitness
  set fitness food-energy
end

to create-next-day
  clear-links
  reset-ticks

ifelse clumped? [
  ask vultures  [
  setxy homex homey
;  setxy  random-xcor random-ycor
  set color grey
  set food-energy 0
  set mycarcass 0
  set food-energy (food-energy * 7000)
  set got-here 0
 ]

 ask lappets  [
  setxy random-xcor random-ycor
  set color grey
  set food-energy 0
  set mycarcass 0
  set food-energy (food-energy * 7000)
  set got-here 0
 ]

 ask eagles  [
  setxy random-xcor random-ycor
  set color grey
  set food-energy 0
  set mycarcass 0
  set food-energy (food-energy * 7000)
  set got-here 0
 ]

 ask hyenas  [
  setxy homex homey
; setxy  random-xcor random-ycor
  set color grey
  set food-energy 0
  set mycarcass 0
  set food-energy (food-energy * 7000)
  set got-here 0
 ]

 ask jackals  [
  setxy homex homey
; setxy  random-xcor random-ycor
  set color grey
  set food-energy 0
  set mycarcass 0
  set food-energy (food-energy * 7000)
  set got-here 0
 ]


]
[


  ask vultures  [

  setxy  random-xcor random-ycor
  set color grey
  set food-energy 0
  set mycarcass 0
  set food-energy (food-energy * 7000)
  set got-here 0
 ]

 ask lappets  [
  setxy random-xcor random-ycor
  set color grey
  set food-energy 0
  set mycarcass 0
  set food-energy (food-energy * 7000)
  set got-here 0
 ]

 ask eagles  [
  setxy random-xcor random-ycor
  set color grey
  set food-energy 0
  set mycarcass 0
  set food-energy (food-energy * 7000)
  set got-here 0
 ]

 ask hyenas  [

 setxy  random-xcor random-ycor
  set color grey
  set food-energy 0
  set mycarcass 0
  set food-energy (food-energy * 7000)
  set got-here 0
 ]

 ask jackals  [

 setxy  random-xcor random-ycor
  set color grey
  set food-energy 0
  set mycarcass 0
  set food-energy (food-energy * 7000)
  set got-here 0
 ]
]
 go
end

;;------------------------------------------------------------------------------------------------------;;
;;                                           END OF MODEL                                               ;;
;;------------------------------------------------------------------------------------------------------;;
@#$#@#$#@
GRAPHICS-WINDOW
210
12
680
503
-1
-1
23.0
1
10
1
1
1
0
1
1
1
0
19
0
19
0
0
1
ticks
30.0

BUTTON
15
16
82
49
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
88
16
151
49
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
7
61
179
94
N-vultures
N-vultures
0
300
116
1
1
NIL
HORIZONTAL

MONITOR
37
548
94
593
NIL
day
17
1
11

SLIDER
208
602
380
635
vul-vis
vul-vis
0
20
6
0.5
1
km
HORIZONTAL

INPUTBOX
9
427
164
487
day-length
21600
1
0
Number

INPUTBOX
211
534
284
594
v-vulture
0.0123
1
0
Number

TEXTBOX
683
554
826
586
45km/hr = 0.0125km/sec\n33km/hr = 0.0092km/sec\n
12
0.0
1

SLIDER
8
183
180
216
N-hyenas
N-hyenas
0
100
42
1
1
NIL
HORIZONTAL

INPUTBOX
446
534
528
594
v-hyena
0.0028
1
0
Number

SLIDER
383
602
555
635
lap-vis
lap-vis
0
20
7
0.5
1
km
HORIZONTAL

INPUTBOX
364
534
446
594
v-eagle
0.0124
1
0
Number

INPUTBOX
284
534
364
594
v-lappet
0.0139
1
0
Number

INPUTBOX
527
534
609
594
v-jackal
0.0033
1
0
Number

SLIDER
7
219
179
252
N-jackals
N-jackals
0
300
65
1
1
NIL
HORIZONTAL

SLIDER
8
99
180
132
N-lappets
N-lappets
0
100
8
1
1
NIL
HORIZONTAL

SLIDER
8
141
180
174
N-eagles
N-eagles
0
100
80
1
1
NIL
HORIZONTAL

SLIDER
8
301
180
334
N-carcasses
N-carcasses
0
100
18
1
1
NIL
HORIZONTAL

TEXTBOX
14
350
164
430
Numbers\n116 vul; 8 lapp; 80 eagles; \n42 hyenas; 65 jackals; \n18 carcasses
12
0.0
1

TEXTBOX
834
529
984
601
v-vulture = 0.01; \nv-hyena = 0.002; \nv-eagle = 0.009; \nv-lappet = 0.01; \nv-jackal = 0.0015;
12
0.0
1

TEXTBOX
20
496
138
544
43200 = 12 hours\n36000 = 10 hours\n21600 - 6 hours
12
0.0
1

SLIDER
206
638
378
671
eagle-vis
eagle-vis
0
20
8
0.5
1
km
HORIZONTAL

SLIDER
562
640
734
673
hy-vis
hy-vis
0
10
1
0.5
1
km
HORIZONTAL

SLIDER
559
602
731
635
jack-vis
jack-vis
0
10
1
0.5
1
km
HORIZONTAL

TEXTBOX
621
563
673
581
km/second
11
0.0
1

TEXTBOX
137
601
196
671
vul - 4 \nlap - 6\neagle - 5.5\njack - 0.5 \nhyena - 0.5
11
0.0
1

SWITCH
9
598
115
631
clumped?
clumped?
0
1
-1000

SWITCH
-1
635
131
668
info-transfer?
info-transfer?
0
1
-1000

SWITCH
4
669
129
702
competition?
competition?
0
1
-1000

TEXTBOX
1098
537
1468
629
Daily flight duration (h between two successive roosts)\nLappet: ~ 6 hours\nAWBV: ~ 5 hours 15 mins \n\nHyena spends 7 hours 30 mins foraging but only 10% of this is diurnal activity
11
0.0
1

TEXTBOX
1018
249
1168
333
VISUAL FIELDS\nvulture = 286\nlappet = 286\neagle = 260\njackals = 250\nhyenas = 250
11
0.0
1

SWITCH
1
708
114
741
nocturnal?
nocturnal?
1
1
-1000

TEXTBOX
826
247
976
359
EMPIRICAL SPEEDS\n0.0123 vulture = 44.4km/hr\n0.0139 lappet = 50.3km/hr\n0.0124 eagle = 44.6km/hr\n0.0028 hyena = 10km/hr\n0.0033 jackal = 12km/hr\n\n\n
11
0.0
1

TEXTBOX
438
650
588
734
VISION\nvulture 12\nlappet 14\neagle 16\njackal 0.5\nhyena 0.5\n
11
0.0
1

MONITOR
735
10
969
55
NIL
count carcasses with [shape = \"target\"]
17
1
11

CHOOSER
120
704
258
749
time
time
18000 10800
1

@#$#@#$#@
4.38 kg/km2/day
10000km2 habitat
4.38*10000 = 43800kg/day = 43.8 tonnes/day
Let's say our carcasses are 500kg each
43800/500 = 87.6 or 88 carcasses in the simulation space per day

2500km2 habitat
4.38*2500 = 10950kg/day
Let's say our carcasses are 500kg each
10950/500 = 21.9 or 22 carcasses in the simulation space per day

This environment is smaller at 900km2
4.38*900 = 3942kg/day ~ 4000kg/day
Let's say our carcasses are 100kg each
4000/100 = 40 carcasses in the simulation space per day

Let's try smaller again at 400km2
4.38*400 = 1752kg/day ~ 1800kg/day
Let's say our carcasses are 100kg each
1800/100 = 18 carcasses in the simulation space per day



## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

Mean cross country speed for Steppe Eagle is 12.4m/s
Soaring Migration of Steppe Eagles Aquila nipalensis in Southern Israel: Flight behaviour under Various Wind and Thermal Conditions. Reto Spaar and Bruno Bruderer

Flight speed for Lappet is 44.4km/hr and for AWBV it is 50.3km/hr Spiegel et al. 2013.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.2.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>day = 2</exitCondition>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="N-vultures">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-lappets">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-eagles">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-hyenas">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-jackals">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="day-length">
      <value value="43200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-smallcarcasses">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-largecarcasses">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-vulture">
      <value value="0.0125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-lappet">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-eagle">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-hyena">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-jackal">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vul-vis">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lap-vis">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eagle-vis">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hy-vis">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jack-vis">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
