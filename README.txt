LHC1: lhcData
N:		4000
SCC: 		0, 75
spike: 		0, 1.5
production: 	0, 1.5
revenue:	0, 1.5
e:		-1.0, -0.1
Notes: limited solution range to 0-50, which turned out to be too small. I also forgot to set the seed for the LHC
so that won't be reproducible :( I also forgot to include the load groups, so it just used a single load group, damn.
I also used ramp=5 and rough.pc density of 500 points


LHC2: lchDataWide
N:		1000
SCC: 		0, 75
spike: 		0, 5
production: 	0, 5
revenue:	0, 5
e:		-1.0, -0.1
Notes: I want to see if good fits will be all over the place, or concentrated. Made range 0-150 and ramp=0.5, and pc.rough
density 100 points.
C
LH3: lhcDataWide1
N:		1000
SCC: 		0, 200
spike: 		0, 10
production: 	0, 10
revenue:	0, 10
e:		-1.0, -0.1
Notes: I used a ramp of 0.5, a rough pc of 0-250 by 1. 

LHC4: lhc4
Description:	Test very wide range for spike and revenue and elasticity
N:		1000
SCC: 		50	# SCC doesn't have large impact, probably due to low elasticity.
spike: 		0, 200
production: 	0	# This parameter never seems to matter much. Likely due to low elasticity range
revenue:	0, 200
e:		-1.0, -0.1
Notes: I used a ramp of 0.5, a rough pc of 0-150 by 3 (this is exploratory). 

LHC5: lhc5
Description:	Lhc4 didn't work out so well, big ranges are bad. This expands a bit on LHC3
N:		4000
SCC: 		0, 150	# SCC doesn't have large impact, probably due to low elasticity.
spike: 		0, 10	# Seems like a good limit based on prior experiments.
production: 	0	# This parameter never seems to matter much. Likely due to low elasticity range
revenue:	0, 50
e:		-0.9, -0.2	# From the literature
Notes: I used a ramp of 0.5, a rough pc of 0-100 by 2. The range is acceptable, it seems rare for values above 100, expect when the hit the limit anyways



