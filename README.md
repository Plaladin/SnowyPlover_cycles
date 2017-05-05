# SnowyPlover_cycles
Within this project we are analyzing [hypotheses](https://docs.google.com/document/d/15DifWNqtSYtvQbsx7GWgef-I5H0ShBLK6qTi_maQf08/edit?usp=sharing) linked to snowy plover nest initiation.

_CeutaData.csv_

|column | definition|
|------:|:----------|
pk| unique ID of each row
Year| (2006-2016, excl. 2014)
ID | (Year + Nesting Site + Nest Nr. merged together)
FD | Found Date 
FDY | Found Date in Julian Days
FDZ | Found Date standardized
LD | Laying Date (if Fate = Hatch, then ED - 25 days, otherwise predicted on floating stage of eggs)
LDY |Laying Date in Julian Days
LDZ| Laying Date standardized
ED | End Date
EDY | End Date in Julian Days
EDZ | End Date standardized
F | Fate (Hatched, Unhatched, Abandoned, Predated, Flooded, Trampled, Broken, Unknown)
T | Maximum Temperature at the given date
MP | Time of Lunar Meridian Passing (NA - indicates no meridian passing on the given date)
IMP| Illumination at Meridian Passing (raw data)
DaNM | days after the last new moon for a given date
Inoon | Illumination at 12 pm for a given date (interpolated Illumination at Meridian Passing)
TH| Tide height in cm (these are only the heights of the highest high tide for a given day)
ST| Springtide (if on the given date a spring tide happened, then there is the height of the spring tide written, else NA. Spring tides correlate with new moon and full moon.)
DaST | Days after the last Spring tide
