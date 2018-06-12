# SnowyPlover_cycles
Within this project we are analyzing [hypotheses](https://docs.google.com/document/d/15DifWNqtSYtvQbsx7GWgef-I5H0ShBLK6qTi_maQf08/edit?usp=sharing) linked to snowy plover nest initiation.

To reproduce the code from Silvia Plaschke's Master's Thesis "Testing for semilunar rhythms in nesting schedules of Snowy Plovers" please save the files metadata_nests_birds.csv, metadata_nests_birds_CK.csv, laid_new_2.csv, moonphases.csv, tides_all.csv and illumination_all.csv in your working directory. 
For the data preparation run the R script Prepare_Data.R and Constants_Functions.R .
To reproduce the graphics and model outputs run the R script Analysis_and_Graphics.R .


Descriptions of shortcuts used in the csv files:

|column | definition|
|------:|:----------|
pk| unique ID of each row
year| (2006-2016, excl. 2014)
nest| (Year + Nesting Site + Nest Nr. merged together)
found| Found Date 
laid| Laying Date (if Fate = Hatch, then ED - 25 days, otherwise predicted on floating stage of the youngest egg)
end| End Date
lat| Latitude (GPS Coordinates)
lon| Longitude (GPS Coordinates)
fate| Fate (Hatched, Unhatched, Abandoned, Predated, Flooded, Trampled, Broken, Unknown)
male| male ID
female| female ID
pair| pair ID (male and female merged together)
datetime_| datetime of the respective event
event| moon phases (fq = First Quater, fm= Full Moon, lq= Last Quater, nm= New Moon)
Inoon| Illumination at 12 pm for a given date (interpolated Illumination at Meridian Passing)
max_tide_height| Tide height in cm (these are only the heights of the highest high tide for a given day)
m_start| start of moon cycle (first day of New Moon)
m_end| end of moon cycle
m_cycle| number of moon cycle in the respective year
days_after_nm| days after the last New Moon
st_start| start of spring tide cycle (first day of New Moon)
st_end| end of spring tide cycle 
st_cycle| number of spring tide cycle in the respective year
dast| Days after the last Spring tide
days_after_st| Days after the last Spring tide
dur| duration of spring tide cycle
rad_st| days after last spring tide converted to radians
meridian_passing| datetime of moon passing meridian
illumination_mp| illumination at meridan passing
illumination_noon| illumination at noon

