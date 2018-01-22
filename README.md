# Tacs — Truth and Cont. Sequences

**Tacs** is an open source library for interpreting and analyzing **continuous sequences** of data. It treats continuous data as *signals,* not *samples* by observing some necessary semantics. This gives us a far more appropriate means of reasoning over data like IoT sensor readings, and means our conclusions are likely to be much more accurate.

There is (or will be) functionality for:
* **remodeling** discrete samples as the intervals they represent (e.g. varying time slices)
* **slicing** the data by length or count (e.g. from midnight to 5:00 today)
* **aggregating** with correct weighting (e.g. getting a total by taking the integral versus naively summing certain points) 
* **sampling** to specified points or resampling to uniform intervals (invaluable when moving sensor data into data science or BI platforms)
* **compressing** the data by strategies like *piecewise linear within an accuracy bound*, helping to overcome the problem of volume while preserving meaning

Sensor readings are a common form of continuous data. Interpreting those readings isn't as easy as it sounds. Real-world sensors often decide when to send values (so the values are of arbitrary spacing), sometimes *can't* send values (so there are gaps), and always lie just a little bit.

Sensors certainly aren't the only source of continuous data! But they're ubiquitous and easy to relate to, so we'll start there.

# Background

## Lies, damn lies, and sensor data
Sensors don't *intentionally* fib, of course... they're just lousy at telling the exact truth. Take a look through the [terminology](http://www.ce.utexas.edu/prof/Novoselac/classes/CE397/Handouts/SI_F09_Ch36.pdf). Most of the terms define types of inaccuracy, which include: 
* accuracy 
* deadband 
* drift 
* hysteresis
* linearity

At levels above the physical transducer, new types of uncertainty await: 
* transfer function
* digital resolution
* sampling strategy
* network connectivity

This combines into an **uncertainty** about the reported values (assuming the sensor works and reports any values at all). While unfortunate in many ways, this means the data is very compressible. How could a sensor with maximum 1% accuracy and a 12-bit analog-to-digital converter possibly create 64 bits of real information, sample after sample? 

Despite all the inaccuracy and variability, readings from commercial sensors are often transmitted as JSON, stored as documents, and aggregated as if they're uniform time slices. So as much as the sensors lie, we on the receiving end are typically just as culpable.

## Datacenters: server monitoring
Continuous data comes from things that aren't physical sensors, too. Our next example is the monitoring data collected by commercial datacenters. Thousands of rack-mounted servers are constantly reporting their utilization and health. That information is invaluable when making strategic decisions about pricing, capacity, and purchasing. It's not crucial data for operations (where the salient observation is a simple "server is failing/failed")... so the individual is unimportant compared to the population.

The biggest challenge there? Volume. Specialized databases address this: [Berengei](https://code.facebook.com/posts/952820474848503/beringei-a-high-performance-time-series-storage-engine/) and [InfluxDB](https://en.wikipedia.org/wiki/InfluxDB), for example. The former uses "delta-of-delta" encoding, which is [nicely explained in section 4.1](http://www.vldb.org/pvldb/vol8/p1816-teller.pdf) of the paper on its predecessor.

## Time-series data
In the datacenter case, two things greatly simplify the situation: 
* individual readings aren't mission-critical (i.e. we're after the big picture)
* the readings are evenly spaced (or considered as such)
* closed-world assumption (no value at a timestamp? -> no data then)

These may not seem like a big deal, but in fact are *huge.* We can use traditional stream-processing techniques: windowing the values and disregarding their timestamps within the ensuing calculation. Traditional record-oriented aggregations fit as well: SELECTing a time range, then GROUPing and AVERAGEing the values. Aside from volume, this data isn't so uncomfortable!

This is known as *time series data* - evenly-spaced, timestamped values., usually with a closed-world assumption. Time series data also has some form of support in most analysis envirionments: R, Python, MATLAB, SPSS, SAS, and the like.

## Continuous data
Inconveniently, sensors don't produce time series information. They may yield time series *data samples,* but that's not quite the same. The earlier section on sensor data mentioned that
* sensor data is often compressible ("12 bit ADC")
* sensor data can have gaps ("the equipment was off")
* sensor data gets sampled according to some strategy, ("every 5 minutes")

all of which are true. But there's more. Consider:

#### A thermostat is reporting its status {off, heat, cool}. 

* Do I really care about all of the readings where the value didn't change? A thousand consecutive samples of *Heat, heat, heat, heat, heat*... spare me!

* Why is the thing even *sending* those unnecessary readings? Can't it wait to tell me when the value changes? (Answer: yes, this is common.)

* If it *is* smart and sends data only as-needed, how do I know the thing is still alive after a week of lovely, mild weather? (Answer: this is the reason for heartbeats, which may be in the form of otherwise "unnecessary" readings.)

#### We are looking at the history of a refrigerator's actual temperature and desired temperature. Unnecessary values were eliminated at some point.
* The *desired temperature* was 1° at 1:00 and 3° at 3:00. Was it 2° at 2:00?

        Hopefully not. It would be silly to assume linear interpolation here. The most logical assumption is that the desired temperature was 1° until it was changed to 3°. 

* The actual temperature was also 1° at 1:00 and 3° at 3:00. Was *it* 2° at 2:00?

        Sure! Er... possibly? In fact, this completely depends on how and when the "unnecessary values were eliminated." 
        
        Did somebody discard the values in between because linear interpolation was accurate enough as the fridge gradually warmed? 
        
        Or did somebody open the fridge door at 3:00, causing it to experience (and report!) an immediate change to 3°?

#### A flow meter is measuring water usage. It gets measured every 60 seconds.
* Many of the readings show a delta of <1%. The meter is accurate to within 2% of its range at best. Do those readings contain any meaningful *information*?

        Noise is nothing more than a heartbeat.

* Linear interpolation seems like a reasonable way to impute what happened between flow readings... but a lot can happen in the span of 60 seconds! Are we really measuring the physical phenomenon frequently enough to capture its behavior?

        Flow can change rapidly as valves open and close. This general concept is called its Nyquist frequency, which tells us how fast a physical phenomenon can change. If the sensor readings aren't frequent enough, then it's not fair to assume what happens between them.


//TODO - data gaps

//TODO - all of this
### SmartThings

A great example of real-world sensor data that gets reported as-needed

### Libraries

As compared to time-series, e.g. FsharpX TimeSeries
As compared to unweighted, e.g. pandas


## Drilling for oil

Non-time-based data

# Semantics

## Thinking in intervals
How and why Tacs remodels from points to intervals (and vice versa).