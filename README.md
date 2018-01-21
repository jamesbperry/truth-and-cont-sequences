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

The difficulty? Volume!

TODO: example, Microsoft/FB data resolution compression

## Drilling for oil

Non-time-based data

## Continuous data sequences
As compared to time-series, e.g. FsharpX TimeSeries
As compared to unweighted, e.g. pandas

# Semantics
