+ [ ] measurement unit by input type (N, seed, )
+ [ ] trial design
  + [x] sparse design
  + [x] strip design
  + [x] latin square
  + [x] randomized block
+ [x] assign rates
+ [x] options of specifying sequences (within and across strips)
+ [ ] available form options in the website 
+ [x] cut off ab-lines outside of the field boundary with a bit of buffer around the field
+ [x] make harvest ab-line (Taro)
+ [ ] let them specify the full sequence (everything is a special case of this except randomized ones) (Taro)
+ [ ] base_rate, conversion, dictionary (Brittani)
+ [ ] rate option (original unit, or equivalent) (Brittani)
+ [ ] check examples
+ [ ] explain the structure of experiment plots and trial design, what is the definition of plot?
+ [x] visualize strip id, plot id, block id
+ [x] accept sf as boundary and ab-line
+ [ ] what is stratification by MSU?
+ [ ] explain parameters well in the prep functions
+ [ ] use past trial design plots and change according to the past trial rates
+ [ ] add harvester abline in writing files


# Problems

1. when field is not straight and plot widths are different, rates transitions do not align well
2. harvester ab-line for two-input case
3. what is the white space around the experiment part of the field in readme?

# Vignette

+ full demo
+ ab-line options
  -  input: as-applied shape file, ab-line file, or angle
  -  lock, free, none 
+[x] trial design options
  + ls
  + jcls
  + rb
  + strip
  + ejca
+ [x] modify rates
+ [ ] key parameters in make_plots