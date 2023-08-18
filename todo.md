+ [ ] measurement unit by input type (N, seed, )
+ [x] trial design
  + [x] sparse design
  + [x] strip design
  + [x] latin square
  + [x] randomized block
+ [x] assign rates
+ [x] options of specifying sequences (within and across strips)
+ [ ] available form options in the website 
+ [x] cut off ab-lines outside of the field boundary with a bit of buffer around the field
+ [x] make harvest ab-line (Taro)
+ [x] let them specify the full sequence (everything is a special case of this except randomized ones) (Taro)
+ [ ] base_rate, conversion, dictionary (Brittani)
+ [ ] rate option (original unit, or equivalent) (Brittani)
+ [ ] Don't produce a planter AB-line when they are planting at a different angle. Make this part of vignette. Just make sure it is "none" (Brittani)
+ [x] check examples
+ [x] explain the structure of experiment plots and trial design, what is the definition of plot?
+ [x] visualize strip id, plot id, block id
+ [x] accept sf as boundary and ab-line
+ [ ] what is stratification by MSU?
+ [x] explain parameters well in the prep functions
+ [x] use past trial design plots and change according to the past trial rates
+ [x] add harvester abline in writing files
+ [x] add explanations of how sequencing work in rate assignment
+ [x] visualize with focus on ab-line
+ [x] transform utm of ab-lines and harvest ab-lines when writing
+ [x] check plot width is the multiple of machine width
+ [x] warning from using geom_sf_text()
+ [ ] two N fertilizers (total N)
+ [x] discrete example? cover crop?
+ [ ] check the orthogonality with past trial design
+ [a] improve the creation of headland (difference of field and experiment)
+ [ ] what to do with blocks?
+ [ ] distribution of input rate in check 
+ [ ] 


# Problems

+ [x] when field is not straight and plot widths are different, rates transitions do not align well
+ [x] harvester ab-line for two-input case
+ [x] what is the white space around the experiment part of the field in readme?

# Vignette

+ [x] full demo
+ [ ] ab-line options
  -  input: as-applied shape file, ab-line file, or angle
  -  lock, free, none 
+ [x] trial design options
  + ls
  + jcls
  + rb
  + strip
  + ejca
+ [x] modify rates
+ [ ] key parameters in make_plots
+ [ ] how rate conversion works with explanation on base_rate
+ [ ] add more real examples to explain workflows
  + [ ] go through the process we indeed went through for a DIFM experiment with iterative processes (change side_length, check map, change side_length, etc) (single input and two input)
  + [ ] create the design Laura uses for her CIG grant (manual)
