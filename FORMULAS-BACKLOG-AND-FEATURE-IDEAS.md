
* [x] improve formula failure reporting
   a. [x] report the first failure
   b. [x] make sure the fact count only increases once per formula  

* [x] make number of generated facts per formula dynamically bindable
   a. [x] throw an exception if this value is set <= 1

* [x] figure out how to make syntax validation errors show something more sensible than the 
      error message you'd see for a problem with a fact validation problem 

* [ ] don't run more tests than need be if there is already failure in this formula's batch.
 
* [ ] make line numbers report correctly
  
* [ ] implement shrinking. Report only the first fully shrunken failure
         [  ] to do correctly, 'shrink' depends on 'generate'

* [ ] consider implementing with @marick's metaconstant syntax

* [ ] shrink function overrides
   a. [ ] no shrink
  
* [ ] if we do metaconstant style, implement generator overriding
  
    --- getting there with the new validate method for "formula"
      a. [] fix:  however, there's an interesting form of failure if you run (formula [a 1] 1 =>)
          ... since the formula macro splices in :formula :formula-in-progress
          
          will the below note help this?
          
NOTES:

* possibly don't use fact macro inside of formula, but instead do something like tabular