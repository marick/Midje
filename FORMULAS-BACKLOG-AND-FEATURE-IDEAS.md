
* [x] improve formula failure reporting
   a. [x] report the first failure
   b. [x] make sure the fact count only increases once per formula  

* [x] make number of generated facts per formula dynamically bindable
   a. [x] throw an exception if this value is set <= 1

* [x] figure out how to make syntax validation errors show something more sensible than the 
      error message you'd see for a problem with a fact validation problem 

* [x] don't run more tests than need be if there is already failure in this formula's batch.
  
* [ ] Work with Meikel Brandmeyer to combine ClojureCheck's Generators with Shrink.
      implement shrinking. Report only the first fully shrunken failure
         [ ] to do correctly, 'shrink' depends on 'generate'

* [ ] shrink function overrides
   a. [ ] no shrink
     
* [ ] fix strange error if you run (formula [a 1] 1 =>)
      ... since the formula macro splices in :formula :formula-in-progress
      possibly solution is to not using fact macro inside of formula, 
      but instead do something like tabular
          
 
* [ ] if line numbers shift, then ensure that they always report correctly -- so far I don't know if this even needs to change, since it seems to work fine.  Think about it and decide if tests to prevent regressions are useful here.

* [ ] consider implementing with @marick's metaconstant syntax
   a. [ ] if we do metaconstant style, implement generator overriding