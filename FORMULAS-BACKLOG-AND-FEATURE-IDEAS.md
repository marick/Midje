
1. [x] improve formula failure reporting
 a. [x] report the first failure
 b. [x] make sure the fact count only increases once per formula
  
2. [ ] implement shrinking.  Report only the first fully shrunken failure

   QUESTION: how can I change the inputs (which need to shrink), when checking in unprocessed? sounds like formulas 
   might need to record the inputs in a map before evaluating each generation, whch would 
   happen way at the top level.  Then that map could be checked in unprocessed, but that 
   unsettles me. Hmmm...
   

3. [ ] don't run more tests than need be if there is alreadya failure in this formula's batch.

4. [ ] consider implementing with @marick's metaconstant syntax

5. [ ] shrink function overrides
 a. [ ] no shrink
  
6. [ ] if we do metaconstant style, implement generator overriding


OTHER:

* make number of generated facts per formula dynamically bindable
   - add-watch that throws an eception of this value is set < 1
   
* possibly don't use fact macro inside of formula, but instead do something like tabular

* figure out how to make syntax validation errors show something more sensible than the 
  error message you'd see for a problem with a fact validation problem