(defun midje-random-praise ()
  (concat (nth (mod (random t) (length midje-praise-quotes))
	       midje-praise-quotes )
	  "\n"))

(defvar midje-praise-quotes 
'(
  "When someone asks you if you're a god, you say 'YES'! -- Zeddemore" 
  "The work itself praises the master. -- CPE Bach"
  "Happiness comes when you believe that you have done something truly meaningful. -- Yan"
  "I do believe in praising that which deserves to be praised. -- Dean Smith"
  "Out of clutter find simplicity; from discord find harmony; in the middle of difficulty lies opportunity. -- Einstein"
  "Satisfaction lies in the effort, not in the attainment, full effort is full victory. -- Gandhi"
  "One of the symptoms of an approaching nervous breakdown is the belief that one's work is terribly important. -- Russell"
  "The sum of wisdom is that time is never lost that is devoted to work. -- Emerson"
  "Without work, all life goes rotten. -- Camus"
  "Work joyfully and peacefully, knowing that right thoughts and right efforts will inevitably bring about right results. -- Allen"
  "Work is either fun or drudgery. It depends on your attitude. I like fun. -- Barrett"
  "There's a certain satisfaction in a little bit of pain. -- Madonna"
  "One small test for a codebase, one giant leap for quality kind! -- @zspencer"
  "Hey! You're green! Refactor then we get to go red again! -- @zspencer"
  "Woohoo! -- @zspencer"
  "Ding! You Have Gained Experience -- @zspencer"
  "A journey of a thousand miles begins with a single step. -- @alanmstokes"
  "If this isn't nice, I don't know what is. -- Vonnegut"
  "Simplicity, carried to the extreme, becomes elegance. -- Jon Franklin"
  "This is the future you were hoping for. -- @Vaguery"
  "O frabjous day! Callooh! Callay! -- Lewis Carroll"
  "Whoever wants to reach a distant goal must take small steps. -- fortune cookie"
  "The next function taunts you still. Will you rise to the challenge? -- anonymous"
  "Any intelligent fool can make things bigger, more complex, and more violent. It takes a touch of genius -- and a lot of courage -- to move in the opposite direction. -- Schumacher"
  "Excellence is not an act but a habit. -- Aristotle"
  "Code you'd be proud to give your mom to show off on the fridge. -- Mike Cohn"
  "A clean boundary between useful abstractions and the grubby code that touches the real world is always a good thing. -- Ron Jeffries"
  "Of course the universe *is* out to get us, but it's not going to do it by passing a null to one of our methods. -- Ron Jeffries"
  "Good code is its own best documentation. -- Steve McConnell"
  "First they ignored you, then they laughed at you, then they fought you, now you've won. -- Not quite Gandhi"
))

(provide 'midje-mode-praise)
