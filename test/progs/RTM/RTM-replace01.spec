// Inner variables
Q = 'nil
Q1 = 'nil
S1 = 'nil
S2 = 'nil
Q2 = 'nil
RulesRev = 'nil
Rules = 'nil
Rule = 'nil

// Describe the TM
// Replace 0 by 1 in string 0*
// Example: _000 --> _111
Start = '1
End = '6
pc_max = '7
Transitions = 
 '((1 . (BLANK . (BLANK . 2))) . 
  ((2 . (SLASH . (RIGHT . 3))) . 
  ((3 . (0 .     (1     . 2))) . 
  ((3 . (BLANK . (BLANK . 4))) . 
  ((4 . (SLASH . (LEFT  . 5))) . 
  ((5 . (1 .     (1     . 4))) . 
  ((5 . (BLANK . (BLANK . 6))) . 
   nil))))))))

// Tape for full specialization
//S = 'nil
//S_left = 'nil
//S_right = '(0 . (0 . (0 . nil)))
