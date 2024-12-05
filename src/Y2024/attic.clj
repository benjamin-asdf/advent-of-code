











(loop
    [grid (->grid input)
     orig-pos [0 0]
     pos [0 0]
     seen? #{}
     xmas-left (into [] target-word)]

  ;; so basically, I can check all neighbours, if a 'continuation'
  ;; of the XMAS word is found,
  ;;
  ;; if there is, then update the pos to that neighbour,
  ;; - remove from the front of xmas,
  ;; - add that neighbour seen? ? - no, because they can overlap, too
  ;;
  ;; if there is no continuation, then orig-pos did not yield an xmas word,
  ;; we can return to orig-pos, and add orig-pos to seen?
  ;;
  ;; I guess seen? is not needed because we go orig pos.. then through the complete
  ;; grid.










  ;; if the thing at pos is equal to the
  ;; first letter in xmas-left
  ;;    then,
  ;;    if this was the last letter of xmas,
  ;;      count an xmas occurance
  ;;    else, proceed with the next unseed neighbour
  ;;          and remove first one from the xmas-left
  ;;
  ;; else,





    )
