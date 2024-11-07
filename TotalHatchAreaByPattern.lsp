(defun c:TotalHatchAreaByPattern (/ p1 p2 hatches hatchObj hatchArea patternName areaList invalidHatches result)
  (setq areaList (list))
  (setq invalidHatches (list))
  (princ "\nSelect hatches to calculate total area by pattern: ")
  (setq p1 (getpoint "\nSpecify first corner: "))
  (setq p2 (getcorner p1 "\nSpecify opposite corner: "))
  (setq hatches (ssget "C" p1 p2 '((0 . "HATCH"))))
  (if hatches
    (progn
      (foreach hatch (vl-remove-if 'listp (mapcar 'cadr (ssnamex hatches)))
        (setq hatchObj (vlax-ename->vla-object hatch))
        (setq hatchArea (vl-catch-all-apply 'vla-get-area (list hatchObj)))
        (setq patternName (vl-catch-all-apply 'vla-get-PatternName (list hatchObj)))
        (if (and (not (vl-catch-all-error-p hatchArea)) (not (vl-catch-all-error-p patternName)))
          (progn
            (setq existingEntry (assoc patternName areaList))
            (if existingEntry
              (setq areaList (subst (cons patternName (+ (cdr existingEntry) hatchArea)) existingEntry areaList))
              (setq areaList (cons (cons patternName hatchArea) areaList))
            )
          )
          (setq invalidHatches (cons (vla-get-Handle hatchObj) invalidHatches))
        )
      )
      (setq result "\nTotal hatch area by pattern:\n")
      (foreach entry areaList
        (setq result (strcat result "Pattern: " (car entry) ", Area: " (rtos (cdr entry) 2 2) " sq units\n"))
      )
      (if invalidHatches
        (progn
          (setq result (strcat result "\nHatches that could not be processed (handles):\n"))
          (foreach handle invalidHatches
            (setq result (strcat result handle "\n"))
          )
        )
      )
      (textpage)
      (textscr)
      (princ result)
    )
    (princ "\nNo hatches selected.\n")
  )
  (princ)
)

(princ "\nType 'TotalHatchAreaByPattern' to run the command.\n")
