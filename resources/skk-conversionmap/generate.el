;; Generate conversion map from `skk-rule-tree'
;;
;;


(defun walk (f tree)
  ""
  (let ((char (nth 0 tree))
	(prefix (nth 1 tree))
	(nextstate (nth 2 tree))
	(kana (nth 3 tree))
	(child (nth 4 tree)))
    (if (null child)
	;; botton case
	`(,@(apply f (list char prefix nextstate kana)) :children nil)
      `(,@(apply f (list char prefix nextstate kana))
	    :children ,(seq-map `(lambda (c) (walk ,f c)) child)))))

(defun rule-list-to-json-string (tree kana-modification-function)
  (let ((json-object-type 'plist))
    (json-encode
     (list (walk '(lambda (char prefix nextstate kana)
 		    (let ((output (apply kana-modification-function (list kana)))) ; 関数等はすべて使用不可能なので消す
 		      (list prefix :next nextstate :kana output)))
 		 ;; (nth 20 (nth 4 skk-rule-tree)))))))
 		 tree)))))


(progn
  (find-file "default.json")
  (erase-buffer)
  (insert (rule-list-to-json-string
	   skk-rule-tree #'(lambda (kana) (pcase kana
					    ((pred consp) (cdr kana))
					    ((pred stringp) kana)
					    (_ nil)))))
  (json-pretty-print-buffer))
