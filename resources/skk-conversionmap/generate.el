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

(with-current-buffer (get-buffer-create "*json-encoded*")
  (let ((json-object-type 'plist))
    (erase-buffer)
    (insert (json-encode
	     (list (walk '(lambda (char prefix nextstate kana)
 		      (let ((output (pcase kana
 				      (`(,_ . ,o) o)
 				      ((and (pred stringp) o) o)
 				      (_ nil)))) ; 関数等はすべて使用不可能なので消す
 			(list prefix :next nextstate :kana output)))
 		   ;; (nth 20 (nth 4 skk-rule-tree)))))))
 		   skk-rule-tree))))))
