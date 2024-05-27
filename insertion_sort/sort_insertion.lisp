(SETQ list1 (list 11 33 23 45 13 25 8 135))
(SETQ list2 (list 83 72 65 54 47 33 29 11))

(SETQ old_list list1)
(SETQ new_list (list (car old_list)))

(DEFVAR old_leng ( - (LENGTH old_list) 2))
(DEFVAR key)
(DEFVAR atm)
(DEFVAR insertion_point)
(DEFVAR i_flag T)

(DEFUN i_sort (old_list)

)
(FORMAT t "old list is ~D ~%" old_list)
(FORMAT t "initial new_list is ~D ~%" new_list)

(LOOP for i from 0 to old_leng DO

    (SETF key (NTH (+ i 1) old_list))
    (FORMAT t "~D th key is ~D ~%" i key)
    (SETF i_flag T)

    (LOOP for j from 0 to ( - (LENGTH new_list) 1) DO

        (SETF atm (NTH j new_list))
        (FORMAT t "~D th atom is ~D ~%" j atm)

        (COND

            ((AND (EQ j 0) (< key atm))
                (FORMAT t "first atom ~D is bigger than key ~D ~%" atm key)
                (COND
                    ((EQ i_flag T)
                        (SETQ new_list (APPEND (list key) new_list))
                        (FORMAT t "add key ~D to front of list ~%" key)
                        (SETQ j ( + j 1 ))
                        (SETF i_flag NIL)
                    )
                )
            )

            ((< key atm)
                (FORMAT t "~D th atom ~D is bigger than key ~D ~%" j atm key)
                (COND
                    ((EQ i_flag T)
                        
                        (SETQ head (REVERSE (MEMBER (NTH ( - j 1 )  new_list) (REVERSE new_list))))
                        (SETQ tail (MEMBER (NTH j new_list) new_list))
                        (SETQ new_list (APPEND head (list key) tail))

                        (FORMAT t "key ~D is placed in index ~D ~%" key j)
                        
                        (SETQ j ( + j 1 ))                        
                        (SETF i_flag NIL)
                    )
                )

            )
        )
    )
    (IF (EQ i_flag T)
        (PROGN
            (SETQ new_list (APPEND new_list (list key)))
            (FORMAT t "add key ~D to back of list ~%" key)
        )
    )
    (FORMAT t "appended_list of ~D th cycle is ~D ~%~%" i new_list)
)
