(SETQ list1 (list 11 33 23 45 13 25 8 135))
(SETQ list2 (list 83 72 65 54 47 33 29 11))
(SETQ list3 (list 800 722 615 54 47 33 29 11 4 321 44 47))

; 리스트를 전달 받아 실행되는 삽입 정렬 함수 선언
(DEFUN i_sort (old_list)
    ; 함수 호출 구분을 위한 줄 긋기
    (LOOP for k from 0 to 80 DO
        (FORMAT t "*")
    )
    (FORMAT t "~%~%")

    ; 전달 받은 리스트의 첫 원소를 새로운 리스트에 추가.
    (SETQ new_list (list (car old_list)))

    (DEFPARAMETER old_leng ( - (LENGTH old_list) 2))
    ; 리스트의 중간에 key가 삽입 되었는지 검사하는 flag
    (DEFVAR ni_flag T)

    (FORMAT t "passed list is ~D ~%~%~%" old_list)
    (FORMAT t "initial new_list is ~D ~%~%~%" new_list)

    ; 전달 받은 리스트의 크기의 -1 번까지 반복 (8개의 원소가 있다면 7번 반복)
    (LOOP for i from 0 to old_leng DO

        ; 전달 받은 리스트의 2번째 부터 마지막 atom 까지 key에 저장
        (SETF key (NTH (+ i 1) old_list))
        (FORMAT t "~D th key is ~D ~%" (+ i 1) key)
        ; 새로운 key를 가져올 때마다 플래그 초기화
        (SETF ni_flag T)

        ; 예전 리스트에서 가져온 key가 하나씩 추가 될 새로운 리스트의 크기 만큼 반복. (2개의 원소를 가지면 2번 반복)
        ; 이 리스트는 정렬되어 있음을 가정함. 첫 원소만 있을 경우 정렬되어 있는 상태이고 
        ; 이 리스트에 새로운 key를 삽입할 때 순서가 맞춰서 삽입할 것이기 때문. 
        (LOOP for j from 0 to ( - (LENGTH new_list) 1) DO

            (SETF atm (NTH j new_list))
            (FORMAT t "~D th atom is ~D ~%" (+ j 1) atm)

            (COND

                ; key 가 정렬된 리스트의 첫번째 원소보다 작을 경우
                ((AND (EQ j 0) (< key atm))

                    (COND

                        ((EQ ni_flag T)

                            (FORMAT t "first atom ~D is bigger than key ~D ~%" atm key)
                            
                            ; 맨 앞에 key 추가
                            (SETQ new_list (APPEND (list key) new_list))
                            (FORMAT t "add key ~D to front of list ~%" key)

                            ; 리스트에 key를 삽입했음을 표기하고 안쪽 loop 문 탈출
                            (SETF ni_flag NIL)
                            (return)
                        )
                    )
                )

                ; key가 정렬된 리스트의 나머지 원소들 중 하나보다 작을 경우
                ((< key atm)

                    (COND

                        ((EQ ni_flag T)   

                            (FORMAT t "~D th atom ~D is bigger than key ~D ~%" (+ j 1) atm key)

                            (SETQ head (REVERSE (MEMBER (NTH ( - j 1 )  new_list) (REVERSE new_list))))
                            (SETQ tail (MEMBER (NTH j new_list) new_list))
                            (SETQ new_list (APPEND head (list key) tail))
                            (FORMAT t "key ~D is placed in index ~D ~%" key j)
                            ; 리스트에 key를 삽입했음을 표기하고 안쪽 loop 문 탈출
                            
                            (SETF ni_flag NIL)
                            (return)
                        )
                    )

                )
            )
        )
        (IF (EQ ni_flag T)
            (PROGN

                (SETQ new_list (APPEND new_list (list key)))
                (FORMAT t "add key ~D to back of list ~%" key)

            )
        )
        (FORMAT t "appended_list of ~D th cycle is ~D ~%~%" (+ i 1) new_list)
    )

    (FORMAT t "old list is ~D ~%" old_list)
    (FORMAT t "result is ~D ~%~%" new_list)
    new_list
)

(i_sort list1)
(i_sort list2)
(i_sort list3)
