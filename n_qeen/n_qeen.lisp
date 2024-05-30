; 체스판의 크기 설정.
(defparameter n 4)
; 체스판 배열 생성. 모든 원소를 0으로 초기화.
(defparameter board (make-array (list n n) :initial-element 0))


; 체스판 출력 함수.
(DEFUN PRINT_BOARD (BD)

    (FORMAT t " ~% ***** print board ***** ~%")

    (LOOP for i from 0 to 3 DO
        (LOOP for j from 0 to 3 DO
            ; 배열의 원소가 1 이면
            (IF (EQ (AREF BD i j) 1)
                ; "Q" 출력
                (FORMAT t " ~a " "Q")
                ; 아니면 "." 출력
                (FORMAT t " ~a " ".")     
            )
        )
        (FORMAT t "~%~%")
    )

    (FORMAT t " *********************** ~%")

)

; 체스판의 전치행렬 출력하는 함수.
(DEFUN PRINT_TRBOARD (BD)

    (FORMAT t " ~% **** print transposed *** ~%")

    (LOOP for i from 0 to 3 DO
        (LOOP for j from 0 to 3 DO
            ; 배열의 원소가 1 이면
            (IF (EQ (AREF BD j i) 1)
                ; "Q" 출력
                (FORMAT t " ~a " "Q")
                ; 아니면 "." 출력
                (FORMAT t " ~a " ".")     
            )
        )
        (FORMAT t "~%~%")
    )

    (FORMAT t " *********************** ~%")

)

; 해당 위치의 안전성을 체크하는 함수.
(DEFUN isSafe ( row col )

    ; 디버깅을 위한 출력문
    (FORMAT t "is safe ~a ~a is called ~%" row col)

    ; 이전 행의 같은 열에 퀸이 이미 놓였는지 검사
    (LOOP for i from 0 to row DO
        (IF (EQ (AREF board i col) 1)
            (return-from isSafe NIL)
        )
    )

    ; 지금 위치로 부터 왼쪽 위에 퀸이 이미 놓였는지 검사. 
    ; 현재 row 와 col 에 동시에 -1, -2... 한 위치를 검사 함. 첫번째 행 또는 열에 도달할 때까지.
    (LOOP for i from 1 to n DO
        (IF (AND (>= ( - row i ) 0) (>= ( - col i ) 0))
            (PROGN
                (IF (EQ (AREF board ( - row i ) ( - col i )) 1)
                    (return-from isSafe NIL)
                )
            )
        )
    )

    ; 지금 위치로 부터 오른쪽 위에 퀸이 이미 놓였는지 검사
    ; 현재 row 에서 -1, -2.. 하여 첫번째 행 까지만 검사, 
    ; 현재 col 에서 마지막 열까지만 1, 2 씩 증가시키면서 검사
    (LOOP for i from 1 to n DO
        (IF (AND (>= ( - row i ) 0) (< ( + col i ) n))
            (PROGN
                (IF (EQ (AREF board ( - row i ) ( + col i )) 1)
                    (return-from isSafe NIL)
                )
            )
        )
    )

    ; 모든 위협 검사에서 걸리지 않은 경우 해당 위치가 안전함을 알림.
    (return-from isSafe t)
    
)

(DEFUN N_QUEEN ( row )

    ; 디버깅을 위한 출력문
    (FORMAT t "NQ ~a is called ~%" row)
    (PRINT_BOARD board)

    ; N_QUEEN을 호출한 행이 체스판의 크기를 넘어나는 경우,
    ; 이전 행에 모두 Queen을 배치했다고 생각할 수 있음 = 해를 구했다.
    (IF ( >= row 4 )

        ; IF 의 then 부분
        (PROGN
            ; 디버깅을 위한 출력문
            (PRINT row)
            (PRINT_BOARD board)
            ; 해를 구했으면 true를 리턴한다.
            (return-from N_QUEEN t)
        )

        ; IF 의 else 부분. 아직 해를 구하지 못한 경우 실행.
        (PROGN

            ; 지금 row의 모든 col에 대해서 검사함.
            (LOOP for col from 0 to ( - n 1 ) DO
                ; 현재의 row X col 의 위치가 안전한지 확인.
                (IF (isSafe row col)
                    ; 안전한 경우
                    (PROGN

                        ; 해당 위치에 1을 삽입 (Queen을 둔것으로 간주함. 연산의 편이성을 위해 정수로 처리.)
                        (SETF (AREF board row col) 1)
                        ; 디버그를 위한 출력문
                        (FORMAT t "Queen is placed ~%")

                        ; 다음 행 검사 시작을 위한 함수 호출
                        ; 다음 행이 true를 리턴하면 (모든 row에 Queen을 모두 배치한 상황)
                        (IF (N_QUEEN ( + row 1 ))
                            ; true를 리턴( 다음 행 함수들의 리턴값을 처음 호출까지 전달함. )
                            (return-from N_QUEEN t)
                        )

                        ; 다음 행을 검사했는데 NIL 이 return 된 경우 - 해를 구하지 못했음 or 이 열은 안전하지 않음.
                        ; 이 위치에 Queen을 두면, 다음 행에 Queen을 둘 수 있는 위치가 없다는 이야기. 이 위치를 0으로 되돌린다.
                        (SETF (AREF board row col) 0)

                        ; 디버그를 위한 출력문. back track이 정상적으로 수행중인지 확인한다.
                        (FORMAT t "backtrack~%")
                        (PRINT_BOARD board)
                    )
                )
            )
            ; 위의 LOOP 를 탈출했다는 것은 지금 이전 row에 배치 된 Queen들을 바탕으로
            ; 지금 row의 모든 col과 그 위치에서 파생될 수 있는 모든 경우를 체크했다는 것.
            ; 즉, 이전 상황을 고려했을 때, 지금 row에는 Queen을 둘 수 없는 상황인 것 이므로 NIL을 리턴.
            (return-from N_QUEEN NIL)
        )
    )
)

; 해를 찾는데 성공했으면
(IF (N_QUEEN 0)
    ; 해와 그 전치를 출력 ( 체스판의 특정 상황은 그 전치에서 또한 성립하는 것이 당연하므로 )
    (PROGN
        (PRINT_BOARD board)
        (PRINT_TRBOARD board)
    )
    ; 실패했으면 해가 없음을 알림.
    (FORMAT t "there is no solution")
)
