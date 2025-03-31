(defun llegir-linia ()
    "Llegeix una línia de text per consola, la imprimeix i l'escriu en un fitxer."
    (let ((entrada ""))
        (loop
            (setf entrada (read-line))
            (unless (string= entrada "")
                (return entrada)))
        (format t "Has escrit: ~A~%" entrada)
        (let ((stream (open "clasificacion.txt" :direction :output :if-exists :append :if-does-not-exist :create)))
            (when stream
                (unwind-protect
                    (format stream "~A~%" entrada)
                    (close stream))))
        entrada))

(defun iniciar-llegir-linia ()
    "Funció principal per llegir linies de manera interactiva."
    (loop
        (format t "Escriu una linia (o escriu 'sortir' per acabar):~%")
        (let ((entrada (llegir-linia)))
            (when (string= entrada "sortir")
                (return)))))

(iniciar-llegir-linia)