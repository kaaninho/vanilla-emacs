(defvar my/rag-chunk-size 1000
  "Maximale Zeichen pro Chunk.")

(defvar my/rag-db-file "~/.ellama_rag_db.json"
  "Lokale JSON-Datei für die RAG-Embeddings.")

(defun my/rag-chunk-file (file)
  "Lese FILE und splitte in Chunks."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((content (buffer-string))
          (chunks '()))
      (while (> (length content) 0)
        (let ((chunk (substring content 0 (min my/rag-chunk-size (length content)))))
          (push chunk chunks)
          (setq content (substring content (length chunk)))))
      (nreverse chunks))))

(defun my/rag-embed-chunk (chunk)
  "Erzeuge Embedding für CHUNK via Ollama / llama3:latest."
  (with-temp-buffer
    (insert chunk)
    (call-process-region (point-min) (point-max)
                         "ollama" nil t nil "embed" "llama3:latest")
    (json-parse-string (buffer-string))))

(defun my/rag-index-folder (folder)
  "Indexiere alle Markdown-Dateien in FOLDER und speichere Embeddings lokal."
  (interactive "DOrdner: ")
  (let ((db '()))
    (dolist (file (directory-files folder t "\\.md$"))
      (dolist (chunk (my/rag-chunk-file file))
        (let ((embedding (my/rag-embed-chunk chunk)))
          (push (list :file file :chunk chunk :embedding embedding) db))))
    ;; Speicher in JSON
    (with-temp-file my/rag-db-file
      (insert (json-encode db)))
    (message "RAG-Datenbank erstellt: %s" my/rag-db-file)))

(defun my/rag-cosine-similarity (vec1 vec2)
  "Berechne Cosinus-Ähnlichkeit zwischen zwei Vektoren."
  (let ((dot 0.0) (norm1 0.0) (norm2 0.0))
    (dotimes (i (length vec1))
      (setq dot (+ dot (* (elt vec1 i) (elt vec2 i))))
      (setq norm1 (+ norm1 (* (elt vec1 i) (elt vec1 i))))
      (setq norm2 (+ norm2 (* (elt vec2 i) (elt vec2 i)))))
    (/ dot (sqrt (* norm1 norm2)))))

(defun my/rag-query (question &optional topk)
  "Frage Ellama mit RAG. TOPK relevanteste Chunks werden genutzt."
  (interactive "sFrage: ")
  (let* ((topk (or topk 5))
         (db (json-parse-file my/rag-db-file :object-type 'alist))
         (q-embedding (my/rag-embed-chunk question))
         ;; Berechne Similarities
         (scored (mapcar (lambda (entry)
                           (cons entry (my/rag-cosine-similarity
                                        (alist-get 'embedding q-embedding)
                                        (alist-get 'embedding entry))))
                         db))
         ;; Sortiere absteigend
         (sorted (seq-take (sort scored (lambda (a b) (> (cdr a) (cdr b)))) topk))
         (context (mapconcat (lambda (x) (alist-get 'chunk (car x))) sorted "\n---\n"))
         (prompt (concat "Kontext:\n" context "\n\nFrage: " question)))
    ;; Sende an Ellama-Chat
    (ellama-chat prompt)))


(provide 'rag)
