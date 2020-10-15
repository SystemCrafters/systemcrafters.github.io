;;; publish.el --- Build systemcrafters.cc

;; Copyright (C) 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;; Copyright (C) 2020 David Wilson <david@daviwil.com>

;; Author: David Wilson <david@daviwil.com>
;; Maintainer: David Wilson <david@daviwil.com>
;; URL: https://github.com/SystemCrafters/systemcrafters.github.io
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: hypermedia, blog, feed, rss

;; This file is not part of GNU Emacs.

;; This file is loosely based on Pierre Neidhardt's publish.el, here's his
;; authorship details:

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://gitlab.com/Ambrevar/ambrevar.gitlab.io

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Docs License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Docs License for more details.
;;
;; You should have received a copy of the GNU General Docs License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Usage:
;; emacs --batch -l publish.el --funcall sc/publish

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(use-package esxml
  :ensure t)

(use-package ox-slimhtml
  :ensure t)

(use-package webfeeder
  :ensure t)

(require 'ox-publish)

(setq sc/site-title   "System Crafters")
(setq sc/site-tagline "")

(setq org-publish-use-timestamps-flag t
      org-publish-timestamp-directory "./.org-cache/"
      org-export-with-section-numbers nil
      org-export-with-smart-quotes t
      org-export-with-tags 'not-in-toc
      org-export-with-toc nil)

;; We're using Git, we don't need no steenking backups
(setq make-backup-files nil)

;; Define custom link formats for export
;; NOTE: This pattern is deprecated, I might need to change it later
(org-add-link-type
 "youtube"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format
            "<div class=\"embed-responsive embed-responsive-16by9\">
               <iframe class=\"embed-responsive-item\" src=\"//www.youtube.com/embed/%s?rel=0\" allowfullscreen></iframe>
             </div>"
            path)))))

(defun sc/site-header (info)
  (let* ((file (plist-get info :output-file)))
    (concat
     (sxml-to-xml
      `(div (div (@ (class "blog-header"))
                 (div (@ (class "container"))
                      (div (@ (class "row align-items-center justify-content-between"))
                           (div (@ (class "col-sm-12 col-md-8"))
                                (div (@ (class "blog-title"))
                                     ,sc/site-title))
                           (div (@ (class "col-sm col-md"))
                                (div (@ (class "blog-description text-sm-left text-md-right text-lg-right text-xl-right"))
                                     ,sc/site-tagline)))))

            (div (@ (class "blog-masthead"))
                 (div (@ (class "container"))
                      (div (@ (class "row align-items-center justify-content-between"))
                           (div (@ (class "col-sm-8 col-md-8"))
                                (nav (@ (class "nav"))
                                     (a (@ (class "nav-link") (href "/")) "Home")
                                     (a (@ (class "nav-link") (href "/articles")) "Articles")))
                           (div (@ (class "col-sm-4 col-md-4 nav-icons"))
                                (div (@ (class "text-sm-left text-md-right text-lg-right text-xl-right"))
                                     (a (@ (class "nav-link nav-icon")
                                           (href "https://github.com/daviwil")
                                           (target "_blank"))
                                        (i (@ (class "fa fa-github fa-lg")) ""))
                                     (a (@ (class "nav-link nav-icon")
                                           (href "https://twitter.com/SystemCrafters")
                                           (target "_blank"))
                                        (i (@ (class "fa fa-twitter fa-lg")) ""))
                                     (a (@ (class "nav-link nav-icon")
                                           (href "https://youtube.com/c/SystemCrafters")
                                           (target "_blank"))
                                        (i (@ (class "fa fa-youtube fa-lg")) ""))
                                     (a (@ (class "nav-link nav-icon")
                                           (href "https://systemcrafters.cc/rss.xml")
                                           (target "_blank"))
                                        (i (@ (class "fa fa-rss fa-lg")) ""))))))))))))

(defun sc/site-footer (info)
  (concat
   ;; "</div></div>"
   (sxml-to-xml
    `(footer (@ (class "blog-footer"))
      (div (@ (class "container"))
           (div (@ (class "row"))
                (div (@ (class "col-sm-12 col-md-8"))
                     "<p xmlns:dct=\"http://purl.org/dc/terms/\" xmlns:cc=\"http://creativecommons.org/ns#\" class=\"license-text\"><a rel=\"cc:attributionURL\" href=\"https://systemcrafters.cc\"><span rel=\"dct:title\">System Crafters</span></a> by <a rel=\"cc:attributionURL\" href=\"https://daviwil.com\"><span rel=\"cc:attributionName\">David Wilson</span></a> is licensed under <a href=\"https://creativecommons.org/licenses/by-sa/4.0\">CC BY-SA 4.0</a></p>")
                (div (@ (class "col-sm col-md text-sm-left text-md-right text-lg-right text-xl-right"))
                     (p "Made with " ,(plist-get info :creator)))))))
   ;;<br /><img style=\"height:22px!important;margin-left: 3px;vertical-align:text-bottom;\" src=\"https://search.creativecommons.org/static/img/cc_icon.svg\" /><img  style=\"height:22px!important;margin-left: 3px;vertical-align:text-bottom;\" src=\"https://search.creativecommons.org/static/img/cc-by_icon.svg\" /><img  style=\"height:22px!important;margin-left: 3px;vertical-align:text-bottom;\" src=\"https://search.creativecommons.org/static/img/cc-sa_icon.svg\" />
   (sxml-to-xml
     `(script (@ (src "https://code.jquery.com/jquery-3.2.1.slim.min.js")
                 (integrity "sha384-KJ3o2DKtIkvYIK3UENzmM7KCkRr/rE9/Qpg6aAZGJwFDMVNA/GpGFF93hXpG5KkN")
                 (crossorigin "anonymous"))))
   (sxml-to-xml
     `(script (@ (src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.11.0/umd/popper.min.js")
                 (integrity "sha384-b/U6ypiBEHpOf/4+1nzFpr53nxSS+GLCkfwBdFNTxtclqqenISfwAzpKaMNFNmj4")
                 (crossorigin "anonymous"))))
   (sxml-to-xml
     `(script (@ (src "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta/js/bootstrap.min.js")
                 (integrity "sha384-h0AbiXch4ZDo7tp9hKZ4TsHbi047NrKGLO3SEJAg45jXxnGIfYzk4Si90RDIqNm1")
                 (crossorigin "anonymous"))))))

(setq org-html-preamble  #'sc/site-header
      org-html-postamble #'sc/site-footer
      org-html-metadata-timestamp-format "%Y-%m-%d"
      org-html-checkbox-type 'site-html
      org-html-html5-fancy nil
      org-html-htmlize-output-type nil
      org-html-validation-link nil
      org-html-doctype "html5")

(defun get-article-output-path (org-file pub-dir)
  (let ((article-dir (concat pub-dir
                             (file-name-as-directory
                               (file-name-sans-extension
                                 (file-name-nondirectory org-file))))))

    (if (string-match "\\/index.org$" org-file)
        pub-dir
        (progn
          (unless (file-directory-p article-dir)
            (make-directory article-dir t))

          article-dir))))

(defun sc/org-html-template (contents info)
  (concat
   "<!DOCTYPE html>"
   (sxml-to-xml
    `(html (@ (lang "en"))
           (head
            "<!-- " ,(org-export-data (org-export-get-date info "%Y-%m-%d") info) " -->"
            (meta (@ (charset "utf-8")))
            (meta (@ (author "David Wilson")))
            (meta (@ (name "viewport")
                     (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
            "
    <!-- Font Awesome -->
    <link rel=\"stylesheet\" href=\"https://stackpath.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css\"  crossorigin=\"anonymous\">

    <!-- Bootstrap core CSS -->
    <link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta/css/bootstrap.min.css\" integrity=\"sha384-/Y6pD6FV/Vv2HJnA6t+vslU6fwYXjCFtcEpHbNJ0lyAFsXTsjBbfaDjzALeQsN6M\" crossorigin=\"anonymous\">

    <!-- Custom styles for this template -->
    <link href=\"/css/site.css\" rel=\"stylesheet\">

    <link href=\"https://fonts.googleapis.com/css?family=Alegreya|Bitter|Crete+Round|Faustina|Glegoo|Manuale|Roboto+Slab|Source+Serif+Pro\" rel=\"stylesheet\">
    <link href=\"https://fonts.googleapis.com/css?family=recentCabin|PT+Sans|Source+Sans+Pro\" rel=\"stylesheet\">"
             (title ,(concat (org-export-data (plist-get info :title) info) " - System Crafters")))
           (body
             ,(sc/site-header info)
             (div (@ (class "container"))
                  (div (@ (class "row"))
                       (div (@ (class "col-sm-12 blog-main"))
                            (div (@ (class "blog-post"))
                                 (h2 (@ (class "blog-post-title"))
                                     ,(org-export-data (plist-get info :title) info))
                                 (p (@ (class "blog-post-meta"))
                                    ,(org-export-data (org-export-get-date info "%B %e, %Y") info))
                                 ,contents
                                 ,(let ((tags (plist-get info :filetags)))
                                    (when (and tags (> (list-length tags) 0))
                                      `(p (@ (class "blog-post-tags"))
                                          "Tags: "
                                          ,(mapconcat (lambda (tag) tag)
                                                        ;; TODO: We don't have tag pages yet
                                                        ;; (format "<a href=\"/tags/%s/\">%s</a>" tag tag))
                                                      (plist-get info :filetags)
                                                      ", "))))
                                 ,(when (equal "article" (plist-get info :page-type))
                                    "<script src=\"https://utteranc.es/client.js\"
                                              repo=\"SystemCrafters/systemcrafters.github.io\"
                                              issue-term=\"title\"
                                              label=\"comments\"
                                              theme=\"photon-dark\"
                                              crossorigin=\"anonymous\"
                                              async>
                                     </script>")))))

             ,(sc/site-footer info))))))

(org-export-define-derived-backend 'site-html
  'slimhtml
  :translate-alist
  '((template . sc/org-html-template))
  :options-alist
  '((:page-type "PAGE-TYPE" nil nil t)
    (:html-use-infojs nil nil nil)))

(defun org-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML, using the FILENAME as the output directory."
  (let ((article-path (get-article-output-path filename pub-dir)))
    (cl-letf (((symbol-function 'org-export-output-file-name)
               (lambda (extension &optional subtreep pub-dir)
                 (concat article-path "index" extension))))
      (org-publish-org-to 'site-html
                          filename
                          (concat "." (or (plist-get plist :html-extension)
                                          "html"))
                          plist
                          article-path))))

(defun sc/sitemap-entry (entry style project)
  (format "<h4><em>%s</em> - <a href=\"%s\">%s</a></h4>"
          (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
          (concat (file-name-sans-extension entry) "/")
          (org-publish-find-title entry project)))

(defun sc/generate-sitemap (title list)
  (concat
    "#+TITLE: " title "\n\n"
    "#+BEGIN_EXPORT html\n"
    (mapconcat (lambda (item)
                 (car item))
               (cdr list)
               "\n")
    "\n#+END_EXPORT\n"))

(defun sc/make-site-section (title path-name &optional sitemap?)
  (list (concat "systemcrafters.cc:" path-name)
        :recursive t
        :base-directory (concat "./" path-name)
        :publishing-function '(org-html-publish-to-html)
        :publishing-directory (concat "./docs/" path-name)
        :auto-sitemap sitemap?
        :sitemap-title title
        :sitemap-filename "index.org"
        :sitemap-style 'list
        :sitemap-format-entry #'sc/sitemap-entry
        :sitemap-function #'sc/generate-sitemap
        :sitemap-sort-files 'anti-chronologically))

(setq org-publish-project-alist
      (list
       (sc/make-site-section "Articles" "articles" t)
       (list "systemcrafters.cc:main"
             :recursive t
             :base-directory "./"
             :publishing-function '(org-html-publish-to-html)
             :publishing-directory "./docs/"
             :with-title nil)
       (list "systemcrafters.cc:static"
             :base-directory "./articles"
             :exclude "\\(\\.org\\|\\.md\\)"
             :base-extension 'any
             :publishing-directory "./docs/articles/"
             :publishing-function 'org-publish-attachment
             :recursive t)
       (list "site" :components '("systemcrafters.cc:main"
                                  "systemcrafters.cc:articles"))))

(defun sc/publish ()
  (interactive)
  (org-publish-all t)
  (setq webfeeder-default-author "David Wilson <david@systemcrafters.cc>")
  (pp
           (mapcar (lambda (f) (replace-regexp-in-string ".*/docs/articles/" "" f))
                   (directory-files-recursively "docs/articles" "index.html")))
  (webfeeder-build
   "rss.xml"
   "./docs/"
   "https://systemcrafters.cc/"
   (delete "articles/index.html"
           (mapcar (lambda (f) (replace-regexp-in-string "docs/" "" f))
                   (directory-files-recursively "docs/articles" "index.html")))
   :builder 'webfeeder-make-rss
   :title "System Crafters"
   :description "The home for computer configuration enthusiasts."
   :max-entries 20))
