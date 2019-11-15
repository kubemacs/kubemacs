- [Description](#sec-1)
  - [Features:](#sec-1-1)
- [Install](#sec-2)
- [Combined Concepts](#sec-3)
  - [[The Org Manual: Capture](https://orgmode.org/manual/Capture.html#Capture)](#sec-3-1)
  - [[org-protocol.el – Intercept calls from emacsclient to trigger custom actions](https://orgmode.org/worg/org-contrib/org-protocol.html)](#sec-3-2)
  - [[alphapapa/org-protocol-capture-html: Capture HTML from the browser selection into Emacs as org-mode content](https://github.com/alphapapa/org-protocol-capture-html)](#sec-3-3)
- [Configuration](#sec-4)
  - [Getting your browser to call emacsclient for `org-protocol://` links](#sec-4-1)
    - [[Linux setup (Gnome)](https://orgmode.org/worg/org-contrib/org-protocol.html#org4166fc4)](#sec-4-1-1)
    - [xdg protocol handler](#sec-4-1-2)
  - [[The Org Manual: Capture templates](https://orgmode.org/manual/Capture-templates.html)](#sec-4-2)
- [Mapping Bookmarklets to Capture Templates](#sec-5)
  - [org-capture-html (oc-H)](#sec-5-1)
    - [bookmarklet](#sec-5-1-1)
    - [capture template](#sec-5-1-2)
  - [org-capture-paragraph (oc-p)](#sec-5-2)
  - [org-capture-link (oc-L)](#sec-5-3)
- [A bookmarklet to save the current title/link](#sec-6)
- [Linux setup (Gnome)](#sec-7)


# Description<a id="sec-1"></a>

This layer adds support for capturing urls, page titles, and selected content from your web browser via bookmarklets that call emacsclient with a special `org-protocol://` uri.

## Features:<a id="sec-1-1"></a>

-   Capture Current Page (Link + Page Title)
-   Capture Selected Text
-   Capture Html (as org via pandoc)

# Install<a id="sec-2"></a>

To use this configuration layer, add it to your `~/.spacemacs`. You will need to add `org-capture` to the existing `dotspacemacs-configuration-layers` list in this file.

# Combined Concepts<a id="sec-3"></a>

## [The Org Manual: Capture](https://orgmode.org/manual/Capture.html#Capture)<a id="sec-3-1"></a>

This is usually just used within emacs.

Capture lets you quickly store notes with little interruption of your work flow. Org's method for capturing new items is heavily inspired by John Wiegley's excellent Remember package.

## [org-protocol.el – Intercept calls from emacsclient to trigger custom actions](https://orgmode.org/worg/org-contrib/org-protocol.html)<a id="sec-3-2"></a>

This is what connects org-mode to the “outside world” using a MIME protocol handler.

org-protocol intercepts calls from emacsclient to trigger custom actions without external dependencies. Only one protocol has to be configured with your external applications or the operating system, to trigger an arbitrary number of custom actions. Just register your custom sub-protocol and handler with the variable \`org-protocol-protocol-alist'.

## [alphapapa/org-protocol-capture-html: Capture HTML from the browser selection into Emacs as org-mode content](https://github.com/alphapapa/org-protocol-capture-html)<a id="sec-3-3"></a>

org-protocol is awesome, but browsers do a pretty poor job of turning a page's HTML content into plain-text. However, Pandoc supports converting *from* HTML *to* org-mode, so we can use it to turn HTML into Org-mode content! It can even turn HTML tables into Org tables!

# Configuration<a id="sec-4"></a>

## Getting your browser to call emacsclient for `org-protocol://` links<a id="sec-4-1"></a>

### [Linux setup (Gnome)](https://orgmode.org/worg/org-contrib/org-protocol.html#org4166fc4)<a id="sec-4-1-1"></a>

```shell
gconftool-2 -s /desktop/gnome/url-handlers/org-protocol/command '/usr/local/bin/emacsclient %s' --type String
gconftool-2 -s /desktop/gnome/url-handlers/org-protocol/enabled --type Boolean true
```

### xdg protocol handler<a id="sec-4-1-2"></a>

Save the following to `$HOME/.local/share/applications/org-protocol.desktop`

```shell
[Desktop Entry]
Name=org-protocol
Exec=emacsclient %u
Type=Application
Terminal=false
Categories=System;
MimeType=x-scheme-handler/org-protocol;
```

Then run the following:

```shell
# On KDE:
kbuildsycoca4
# On GNOME:
update-desktop-database ~/.local/share/applications/
```

## [The Org Manual: Capture templates](https://orgmode.org/manual/Capture-templates.html)<a id="sec-4-2"></a>

You can use templates for different types of capture items, and for different target locations. The ones you want to use with org-protocol require

```emacs-lisp

(setq org-capture-templates
      '(
        ("t" "TODO"
         entry (file+headline "~/org/TODO.org" "Inbox")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"
         )
        ))
```

# Mapping Bookmarklets to Capture Templates<a id="sec-5"></a>

## org-capture-html (oc-H)<a id="sec-5-1"></a>

### bookmarklet<a id="sec-5-1-1"></a>

This requires pandoc installed to convert the html into org-mode.

```javascript
javascript:location.href = 'org-protocol://capture-html?template=w&url=' + encodeURIComponent(location.href) + '&title=' + encodeURIComponent(document.title || "[untitled page]") + '&body=' + encodeURIComponent(function () {var html = ""; if (typeof document.getSelection != "undefined") {var sel = document.getSelection(); if (sel.rangeCount) {var container = document.createElement("div"); for (var i = 0, len = sel.rangeCount; i < len; ++i) {container.appendChild(sel.getRangeAt(i).cloneContents());} html = container.innerHTML;}} else if (typeof document.selection != "undefined") {if (document.selection.type == "Text") {html = document.selection.createRange().htmlText;}} var relToAbs = function (href) {var a = document.createElement("a"); a.href = href; var abs = a.protocol + "//" + a.host + a.pathname + a.search + a.hash; a.remove(); return abs;}; var elementTypes = [['a', 'href'], ['img', 'src']]; var div = document.createElement('div'); div.innerHTML = html; elementTypes.map(function(elementType) {var elements = div.getElementsByTagName(elementType[0]); for (var i = 0; i < elements.length; i++) {elements[i].setAttribute(elementType[1], relToAbs(elements[i].getAttribute(elementType[1])));}}); return div.innerHTML;}());
```

### capture template<a id="sec-5-1-2"></a>

```emacs-lisp
(add-to-list 'org-capture-templates
             ("w" "Default template" entry
              (file+headline "~/org/capture.org" "Notes")
              "* %^{Title}

  Source: %u, %c

  %i" :empty-lines 1)
)
```

## org-capture-paragraph (oc-p)<a id="sec-5-2"></a>

```javascript
javascript:location.href='org-protocol://capture?template=p&url='+ encodeURIComponent(location.href)+'&title='+ encodeURIComponent(document.title)+'&body='+encodeURIComponent(window.getSelection())
```

```emacs-lisp
(add-to-list 'org-capture-templates
             ("p" "Paragraph" entry
              (file+headline "~/ii/org/agenda/notes.org" "Inbox")
              "* [[%:link][%:description]] %^G
Time: %u
 #+BEGIN_QUOTE
%:initial
#+END_QUOTE


%?")
             )
```

## org-capture-link (oc-L)<a id="sec-5-3"></a>

```javascript
javascript:location.href='org-protocol://capture?template=L&url='+ encodeURIComponent(location.href)+'&title='+ encodeURIComponent(document.title)+'?body='+encodeURIComponent(window.getSelection())
```

```emacs-lisp
(add-to-list 'org-capture-templates
             ("L" "Capture a Link" entry
              (file+headline "~/ii/org/agenda/notes.org" "Inbox")
              "* %? [[%:link][%:description]]
Captured On: %U")
             ;; javascript:location.href='org-protocol://store-link?url='+ encodeURIComponent(location.href)+'&title='+ encodeURIComponent(document.title);
             )
)
```

# A bookmarklet to save the current title/link<a id="sec-6"></a>

Similar to using `aol` to call **org-store-link** and later retrieval with `,il` **org-insert-link**

```javascript
javascript:location.href='org-protocol://capture?template=p&url='+ encodeURIComponent(location.href)+'&title='+ encodeURIComponent(document.title)+'?body='+encodeURIComponent(window.getSelection())
```

# Linux setup (Gnome)<a id="sec-7"></a>
