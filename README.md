# Historian.el

Historian.el stores the results of completing-read and similar
functions persistently. This provides a way to give completion
candidates that are more frequently or more recently used a better
position in the candidates list.

# Usage

Depending on how you've configured `use-package`, setup could be as simple as:

```emacs
(use-package historian)
(use-package ivy-historian)

(use-package ivy
  :init
  (ivy-mode +1)
  (historian-mode +1)

  :config
  (ivy-historian-mode +1))
```

# How do I know it's working?

You can try the following

```emacs
(setq ivy-historian-recent-boost most-positive-fixnum)
```

Then recent candidates should unconditionally appear first, if `ivy-historian` is working. 
