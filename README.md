# Mproj.el

Its a mini project managment tool for easy selection and opening of
projects via minibuffer.

<p align="center">
  <img src="ss.png" alt="Screen Shoot"/>
</p>


## Usage

Give it a list of directories containing projects in it's
subdirectories at most 1 level deep and your done. The variable in question
is `mproj-projects-dirs-list`

```el
(setq mproj-projects-dirs-list
      '("~/.emacs.d/site-lisp-maj/"
        "~/dev/projects"))
```

After that get the project list in the minibuffer via `M-x`
`mproj/open-project`.

For now only a static action is invoked after the project selection,
that resumes to open the project root in dired. That can be changed in
`mproj-default-action` and for example make it open the project in
`perspective-mode`.

## License

Copyright © 2017 Pedro Major <pedro.major@gmail.com>
