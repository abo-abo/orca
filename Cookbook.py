#* Imports
import pycook.elisp as el

#* Recipes
def test_interactive(recipe):
    elfs = ["orca.el", "targets/server-restart.el"]
    plain = "../swiper/targets/plain.el"
    if el.file_exists_p("../swiper/targets/plain.el"):
        elfs.append(plain)
    return [" ".join(["emacs -Q"] + ["-l " + p for p in elfs])]
