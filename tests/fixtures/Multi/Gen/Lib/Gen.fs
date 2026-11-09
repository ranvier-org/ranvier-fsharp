namespace GenLib

module Opened =
    let opened = 1

module Api =
    let f (x: int) : int = x + 1

    let internal g (x: int) : int = x + 2
