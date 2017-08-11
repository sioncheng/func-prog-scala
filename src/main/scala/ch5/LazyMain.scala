package ch5

object LazyMain extends App {

    def if2[A](cond: Boolean, onTrue: ()=> A, onFalse: ()=> A): A = {
        if (cond) onTrue() else onFalse()
    }

    if2(true, ()=> println("true"), ()=>println("false"))

    def if22[A](cond: Boolean, onTrue: =>A, onFalse: =>A): A = {
        if (cond) onTrue else onFalse
    }

    if22(false, println("true"), println("false"))
}
