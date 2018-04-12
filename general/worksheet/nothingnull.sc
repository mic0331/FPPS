def error(msg: String) = throw new Error(msg)

val x = null
val y: String = null
//val z: Int = null // this doesn't work because null only works with AnyRef subtypes

if (true) 1 else false  // type AnyVal

if (true) "toto" else false // type Any