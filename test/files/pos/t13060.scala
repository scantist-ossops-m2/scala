class C {
  def id[A](r: A): A = r
  def bug(x: Int, e: Boolean): Unit = {
    x match {
      case 1 => id(())
      case 2 if e =>
    }
    println()
  }
}
