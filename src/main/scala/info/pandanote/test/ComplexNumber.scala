package main.scala.info.pandanote.test

import scala.reflect.ClassTag

import scala.math._



// BreezeのComplexクラスを元にして複素数を扱うためのクラスを書いてみました。

// 電気工学的な虚数単位が設定できるようにコンストラクタの引数を変更しています。

case class ComplexNumber(real: Double, imag: Double = 0.0, imagUnit: String = "i") {

  // 虚数成分の符号を考慮して、少し見やすくしてみました。

  override def toString(): String = real + (if (imag==0.0) "" else ((if (imag>0.0)"+"else"-") + (if (imag>0.0)imag else -imag) + imagUnit))

  def re() = real

  def im() = imag



  def +(that: ComplexNumber) = ComplexNumber(this.real+that.real,this.imag+that.imag,this.imagUnit)

  def +(that: Int) = ComplexNumber(this.real+that,this.imag,this.imagUnit)

  def +(that: Long) = ComplexNumber(this.real+that,this.imag,this.imagUnit)

  def +(that: Float) = ComplexNumber(this.real+that,this.imag,this.imagUnit)

  def +(that: Double) = ComplexNumber(this.real+that,this.imag,this.imagUnit)

  def -(that: ComplexNumber) = ComplexNumber(this.real-that.real,this.imag-that.imag,this.imagUnit)

  def -(that: Int) = ComplexNumber(this.real-that,this.imag,this.imagUnit)

  def -(that: Long) = ComplexNumber(this.real-that,this.imag,this.imagUnit)

  def -(that: Float) = ComplexNumber(this.real-that,this.imag,this.imagUnit)

  def -(that: Double) = ComplexNumber(this.real-that,this.imag,this.imagUnit)

  def *(that: ComplexNumber) = ComplexNumber(this.real*that.real - this.imag*that.imag, this.real*that.imag+this.imag*that.real,this.imagUnit)

  def *(that: Int) = ComplexNumber(this.real*that,this.imag*that,this.imagUnit)

  def *(that: Long) = ComplexNumber(this.real*that,this.imag*that,this.imagUnit)

  def *(that: Float) = ComplexNumber(this.real*that,this.imag*that,this.imagUnit)

  def *(that: Double) = ComplexNumber(this.real*that,this.imag*that,this.imagUnit)

  def /(that: ComplexNumber) = {

    val xnorm = that.real*that.real+that.imag*that.imag

    ComplexNumber((this.real*that.real+this.imag*that.imag)/xnorm,(this.imag*that.real-this.real*that.imag)/xnorm,this.imagUnit)

  }

  def /(that: Int) = ComplexNumber(this.real/that,this.imag/that,this.imagUnit)

  def /(that: Long) = ComplexNumber(this.real/that,this.imag/that,this.imagUnit)

  def /(that: Float) = ComplexNumber(this.real/that,this.imag/that,this.imagUnit)

  def /(that: Double) = ComplexNumber(this.real/that,this.imag/that,this.imagUnit)

  // 複素数の剰余計算というものが定義されているのかどうかわかりませんが、書いてみました。

  // この部分はBreezeの実装とは異なります。

  def %(that: ComplexNumber) = {

    val div = this./(that)

    this - ComplexNumber(Math.round(div.re()),Math.round(div.im()),this.imagUnit)*that

  }

  def %(that: Int): ComplexNumber = this.%(ComplexNumber(that, 0, this.imagUnit))

  def %(that: Long): ComplexNumber = %(ComplexNumber(that, 0, this.imagUnit))

  def %(that: Float): ComplexNumber = %(ComplexNumber(that, 0, this.imagUnit))

  def %(that: Double): ComplexNumber = %(ComplexNumber(that, 0, this.imagUnit))



  def unary_- = ComplexNumber(-real,-imag,this.imagUnit)

  def abs = math.sqrt(real*real+imag*imag)

  def conjugate = ComplexNumber(real,-imag,this.imagUnit)

  def log = ComplexNumber(math.log(abs),math.atan2(imag,real),this.imagUnit)

  def exp = {

    val expreal = math.exp(real)

    ComplexNumber(expreal*math.cos(imag),expreal*math.sin(imag),this.imagUnit)

  }

  def pow(that: Int): ComplexNumber = pow(ComplexNumber(that,0,this.imagUnit))

  def pow(that: Double): ComplexNumber = pow(ComplexNumber(that,0,this.imagUnit))



  def pow(that: ComplexNumber): ComplexNumber = {

    if (that == ComplexNumber.zero) ComplexNumber.one

    else if (this == ComplexNumber.zero) {

      if (that.imag != 0.0 || that.real < 0.0) ComplexNumber.nan

      else ComplexNumber.zero

    } else {

      val c = log * that

      val expReal = math.exp(c.real)

      ComplexNumber(expReal*math.cos(c.imag),expReal*math.sin(c.imag),this.imagUnit)

    }

  }



  // こんなのも定義してみます。

  def **(that: ComplexNumber): ComplexNumber = pow(that)

  def **(that: Double): ComplexNumber = pow(that)

  def **(that: Int): ComplexNumber = pow(that)



  override def equals(that: Any) = that match {

    case that: ComplexNumber => this.real == that.real && this.imag == that.imag

    case real: Double => this.real == real && this.imag == 0

    case real: Int => this.real == real && this.imag == 0

    case real: Short => this.real == real && this.imag == 0

    case real: Long => this.real == real && this.imag == 0

    case real: Float => this.real == real && this.imag == 0

    case _ => false

  }



  def >(that: ComplexNumber) =

      (this.real > that.real || (this.real == that.real && this.imag > that.imag))



  def >=(that: ComplexNumber) =

      (this.real >= that.real || (this.real == that.real && this.imag >= that.imag))



  def <(that: ComplexNumber) =

      (this.real < that.real || (this.real == that.real && this.imag < that.imag))



  def <=(that: ComplexNumber) =

      (this.real <= that.real || (this.real == that.real && this.imag <= that.imag))



  // 試しにこんな演算子を定義してみます。

  def <>(that: ComplexNumber) =

      (this.real != that.real || this.imag != that.imag)



  override def hashCode() = real.## ^ imag.##

}



object ComplexNumber {

  outer =>

    val one = new ComplexNumber(1,0)

    val zero = new ComplexNumber(0,0)

    val imaginaryUnit = new ComplexNumber(0,1)

    val nan = new ComplexNumber(Double.NaN,Double.NaN)

    // Javaでいうところのstaticなメソッドみたいなものです。

    def exp(that: ComplexNumber): ComplexNumber = that.exp

    def log(that: ComplexNumber): ComplexNumber = that.log



    // 実数に複素数を作用させる演算が定義されていなかったので、追加しました。

    // 試行錯誤の末、以下の一行になりました…

    implicit def fromDouble(d: Double) = new ComplexNumber(d)

}
