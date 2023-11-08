// Finitie impulse filter
package fir

import chisel3.experimental.{withClock, withClockAndReset}
import chisel3._
import dsptools._
import dsptools.numbers._
import breeze.math.Complex

import fir_BW_045_N_40._

class fir (n: Int = 16, coeffs: Seq[Int]=Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16), gainbits: Int = 10) extends Module {
    val io = IO(new Bundle {
        val scale       = Input(UInt(gainbits.W))
        val iptr_A      = Input(DspComplex(SInt(n.W), SInt(n.W)))
        val Z		= Output(DspComplex(SInt(n.W), SInt(n.W)))
    })
    val inreg = RegInit(DspComplex.wire(0.S(n.W), 0.S(n.W))) //registers for sampling rate reduction
   
    inreg := io.iptr_A

    val subcoeffs = coeffs.indices.map(coeffs(_)) //Coeffs
    println(subcoeffs)

    val tapped = subcoeffs.reverse.map(tap => inreg * tap) //Coeffs * regs
    val registerchain = RegInit(VecInit(Seq.fill(tapped.length + 1)(DspComplex.wire(0.S(n.W), 0.S(n.W)))))

     //Summation
    for ( i <- 0 to tapped.length - 1) {
        if (i == 0) {
            registerchain(i + 1) := tapped(i)
        } else {
            registerchain(i + 1) := registerchain(i) + tapped(i)
        }
    }

    val subfil = registerchain(tapped.length)

    val outreg = RegInit(DspComplex.wire(0.S(n.W), 0.S(n.W)))

    //Scaling
    outreg.real := (subfil.real * io.scale)(n, 0).asSInt
    outreg.imag := (subfil.imag * io.scale)(n, 0).asSInt

    io.Z := outreg
}

//This is the object to provide verilog
object fir extends App {
  //Convert coeffs to integers with 16 bit resolution
  val coeffres = 16
  val taps = fir_BW_045_N_40.H.map(_ * (math.pow(2, coeffres - 1) - 1)).map(_.toInt)
  chisel3.Driver.execute(args, () => new fir(coeffs = taps) )
}

