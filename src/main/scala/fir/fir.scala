// Finitie impulse filter
package fir

import scopt.OParser
import java.io.File

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{ChiselStage, FirtoolOption}

import dsptools._
import dsptools.numbers._
//import breeze.math.Complex

class fir (n: Int = 16, coeffs: Seq[Int]=Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16), gainBits: Int = 10) extends Module {
    val io = IO(new Bundle {
        val scale       = Input(UInt(gainBits.W))
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

  case class Config(
      td: String = ".",
      coeffs: Seq[Int] = Seq(),
      n: Int = 0,
      gainBits: Int = 0
  )

  val builder = OParser.builder[Config]

  val parser1 = {
    import builder._
    OParser.sequence(
      programName("fir"),
      opt[String]('t', "target-dir")
        .action((x, c) => c.copy(td = x))
        .text("Verilog target directory"),
      opt[Seq[String]]('c', "coeffs")
        .valueName("<c1>,<c2>...")
        .text("FIR Coefficients")
        .action((x, c) => c.copy(coeffs = x.toList.map((s: String) => s.toInt))),
      opt[Int]('n', "n")
        .text("Number of taps used")
        .action((x, c) => c.copy(n = x)),
      opt[Int]('g', "gainBits")
        .text("Number of gain bits used")
        .action((x, c) => c.copy(gainBits = x))
    )
  }

  OParser.parse(parser1, args, Config()) match {
    case Some(config) => {
      // These lines generate the Verilog output
      (new circt.stage.ChiselStage).execute(
        { Array("--target", "systemverilog") ++ Array("-td", config.td) },
        Seq(
          ChiselGeneratorAnnotation(() => {
            new fir(
              config.n,		
              config.coeffs,
              config.gainBits
            )
          }),
          FirtoolOption("--disable-all-randomization")
        )
      )
    }
    case _ => {
      println("Could not parse arguments")
    }
  }
}


