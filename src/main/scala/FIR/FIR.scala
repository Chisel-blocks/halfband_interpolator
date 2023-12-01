// Finitie impulse filter
package fir
import config._
import config.{FirConfig}

import java.io.File

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.stage.ChiselGeneratorAnnotation

import dsptools._
import dsptools.numbers.DspComplex

class FIR(config: FirConfig) extends Module {
    val io = IO(new Bundle {
        val scale       = Input(UInt(config.gainBits.W))
        val iptr_A      = Input(DspComplex(SInt(config.n.W), SInt(config.n.W)))
        val Z		= Output(DspComplex(SInt(config.n.W), SInt(config.n.W)))
    })
    val inreg = RegInit(DspComplex.wire(0.S(config.n.W), 0.S(config.n.W))) //registers for sampling rate reduction
   
    inreg := io.iptr_A

    val subcoeffs = config.H.indices.map(config.H(_)) //Coeffs
    //println(subcoeffs)

    val tapped = subcoeffs.reverse.map(tap => inreg * tap) //Coeffs * regs
    val registerchain = RegInit(VecInit(Seq.fill(tapped.length + 1)(DspComplex.wire(0.S(config.n.W), 0.S(config.n.W)))))

     //Summation
    for ( i <- 0 to tapped.length - 1) {
        if (i == 0) {
            registerchain(i + 1) := tapped(i)
        } else {
            registerchain(i + 1) := tapped(i)
        }
    }

    val subfil = registerchain(tapped.length)

    val outreg = RegInit(DspComplex.wire(0.S(config.n.W), 0.S(config.n.W)))

    //Scaling
    outreg.real := (subfil.real * io.scale)(config.n, 0).asSInt
    outreg.imag := (subfil.imag * io.scale)(config.n, 0).asSInt

    io.Z := outreg
}

/** Generates verilog */
object FIR extends App with OptionParser {
  // Parse command-line arguments
  val (options, arguments) = getopts(default_opts, args.toList)
  printopts(options, arguments)

  val config_file = options("config_file")
  var fir_config: Option[FirConfig] = None
  FirConfig.loadFromFile(config_file) match {
    case Left(config) => {
      fir_config = Some(config)
    }
    case Right(err) => {
      System.err.println(s"\nCould not load FIR configuration from file:\n${err.msg}")
      System.exit(-1)
    }
  }

  // Generate verilog
  val annos = Seq(ChiselGeneratorAnnotation(() => new FIR(config=fir_config.get)))
  (new ChiselStage).execute(arguments.toArray, annos)
}

/** Module-specific command-line option parser */
trait OptionParser {
  // Module specific command-line option flags
  val available_opts: List[String] = List(
      "-config_file"
  )

  // Default values for the command-line options
  val default_opts : Map[String, String] = Map(
    "config_file"->"fir-config.yml"
  )

  /** Recursively parse option flags from command line args
   * @param options Map of command line option names to their respective values.
   * @param arguments List of arguments to parse.
   * @return a tuple whose first element is the map of parsed options to their values 
   *         and the second element is the list of arguments that don't take any values.
   */
  def getopts(options: Map[String, String], arguments: List[String]) : (Map[String, String], List[String]) = {
    val usage = s"""
      |Usage: ${this.getClass.getName.replace("$","")} [-<option> <argument>]
      |
      | Options
      |     -config_file        [String]  : Generator YAML configuration file name. Default "fir-config.yml".
      |     -h                            : Show this help message.
      """.stripMargin

    // Parse next elements in argument list
    arguments match {
      case "-h" :: tail => {
        println(usage)
        sys.exit()
      }
      case option :: value :: tail if available_opts contains option => {
        val (newopts, newargs) = getopts(
            options ++ Map(option.replace("-","") -> value), tail
        )
        (newopts, newargs)
      }
      case argument :: tail => {
        val (newopts, newargs) = getopts(options, tail)
        (newopts, argument.toString +: newargs)
      }
      case Nil => (options, arguments)
    }
  }

  /** Print parsed options and arguments to stdout */
  def printopts(options: Map[String, String], arguments: List[String]) = {
    println("\nCommand line options:")
    options.nonEmpty match {
      case true => for ((k,v) <- options) {
        println(s"  $k = $v")
      }
      case _ => println("  None")
    }
    println("\nCommand line arguments:")
    arguments.nonEmpty match {
      case true => for (arg <- arguments) {
        println(s"  $arg")
      }
      case _ => println("  None")
    }
  }
}

