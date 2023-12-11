// See LICENSE_AALTO.txt for license details

package fir.config

import net.jcazevedo.moultingyaml._
import net.jcazevedo.moultingyaml.DefaultYamlProtocol._
import scala.math.BigInt
import scala.io.Source
import chisel3._

/** FIR parameter case class
  */
case class FirGeneric(
  syntax_version:     Option[Int], // None for scala instantiation
  resolution:	      Int,
  gainBits:           Int
)

case class FirTaps(
  H:                  Seq[Double]
)

case class FirConfig(
  syntax_version:     Option[Int], // None for scala instantiation
  resolution:         Int,
  H:		      Seq[Int],
  gainBits:           Int
)

object FirConfig {
  implicit val firConfigFormat = yamlFormat3(FirGeneric)

  implicit val dHFormat = yamlFormat1(FirTaps)

  // TODO: Update this to always match the major version number of the release
  val syntaxVersion = 2

  /** Exception type for FIR config parsing errors */
  class FirConfigParseException(msg: String) extends Exception(msg)

  /** Type for representing error return values from a function */
  case class Error(msg: String) {
    /** Throw a parsing exception with a debug message. */
    def except() = { throw new FirConfigParseException(msg) }

    /** Abort program execution and print out the reason */
    def panic() = {
      System.err.println(msg)
      System.exit(-1)
    }
  }

  /** parse legal syntax version from config yaml AST */
  private[config] def parseSyntaxVersion(yamlAst: YamlValue): Either[BigInt,Error] = {
    // get version number as an integer
    val version: BigInt = yamlAst.asYamlObject.fields.get(YamlString("syntax_version")) match {
      case Some(version) => version match {
        case maybeDecimal: YamlNumber => maybeDecimal.asInstanceOf[YamlNumber].value.toBigIntExact match {
          case Some(integer) => integer
          case None => return Right(Error(s"Top-level key `syntax_version` must have an integer value. $version is not!"))
        }
        case _ => return return Right(Error(s"Top-level key `syntax_version` must have an integer value. $version is not!"))
      }
      case None => return Right(Error("Missing required top-level key: `syntax_version`."))
    }
    if (syntaxVersion != version)
      return Right(Error(s"Unsupported syntax version: $version.\n- Supported versions: $syntaxVersion"))
    Left(version)
  }

  def loadFromFile(filename: String): Either[FirConfig, Error] = {
    println(s"\nLoading fir configuration from file: $filename")
    var fileString: String = ""
    try {
      val bufferedSource = Source.fromFile(filename)
      fileString = bufferedSource.getLines.mkString("\n")
      bufferedSource.close
    } catch {
      case e: Exception => return Right(Error(e.getMessage()))
    }
    
    // print file contents as troubleshooting info
    println("\nYAML configuration file contents:")
    //println(s"```\n$fileString\n```")

    // Determine syntax version
    val yamlAst = fileString.parseYaml
    val syntaxVersion = parseSyntaxVersion(yamlAst)
    syntaxVersion match {
      case Left(value) => ()
      case Right(err) => return Right(err)
    }

    // Parse FirConfig from YAML AST
    val generic = yamlAst.convertTo[FirGeneric]
    val taps = yamlAst.convertTo[FirTaps]

    val config = new FirConfig(generic.syntax_version, generic.resolution, taps.H.map(_ * (math.pow(2, generic.resolution - 1) / 2 - 1)).map(_.toInt), generic.gainBits)

    println("resolution:")
    println(config.resolution)

    println("taps:")
    println(config.H)

    println("gainBits:")
    println(config.gainBits)

    Left(config)
  }
}
