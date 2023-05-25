package projectname

import spinal.lib._
import spinal.core._
import spinal.lib.bus.amba3.ahblite._
import spinal.lib.bus.amba3.apb
import spinal.lib.bus.amba3.apb._
import spinal.lib.{master, slave}
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite.AxiLite4Utils.Axi4Rich
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.amba4.axilite._

class T2 extends Component{
  val busin = slave(Apb3(Apb3Config(12,32)))
  val busout = master(Apb3(Apb3Config(12,32)))

  busout <> busin
}

object T2Verilog extends App {
  Config.spinal.generateVerilog(new T2())
}


class Ahb2ApbTop(ahbConfig:AhbLite3Config, apbConfig:Apb3Config) extends Component{
  val ahb = slave(AhbLite3(ahbConfig))
  val apb = master(Apb3(apbConfig))
  val bridge = AhbLite3ToApb3Bridge(ahbConfig,apbConfig)
  ahb <> bridge.io.ahb
  apb <> bridge.io.apb
}
object Ahb2ApbTopVerilog extends App {
  Config.spinal.generateVerilog(new Ahb2ApbTop(new AhbLite3Config(32,64),new Apb3Config(32,64)))
}

class Axi4ToAxiLite4(axi4Config:Axi4Config) extends Component{
  val io = new Bundle{
    val axi = slave(Axi4(axi4Config))
    val axilite = master(AxiLite4(AxiLite4Config(addressWidth = axi4Config.addressWidth,dataWidth = axi4Config.dataWidth)))
  }
  io.axi.toLite() >> io.axilite
}
object Axi4ToAxiLite4Verilog extends App {
  Config.spinal.generateVerilog(new Axi4ToAxiLite4(new Axi4Config(dataWidth = 64,addressWidth = 32,idWidth = 4)))
}

class AxiLite4ToAxi4(axilite4config:AxiLite4Config) extends Component{
  val io = new Bundle{
    val axilite = slave(AxiLite4(axilite4config))
    val axi = master(Axi4(Axi4Config(addressWidth = axilite4config.addressWidth,dataWidth = axilite4config.dataWidth,idWidth = 4)))
  }
  io.axi.toLite() << io.axilite
}
object AxiLite4ToAxi4Verilog extends App {
  Config.spinal.generateVerilog(new AxiLite4ToAxi4(new AxiLite4Config(dataWidth = 64,addressWidth = 32)))
}

class AxiCrossbarNx1(slaveNum:Int) extends Component{
  val masterAxi4fg = Axi4Config(
    addressWidth = 32,
    dataWidth = 64,
    idWidth = 4)
  val slaveAxi4Config = masterAxi4fg.copy(idWidth = 8)
  val io = new Bundle{
    val axi4In = Vec(slave(Axi4(masterAxi4fg)),slaveNum)
    val axi4Out = Vec(master(Axi4(slaveAxi4Config)),1)
    axi4In.foreach(Axi4SpecRenamer(_))
    axi4Out.foreach(Axi4SpecRenamer(_))
  }

  val clk, rst_n = in Bool()
  val myClockDomain = ClockDomain(clk, rst_n, config = ClockDomainConfig(resetKind = ASYNC, resetActiveLevel = LOW))
  new ClockingArea(myClockDomain) {
    val crossbarFactoryInst = Axi4CrossbarFactory()
    crossbarFactoryInst.addSlaves(
      (io.axi4Out(0), SizeMapping(0, 4 GiB)),
    )

    io.axi4In.foreach(crossbarFactoryInst.addConnection(_, io.axi4Out))
    io.axi4Out.foreach(axi4 =>
      crossbarFactoryInst.addPipelining(axi4)(
        (masterPort, slavePort) => {
          masterPort.ar >-> slavePort.ar
          slavePort.r >-> masterPort.r
        }
      )(
        (masterPort, slavePort) => {
          masterPort.aw >-> slavePort.aw
          masterPort.w >-> slavePort.w
          slavePort.b >-> masterPort.b
        }
      )
    )
    crossbarFactoryInst.build()
  }

}

object AxiCrossbarNx1Verilog extends App {
  Config.spinal.generateVerilog(new AxiCrossbarNx1(3))
}

class AxiCrossbar1x7() extends Component{
  val Axi4fg = Axi4Config(
    addressWidth = 31,
    dataWidth = 32,
    idWidth = 4)
  val io = new Bundle{
    val axi4In = Vec(slave(Axi4(Axi4fg)),1)
    val axi4Out = Vec(master(Axi4(Axi4fg)),7)
    axi4In.foreach(Axi4SpecRenamer(_))
    axi4Out.foreach(Axi4SpecRenamer(_))
  }

  val clk,rst_n = in Bool()
  val myClockDomain = ClockDomain(clk,rst_n,config = ClockDomainConfig(resetKind = ASYNC,resetActiveLevel = LOW))
  new ClockingArea(myClockDomain){
    val crossbarFactoryInst = Axi4CrossbarFactory()
    crossbarFactoryInst.addSlaves(
      (io.axi4Out(0), SizeMapping(0x60000000, 64 KiB)),
      (io.axi4Out(1), SizeMapping(0x60010000, 64 KiB)),
      (io.axi4Out(2), SizeMapping(0x60020000, 64 KiB)),
      (io.axi4Out(3), SizeMapping(0x60030000, 64 KiB)),
      (io.axi4Out(4), SizeMapping(0x60040000, 64 KiB)),
      (io.axi4Out(5), SizeMapping(0x60050000, 64 KiB)),
      (io.axi4Out(6), SizeMapping(0x60060000, 64 KiB))
    )

    io.axi4In.foreach(crossbarFactoryInst.addConnection(_, io.axi4Out))
    io.axi4Out.foreach(axi4 =>
      crossbarFactoryInst.addPipelining(axi4)(
        (masterPort, slavePort) => {
          masterPort.ar >-> slavePort.ar
          slavePort.r >-> masterPort.r
        }
      )(
        (masterPort, slavePort) => {
          masterPort.aw >-> slavePort.aw
          masterPort.w >-> slavePort.w
          slavePort.b >-> masterPort.b
        }
      )
    )
    crossbarFactoryInst.build()
  }

}
object AxiCrossbar1x7Verilog extends App {
  Config.spinal.generateVerilog(new AxiCrossbar1x7())
}