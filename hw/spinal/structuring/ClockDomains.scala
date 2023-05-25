package structuring

import spinal.core._
import spinal.lib._

// Hardware definition
case class Test0() extends Component {
  val io = new Bundle {
    val coreClock = in Bool()
    val coreReset = in Bool()
    val cond1 = in  Bool()
  }

  //定义新的时钟域
  val coreClockDomain = ClockDomain(io.coreClock, io.coreReset)

  //在设计的某个区域内应用该时钟域
  val coreArea = new ClockingArea(coreClockDomain) {
    val coreClockedRegister = Reg(UInt(4 bits)) init (0)
    when(io.cond1){
      coreClockedRegister := 2
    }
  }
}

object MyTopLevelVerilog extends App {
  Config.spinal.generateVerilog(new Test0())
}

class Counters extends Component {
  val io = new Bundle {
    val enable = in Bool()
    val freeCount, gatedCount = out UInt (4 bits)
  }

  val freeCounter = CounterFreeRun(16)
  io.freeCount := freeCounter.value

  val gatedClk = ClockDomain.current.readClockWire && io.enable
  val gated = ClockDomain(gatedClk, ClockDomain.current.readResetWire)

  //这里在门控加法器中应用门控时钟域
  val gatedCounter = gated(CounterFreeRun(16))
  io.gatedCount := gatedCounter.value
}
object CountersVerilog extends App {
  Config.spinal.generateVerilog(new Counters())
}

class CustomClockExample extends Component {
  val io = new Bundle {
    val clk     = in Bool()
    val resetn  = in Bool()
    val result  = out UInt(4 bits)
  }

  //配置时钟域
  val myClockDomain = ClockDomain(
    clock   = io.clk,
    reset   = io.resetn,
    config  = ClockDomainConfig(
      clockEdge        = RISING,
      resetKind        = ASYNC,
      resetActiveLevel =LOW
    )
  )

  //定义用myClockDomain的区域
  val myArea = new ClockingArea(myClockDomain) {
    val myReg = Reg(UInt(4 bits)) init(7)

    myReg := myReg + 1

    io.result := myReg
  }
}

class InternalClockWithPllExample extends Component {
  val io = new Bundle {
    val clk100M = in Bool()
    val aReset  = in Bool()
    val result  = out UInt(4 bits)
  }
  //myClockDomain.clock会被命名为myClockName_clk
  //myClockDomain.reset会被命名为myClockName_reset
  val myClockDomain = ClockDomain.internal("myClockName")

  //例化PLL(可能是黑盒)
//  val pll = new Pll()
//  pll.io.clkIn := io.clk100M
//
//  //给myClockDomain信号赋值
//  myClockDomain.clock := pll.io.clockOut
//  myClockDomain.reset := io.aReset || !pll.io     //有问题：这里缺少语句

  //之后在myCLockDomain可以任你发挥
  val myArea = new ClockingArea(myClockDomain) {
    val myReg = Reg(UInt(4 bits)) init(7)
    myReg := myReg + 1

    io.result := myReg
  }

}

class ExternalClockExample extends Component {
  val io = new Bundle {
    val result = out UInt(4 bits)
  }

  //在顶层你有两个信号： myClockName_clk和myClockName_reset
  val myClockDomain = ClockDomain.external("myClockName")

  val myArea = new ClockingArea(myClockDomain) {
    val myReg = Reg(UInt(4 bits)) init(7)
    myReg := myReg + 1

    io.result := myReg
  }
}
object ExternalClockExampleVerilog extends App {
  Config.spinal.generateVerilog(new ExternalClockExample())
}

//             _____                        _____             _____
//            |     |  (crossClockDomain)  |     |           |     |
//  dataIn -->|     |--------------------->|     |---------->|     |--> dataOut
//            | FF  |                      | FF  |           | FF  |
//  clkA   -->|     |              clkB -->|     |   clkB -->|     |
//  rstA   -->|_____|              rstB -->|_____|   rstB -->|_____|

//补全时钟和复位引脚所通过哪个模块IO给定的
class CrossingExample0 extends Component {
  val io = new Bundle {
    val clkA = in Bool()
    val rstA = in Bool()

    val clkB = in Bool()
    val rstB = in Bool()

    val dataIn = in Bool()
    val dataOut = out Bool()
  }

  //clkA时钟域下的样本输入
  val area_clkA = new ClockingArea(ClockDomain(io.clkA, io.rstA)) {
    val reg = RegNext(io.dataIn) init(False)
  }

  //两级寄存器避免亚稳态
  val area_clkB = new ClockingArea(ClockDomain(io.clkB, io.rstB)) {
    val buf0 = RegNext(area_clkA.reg) init(False) addTag(crossClockDomain)
    val buf1 = RegNext(buf0)          init(False)
  }

  io.dataOut := area_clkB.buf1
}
object CrossingExampleVerilog extends App {
  Config.spinal.generateVerilog(new CrossingExample0())
}

//另一种时钟域以参数方式给定的实现方式
class CrossingExample1(clkA: ClockDomain, clkB: ClockDomain) extends Component {
  val io = new Bundle {
    val dataIn  = in Bool()
    val dataOut = out Bool()
  }

  //clkA时钟域下的样本输入
  val area_clkA = new ClockingArea(clkA) {
    val reg = RegNext(io.dataIn) init(False)
  }

  //两级寄存器避免亚稳态
  val area_clkB = new ClockingArea(clkB) {
    val buf0 = RegNext(area_clkA.reg) init(False) addTap(crossClockDomain)
    val buf1 = RegNext(buf0) init(False)
  }

  io.dataOut := area_clkB.buf1
}

