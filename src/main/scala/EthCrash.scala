import scala.collection.mutable.ListBuffer

case class Vault(gem: Double, debt: Double, tpe: String, 
  liquidationRatio: Double, managedRatio: Double, penalty: Double=0.13)


class System {
  var time = 0 // Hour
  var vaults = ListBuffer.empty[Vault]
  var liquidations = ListBuffer.empty[Vault]
  var ethPrice = 1420.0
  val priceDrop = ethPrice*0.01
  val numberOfDrops = 85

  var liquidationPnL = 0.0

  val box = 15*1000*1000

  def init(ethA: Int, ethB: Int, ethC: Int) = {
    (0 until ethA).foreach { i =>
      val colRatio = Math.random()*3 + 1.5 // 150% to 450%
      vaults += Vault(box/ethPrice*colRatio, box, "ETH-A", 1.5, 0.30)
    }
    (0 until ethB).foreach { i =>
      val colRatio = Math.random() + 1.25 // 125% to 225%
      vaults += Vault(box/ethPrice*colRatio, box, "ETH-B", 1.25, 0.50)
    }
    (0 until ethC).foreach { i =>
      val colRatio = Math.random()*3 + 2 // 200% to 500%
      vaults += Vault(box/ethPrice*colRatio, box, "ETH-C", 2.0, 0.30)
    }
  }

  def run() {
    while(liquidations.nonEmpty || time < numberOfDrops)
      this.step()
  }

  def step() {
    time += 1
    // Drop Eth price by 1%
    if(time < numberOfDrops)
      ethPrice -= priceDrop

      
    println(s"Time ${time} ETH price ${ethPrice}")

    val vaultsToLiquidate = vaults
      .filter( v => v.gem*ethPrice < box*v.liquidationRatio)
      .filterNot(v => Math.random() < v.managedRatio) // Remove those that are managed

    // Safe vaults
    vaults = vaults.filter( v => v.gem*ethPrice >= box*v.liquidationRatio)

    liquidations ++= vaultsToLiquidate

    // One actual liquidation every 6 hours
    if(time % 6 == 0 && liquidations.nonEmpty) {
      val v = liquidations.head
      liquidations = liquidations.drop(1)
      
      val profit = Math.min(v.debt*v.penalty, v.gem*ethPrice - v.debt)

      liquidationPnL += profit

      println(s"Liquidation of ${v.tpe} vault. Profit of ${profit.toInt}. Lifetime profit of ${(liquidationPnL/1000/1000).toInt}M. Vault in liquidation ${liquidations.size}")
    }
  }
}


object EthCrash extends App {
  val system = new System()

  system.init(10, 3, 50) // number of vaults for aech type, each vault is 15M

  system.run()

}
