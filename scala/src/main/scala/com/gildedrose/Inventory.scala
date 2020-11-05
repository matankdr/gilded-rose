package com.gildedrose

import com.gildedrose.CategoryUpdater._

object Inventory {
  type ItemName = String

  val catalog: Map[Category, List[ItemName]] =
    Map(
      Legendary     -> List("Sulfuras, Hand of Ragnaros"),
      Conjured      -> List("Conjured Mana Cake"),
      Common        -> List("Elixir of the Mongoose", "+5 Dexterity Vest"),
      BackStagePass -> List("Backstage passes to Metallica concert"),
      Aged          -> List("Aged Brie")
    )
}
