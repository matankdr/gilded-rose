package com.gildedrose

import com.gildedrose.CategoryUpdater._
import com.gildedrose.GildedRoseError.GildedError
import com.gildedrose.ItemCategoryManager.decrease

object ItemUpdateManager {
  def updateItems(items: List[Item]): Either[GildedError, List[Item]] = {
    items.map(updateItem).foldLeft[Either[GildedError, List[Item]]](Right(List())) {
      case (Right(agg), Right(updatedItem)) => Right(agg :+ updatedItem)
      case (Left(err), _)                   => Left(err)
      case (Right(_), Left(err))            => Left(err)
    }
  }

  val updateItem: Item => Either[GildedError, Item] = { case item @ Item(name, sellIn, quality) =>
    classifyItem(item).map {
      case Common        => Item(name, decrease(sellIn, 1), Common.updateQuality(sellIn, quality))
      case Conjured      => Item(name, decrease(sellIn, 1), Conjured.updateQuality(sellIn, quality))
      case BackStagePass => Item(name, decrease(sellIn, 1), BackStagePass.updateQuality(sellIn, quality))
      case Legendary     => item
      case Aged          => Item(name, decrease(sellIn, 1), Aged.updateQuality(sellIn, quality))
    }
  }
}
