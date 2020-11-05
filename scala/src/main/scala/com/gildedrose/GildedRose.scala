package com.gildedrose

class GildedRose(var items: List[Item]) {

  def updateQuality() = {
    ItemUpdateManager.updateItems(items) match {
      case Right(updatedItems) =>
        items = updatedItems

      case err => err
    }
  }
}