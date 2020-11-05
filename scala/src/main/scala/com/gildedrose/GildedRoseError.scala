package com.gildedrose

object GildedRoseError {
  sealed trait GildedError extends Throwable
  final case class CouldNotUpdateItemError(item: Item) extends GildedError
  final case class UnknownItemCategoryError(item: Item) extends GildedError
  /*This could use a smart constructor to ensure that a given item actually has these negative fields: ItemField*
  * but for simplicity I'm just gonna leave it like this here because adding a smart constructor here wouldn't
  * take much of a change in code. Just don't want to over engineer it here for now*/
  final case class NegativeItemFieldError(item: Item, negativeFields: ItemField*) extends GildedError

  sealed trait ItemField
  final case object SellIn extends ItemField
  final case object Quality extends ItemField
}
