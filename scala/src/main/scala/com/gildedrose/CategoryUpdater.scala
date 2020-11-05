package com.gildedrose

import com.gildedrose.GildedRoseError.{GildedError, NegativeItemFieldError, Quality, SellIn, UnknownItemCategoryError}
import com.gildedrose.Inventory.catalog
import com.gildedrose.ItemCategoryManager.{decrease, increase}
import com.sun.net.httpserver.Authenticator.Success

import scala.util.{Failure, Success, Try}

object CategoryUpdater {
  trait Category {
    def updateQuality(sellIn: Int, quality: Int): Int
  }

  case object Legendary extends Category { //Do nothing but maybe later this could change so overriding is necessary
    override def updateQuality(sellIn: Int, quality: Int) = quality
  }

  case object Aged extends Category {
    override def updateQuality(sellIn: Int, quality: Int) =
      increase(quality, 1, 50)
  }

  case object Conjured extends Category {
    override def updateQuality(sellIn: Int, quality: Int) = {
      val by = if (sellIn <= 0) 4 else 2
      decrease(quality, by)
    }
  }

  case object Common extends Category {
    override def updateQuality(sellIn: Int, quality: Int) = {
      val by = if(sellIn <= 0) 2 else 1 //Once sell time has passed it degrades twice as fast
      decrease(quality, by)
    }
  }

  case object BackStagePass extends Category {
    override def updateQuality(sellIn: Int, quality: Int): Int = {
      val by =
        if      (sellIn > 10) Some(1)
        else if (sellIn >  5) Some(2)
        else if (sellIn >= 1) Some(3)
        else                  None

      by.fold(0)(by => increase(quality, by, 50))
    }
  }

  def classifyItem(item: Item): Either[GildedError, Category] = {
    val maybeClassifiedItem =
      if(isCommon(item))             Right(Common)
      else if(isConjured(item))      Right(Conjured)
      else if(isBackStagePass(item)) Right(BackStagePass)
      else if(isLegendary(item))     Right(Legendary)
      else if(isAged(item))          Right(Aged)
      else                           Left(UnknownItemCategoryError(item))

    for {
      _ <- itemFieldsAreValid(item)
      classifiedItem <- maybeClassifiedItem
    } yield classifiedItem
  }

  private def isCommon(item: Item): Boolean =         catalog(Common).contains(item.name.trim)
  private def isLegendary(item: Item): Boolean =      catalog(Legendary).contains(item.name.trim)
  private def isConjured(item: Item): Boolean =       catalog(Conjured).contains(item.name.trim)
  private def isBackStagePass(item: Item): Boolean =  catalog(BackStagePass).contains(item.name.trim)
  private def isAged(item: Item): Boolean =           catalog(Aged).contains(item.name.trim)

  //Backstage passes to a Metallica concert
  //Backstage passes to Metallica concert

  private def itemFieldsAreValid(item: Item): Either[NegativeItemFieldError, Unit] = {
    if(item.sellIn < 0 && item.quality < 0) Left(NegativeItemFieldError(item, SellIn, Quality)) else
      if(item.sellIn < 0) Left(NegativeItemFieldError(item, SellIn)) else
        if(item.quality < 0) Left(NegativeItemFieldError(item, Quality)) else Right(())
  }
}
