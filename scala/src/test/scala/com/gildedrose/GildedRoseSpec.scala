package com.gildedrose

import com.gildedrose.GildedRoseError.GildedError
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GildedRoseSpec extends AnyWordSpec with Matchers with EitherValues {

  def updateItemForDays(days: Int, item: Item) = {
    def loop(remainingDays: Int, item: Either[GildedError, Item]): Either[GildedError, Item] = {
      if (remainingDays == 0 || item.isLeft) item
      else {
        val updated = item.flatMap(ItemUpdateManager.updateItem)
        loop(remainingDays - 1, updated)
      }
    }

    loop(days, Right(item))
  }

  "Aged items" should {
    "Aged Brie increases its quality by 1 after 1 day" in {
      val item = Item("Aged Brie", 2, 1)

      val updatedItem = updateItemForDays(1, item).right.value

      updatedItem.sellIn shouldBe 1
      updatedItem.quality shouldBe 2
    }

    "Aged Brie increases its quality by 2 after 2 days" in {
      val item = Item("Aged Brie", 2, 3)

      val updatedItem = updateItemForDays(2, item).right.value

      updatedItem.sellIn shouldBe 0
      updatedItem.quality shouldBe 5
    }

    "Aged Brie quality is bounded by 50" in {
      val item = Item("Aged Brie", 2, 49)

      val updatedItem = updateItemForDays(4, item).right.value

      updatedItem.sellIn shouldBe 0
      updatedItem.quality shouldBe 50
    }
  }

  "Legendary items" should {
    "Sulfuras is never changed" in {
      val item = Item("Sulfuras, Hand of Ragnaros", 2, 3)

      val updatedItem = updateItemForDays(5, item).right.value

      updatedItem.sellIn shouldBe 2
      updatedItem.quality shouldBe 3
    }
  }

  "Backstage passes" should {
    "Backstage increases its quality as it's sellin value approaches" in {
      val item = Item("Backstage passes to Metallica concert", 20, 10)

      val updatedItem = updateItemForDays(1, item).right.value

      updatedItem.sellIn shouldBe 19
      updatedItem.quality shouldBe 11
    }

    "Backstage increases its quality by 2 when there are 10 days or less" in {
      val item = Item("Backstage passes to Metallica concert", 8, 10)

      val updatedItem = updateItemForDays(1, item).right.value

      updatedItem.sellIn shouldBe 7
      updatedItem.quality shouldBe 12
    }

    "Backstage increases its quality by 3 when there are 5 days or less" in {
      val item = Item("Backstage passes to Metallica concert", 4, 10)

      val updatedItem = updateItemForDays(1, item).right.value

      updatedItem.sellIn shouldBe 3
      updatedItem.quality shouldBe 13
    }

    "Backstage drops its quality to 0 when sellin value exceeded" in {
      val item = Item("Backstage passes to Metallica concert", 0, 10)

      val updatedItem = updateItemForDays(1, item).right.value

      updatedItem.sellIn shouldBe 0
      updatedItem.quality shouldBe 0
    }
  }

  "Common items" should {
    "Elixir of the Mongoose quality is decreased each day" in {
      val item = Item("Elixir of the Mongoose", 3, 3)

      val updatedItem = updateItemForDays(2, item).right.value

      updatedItem.sellIn shouldBe 1
      updatedItem.quality shouldBe 1
    }

    "Elixir of the Mongoose quality is bounded by 0" in {
      val item = Item("Elixir of the Mongoose", 3, 3)

      val updatedItem = updateItemForDays(10, item).right.value

      updatedItem.sellIn shouldBe 0
      updatedItem.quality shouldBe 0
    }

    "Elixir of the Mongoose quality degrades twice as fast when sellin pass" in {
      val item = Item("Elixir of the Mongoose", 1, 9)

      val updatedItem = updateItemForDays(3, item).right.value

      updatedItem.sellIn shouldBe 0
      updatedItem.quality shouldBe 4
    }
  }

  "Conjured items" should {
    "Conjured Mana Cake quality is decreased by 2 if in sellin range" in {
      val item = Item("Conjured Mana Cake", 2, 4)

      val updatedItem = updateItemForDays(1, item).right.value

      updatedItem.sellIn shouldBe 1
      updatedItem.quality shouldBe 2
    }

    "Conjured Mana Cake quality is decreased by 4 if not in sellin range" in {
      val item = Item("Conjured Mana Cake", 1, 6)

      val updatedItem = updateItemForDays(2, item).right.value

      updatedItem.sellIn shouldBe 0
      updatedItem.quality shouldBe 0
    }
  }
}
