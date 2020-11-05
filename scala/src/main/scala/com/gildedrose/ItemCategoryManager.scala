package com.gildedrose

import scala.util.Try

object ItemCategoryManager {
  def decrease(value: Int, by: Int) = {
    if (value > by)
      value - by
    else /*Quality or sellIn will never be negative*/
      0
  }

  def increase(value: Int, by: Int, threshold: Int) = {
    val newQuality = value + by
    if (newQuality <= threshold) /*Quality or sellIn will never be over a specific threshold*/
      newQuality
    else
      threshold
  }
}
