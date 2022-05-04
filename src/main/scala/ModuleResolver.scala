package com.github.jtrim777.djinn

import defns.{ModuleInstance, Version}
import cats.effect.IO

trait ModuleResolver {
  /**
   * Produce the metadata for all versions of the specified module within the given version range
   * Output should be sorted in descending version order
   * @param id The identifier of the module to search for
   * @param versionSpec The specification of the valid versions
   */
  def resolve(id: String, versionSpec: Version): IO[Seq[ModuleInstance]]
}
