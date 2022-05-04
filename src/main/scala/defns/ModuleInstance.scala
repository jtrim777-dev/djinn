package com.github.jtrim777.djinn
package defns

case class ModuleInstance(id: String, version: Version.Concrete, deps: Seq[Dependency]) {
  def conflictsWith(module: String, version: Version): Boolean =
    deps.exists(d => d.id == module && d.versionSpec.compatibleWith(version).isEmpty)

  def relevantDependency(id: String): Option[Dependency] =
    deps.find(_.id == id)

  override def toString: String = s"$id@$version"
}
