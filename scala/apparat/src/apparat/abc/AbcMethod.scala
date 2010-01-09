package apparat.abc

import apparat.bytecode.Bytecode

class AbcMethodParameter(val typeName: AbcName) {
	var name: Option[Symbol] = None
	var optional = false
	var optionalType: Option[Int] = None
	var optionalVal: Option[Any] = None
}

class AbcMethod(val parameters: Array[AbcMethodParameter], val returnType: AbcName,
				val name: Symbol, val needsArguments: Boolean, val needsActivation: Boolean, val needsRest: Boolean,
				val hasOptionalParameters: Boolean, val ignoreRest: Boolean, val isNative: Boolean,
				val setsDXNS: Boolean, val hasParameterNames: Boolean) {
	var body: Option[AbcMethodBody] = None

	override def toString = "[AbcMethod name: " + name.toString() + "]"
}

class AbcMethodBody(val maxStack: Int, val localCount: Int, val initScopeDepth: Int,
					val maxScopeDepth: Int, var code: Array[Byte], var exceptions: Array[AbcExceptionHandler],
					val traits: Array[AbcTrait], var bytecode: Option[Bytecode] = None)

class AbcExceptionHandler(val from: Int, val to: Int, val target: Int, val typeName: AbcName, val varName: AbcName)