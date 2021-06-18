package com.github.aleperaltabazas.json.benchmark

import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.core.Version
import com.fasterxml.jackson.core.type.TypeReference
import com.fasterxml.jackson.databind.DeserializationFeature
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.PropertyNamingStrategies
import com.fasterxml.jackson.datatype.joda.JodaModule
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule
import com.fasterxml.jackson.module.afterburner.AfterburnerModule
import com.fasterxml.jackson.module.kotlin.KotlinModule
import com.github.aleperaltabazas.json.benchmark.data.PersonByReflection
import com.github.aleperaltabazas.json.benchmark.functions.diff
import kotlinx.coroutines.delay
import kotlinx.coroutines.runBlocking
import java.io.File
import java.nio.charset.StandardCharsets

val test: String = File("../json/test.json")
    .readText(StandardCharsets.UTF_8)

fun main() {
    jacksonReflectionRead()
}

private fun jacksonReflectionRead() = runBlocking {
    fun defaultObjectMapper() = ObjectMapper().apply {
        registerModule(KotlinModule())
        registerModule(JavaTimeModule())
        registerModule(JodaModule())
        registerModule(AfterburnerModule())
        configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
        propertyNamingStrategy = PropertyNamingStrategies.LOWER_CAMEL_CASE
        setSerializationInclusion(JsonInclude.Include.NON_NULL)
    }

    val objectMapper = defaultObjectMapper()

    bench("Jackson: Default behaviour (reflection field caching) over 100 iterations", 100) {
        objectMapper.readValue(test, object : TypeReference<List<PersonByReflection>>() {})
    }

    bench("Jackson: No caching over 100 iterations", 100) {
        defaultObjectMapper().readValue(test, object : TypeReference<List<PersonByReflection>>() {})
    }

    val ref = object : TypeReference<List<PersonByReflection>>() {}

    bench("Jackson: Default behaviour without recreating the typeref over 100 iterations", 100) {
        objectMapper.readValue(test, ref)
    }
}

private suspend fun bench(title: String, iterations: Int, iterationDelay: Long? = null, eff: () -> Any) {
    val durations = mutableListOf<Long>()

    for (i in 1..iterations) {
        val (res, d) = diff {
            val e = eff()
        }

        durations.add(d)
        iterationDelay?.let { delay(it) }
    }

    val fastest = durations.minOrNull()!!
    val slowest = durations.maxOrNull()!!
    val average = durations.average()

    println("=== $title ===")
    println()
    println("First read: ${durations.first()}")
    println("Last read: ${durations.last()}")
    println("Fastest read: $fastest (at position ${durations.indexOf(fastest) + 1})")
    println("Slowest read: $slowest (at position ${durations.indexOf(slowest) + 1})")
    println("Average read: $average")
    println()
}
