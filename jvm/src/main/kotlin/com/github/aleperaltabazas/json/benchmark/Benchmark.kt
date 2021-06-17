package com.github.aleperaltabazas.json.benchmark

import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.core.type.TypeReference
import com.fasterxml.jackson.databind.DeserializationFeature
import com.fasterxml.jackson.databind.PropertyNamingStrategies
import com.fasterxml.jackson.datatype.joda.JodaModule
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule
import com.fasterxml.jackson.module.afterburner.AfterburnerModule
import com.fasterxml.jackson.module.kotlin.KotlinModule
import com.fasterxml.jackson.module.kotlin.jacksonObjectMapper
import com.fasterxml.jackson.module.kotlin.readValue
import com.github.aleperaltabazas.json.benchmark.data.Person
import com.google.common.io.Resources
import java.nio.charset.StandardCharsets

val objectMapper = jacksonObjectMapper().also {
    it.registerModule(KotlinModule())
    it.registerModule(JavaTimeModule())
    it.registerModule(JodaModule())
    it.registerModule(AfterburnerModule())
    it.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
    it.propertyNamingStrategy = PropertyNamingStrategies.LOWER_CAMEL_CASE
    it.setSerializationInclusion(JsonInclude.Include.NON_NULL)
}

val test: String = Resources.getResource("test.json")
    .let { Resources.toString(it, StandardCharsets.UTF_8) }
    ?: throw RuntimeException("Error parsing test.json")

fun main() {
    println("=== READ TIME ===\n")

    pureJacksonRead()
    treeRead()

    println("\n=== WRITE TIME ===\n")

    val persons = objectMapper.readValue<List<Person>>(test)

    var start = System.currentTimeMillis()
    persons.forEach(objectMapper::writeValueAsString)
    var end = System.currentTimeMillis()

    println("Jackson write: ${end - start} ms")

    start = System.currentTimeMillis()
    persons.forEach(Person.Companion::write)
    end = System.currentTimeMillis()

    println("Tree write: ${end - start} ms")
}

private fun pureJacksonRead() {
    val action = {
        objectMapper.readValue(test, object : TypeReference<List<Person>>() {})
        Unit
    }

    val averageOver1 = averageDuration(1, action)
    println("Pure jackson average over 1 iterations: $averageOver1 ms")

    val averageOver10 = averageDuration(10, action)
    println("Pure jackson average over 10 iterations: $averageOver10 ms")

    val averageOver100 = averageDuration(100, action)
    println("Pure jackson average over 100 iterations: $averageOver100 ms")

    val averageOver1000 = averageDuration(1000, action)
    println("Pure jackson average over 1000 iterations: $averageOver1000 ms")
}

private fun treeRead() {
    val action = { objectMapper.readTree(test).forEach { Person.parse(it) } }

    val averageOver1 = averageDuration(1, action)
    println("Tree average over 1 iterations: $averageOver1 ms")

    val averageOver10 = averageDuration(10, action)
    println("Tree average over 10 iterations: $averageOver10 ms")

    val averageOver100 = averageDuration(100, action)
    println("Tree average over 100 iterations: $averageOver100 ms")

    val averageOver1000 = averageDuration(1000, action)
    println("Tree average over 1000 iterations: $averageOver1000 ms")
}

private fun averageDuration(iterations: Int = 100, action: () -> Unit): Double {
    val averages = mutableListOf<Long>()

    for (i in 1..iterations) {
        val start = System.currentTimeMillis()
        action()
        val end = System.currentTimeMillis()

        averages.add(end - start)
    }

    return averages.average()
}
