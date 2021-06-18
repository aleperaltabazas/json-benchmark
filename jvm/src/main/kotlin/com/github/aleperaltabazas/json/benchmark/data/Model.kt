package com.github.aleperaltabazas.json.benchmark.data

import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.databind.JsonNode
import java.util.*

interface JsonParseable<T> {
    fun Map<String, String>.getNotNull(key: String) = get(key)
        ?: throw IllegalArgumentException("Expected key '$key' not to be null")

    fun parse(node: JsonNode): T

    fun write(t: T): String

    fun Any.escape() = "\"$this\""
}

data class PersonByReflection(
    @JsonProperty("_id") val id: String,
    val index: Int,
    val guid: UUID,
    val isActive: Boolean,
    val balance: String,
    val picture: String,
    val age: Int,
    val eyeColor: String,
    val name: String,
    val gender: String,
    val company: String,
    val email: String,
    val address: String,
    val phone: String,
    val about: String,
    val registered: String,
    val latitude: Double,
    val longitude: Double,
    val tags: List<String>,
    val friends: List<Friend>,
) {
    companion object : JsonParseable<PersonByReflection> {
        override fun parse(node: JsonNode): PersonByReflection = PersonByReflection(
            id = node.get("_id").asText(),
            index = node.get("index").asInt(),
            isActive = node.get("isActive").asBoolean(),
            balance = node.get("balance").asText(),
            picture = node.get("picture").asText(),
            age = node.get("age").asInt(),
            eyeColor = node.get("eyeColor").asText(),
            name = node.get("name").asText(),
            gender = node.get("gender").asText(),
            company = node.get("company").asText(),
            phone = node.get("phone").asText(),
            email = node.get("email").asText(),
            address = node.get("address").asText(),
            about = node.get("about").asText(),
            registered = node.get("registered").asText(),
            latitude = node.get("latitude").asDouble(),
            longitude = node.get("longitude").asDouble(),
            tags = (node.get("tags")).map { it.asText() },
            friends = node.get("friends").map { Friend.parse(it) },
            guid = UUID.fromString(node.get("guid").asText())
        )

        override fun write(person: PersonByReflection): String = with(person) {
            "{" +
                mapOf(
                    "id" to id.escape(),
                    "index" to index,
                    "isActive" to isActive,
                    "balance" to balance.escape(),
                    "picture" to picture.escape(),
                    "age" to age,
                    "eyeColor" to eyeColor.escape(),
                    "name" to name.escape(),
                    "gender" to gender.escape(),
                    "company" to company.escape(),
                    "email" to email.escape(),
                    "address" to address.escape(),
                    "latitude" to latitude,
                    "longitude" to longitude,
                    "tags" to "[${tags.joinToString(",") { it.escape() }}]",
                    "friends" to "[${friends.joinToString(",") { Friend.write(it) }}]",
                    "guid" to guid.toString(),
                    "phone" to phone,
                )
                    .map { (k, v) -> "${k.escape()}:$v" }
                    .joinToString(",") +
                "}"
        }
    }
}

data class Friend(
    val id: Int,
    val name: String,
) {
    companion object : JsonParseable<Friend> {
        override fun parse(node: JsonNode): Friend = Friend(
            id = node.get("id").asInt(),
            name = node.get("name").asText(),
        )

        override fun write(friend: Friend): String = with(friend) {
            "{" + listOf("\"id\":\"$id\"", "\"name\":\"$name\"").joinToString(",") + "}"
        }
    }
}
