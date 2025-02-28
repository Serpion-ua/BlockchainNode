package blockchain

object Config {
  case class RestServerConfig(host: String, port: Int)

  case class AppConfig(rest: RestServerConfig)
}
